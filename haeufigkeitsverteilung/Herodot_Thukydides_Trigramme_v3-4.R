# ==========================================================================
# Vergleich von Buchstaben-Trigrammen (skip = 1) bei Herodot und Thukydides
# Autor: Charlotte Schubert
# ==========================================================================

# 1. SETUP & BIBLIOTHEKEN --------------------------------------------------
library(tidyverse)
library(here)
library(xml2)
library(tidytext)
library(viridis)
library(stringi)

# Griechische Schrift in Plots
if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  library(sysfonts)
  library(showtext)
  font_add_google("Noto Sans", "noto")
  showtext_auto()
  greek_font <- "noto"
} else {
  greek_font <- ""
  message("Pakete sysfonts/showtext nicht installiert – ",
          "griechische Zeichen ggf. nicht korrekt dargestellt.\n",
          "Installation: install.packages(c('sysfonts', 'showtext'))")
}


# 2. NORMALISIERUNGSPIPELINE -----------------------------------------------
# Kriterien nach stylo-ah-online:
#
# Markup / Format:
#   - Without markup (HTML/XML entfernen)
#   - Without newline (Zeilenumbrüche entfernen)
#   - No hyphenation (Silbentrennung auflösen)
# Sign equalization:
#   - Disambiguate diacritica (NFKD-Zerlegung)
#   - Disambiguate dashes (Strich-Varianten vereinheitlichen)
# Text output:
#   - Tailing sigma uniform (ς → σ)
#   - Without diacritics (alle Akzente, Spiritus etc.)
#   - Without unknown signs (†, *, ⋖, #, §, ⁑)
#   - Without ligature (Ligaturen auflösen)
#   - Equal case (Kleinschreibung)
#   - No brackets (alle Klammern entfernen)
# Word level:
#   - Delete punctuation

normalise_greek <- function(x) {
  x |>
    # --- Markup / Format ---
    str_replace_all("[\\r\\n]+", " ") |>
    str_remove_all("<[^>]+>") |>
    str_replace_all("([\\p{Greek}])[-\u2010-\u2015]\\s*([\\p{Greek}])", "\\1\\2") |>

    # --- Sign equalization ---
    stri_trans_nfkd() |>
    str_replace_all("[\u2010-\u2015\u2212\uFE58\uFE63\uFF0D]", "-") |>

    # --- Text output: Diakritika ---
    str_remove_all("\\p{M}") |>
    str_remove_all("[\u0300-\u036F]") |>
    str_remove_all("[\u1FBD-\u1FFF]") |>
    str_remove_all("\\p{Sk}") |>

    # --- Unbekannte Zeichen ---
    str_remove_all("[†*⋖#§⁑]") |>

    # --- Ligaturen ---
    str_replace_all("\u03D7", "\u03BA\u03B1\u03B9") |>
    str_replace_all("\u03DB", "\u03C3\u03C4") |>
    str_replace_all("\u0223", "ou") |>

    # --- Finales Sigma ---
    str_replace_all("\u03C2", "\u03C3") |>

    # --- Klammern ---
    str_remove_all("[()\\[\\]{}<>\u27E8\u27E9\u2329\u232A]") |>

    # --- Interpunktion ---
    str_remove_all("\\p{P}") |>

    # --- Kleinschreibung ---
    str_to_lower() |>

    # --- Aufräumen ---
    str_squish()
}


# 3. HILFSFUNKTIONEN -------------------------------------------------------

import_text_tlg <- function(filepath, book_col = "l7") {
  data <- read_xml(filepath) |>
    xml_ns_strip()

  text <- xml_children(data) |>
    xml_text() |>
    str_trim()

  rename_vec <- c(author_id = "l0", work_id = "l1", work_title = "l2")
  rename_vec["book"] <- book_col

  xml_children(data) |>
    xml_attrs() |>
    bind_rows() |>
    mutate(text = text) |>
    rename(any_of(rename_vec))
}

# Skip-Trigramme auf vollem Text (inkl. Leerzeichen)
# skip = 1: Positionen i, i+2, i+4
extract_skip_trigrams <- function(text, skip = 1) {
  map(text, \(txt) {
    chars <- stri_split_boundaries(txt, type = "character")[[1]]
    n <- length(chars)
    step <- skip + 1
    max_start <- n - 2 * step
    if (max_start < 1) return(character(0))
    map_chr(seq_len(max_start), \(i) {
      str_c(chars[i], chars[i + step], chars[i + 2 * step])
    })
  })
}


# 4. DATENIMPORT & AUFBEREITUNG --------------------------------------------

autoren <- tribble(
  ~author,        ~folder,            ~book_col,
  "Herodot",      "herodot_all",      "l7",
  "Thukydides",   "Thucydides_all",   "l6"
)

all_works <- autoren |>
  pmap(\(author, folder, book_col) {
    list.files(here(folder), pattern = "\\.xml$", full.names = TRUE) |>
      map(\(f) import_text_tlg(f, book_col = book_col)) |>
      bind_rows() |>
      mutate(author = author)
  }) |>
  bind_rows()

all_books_collapsed <- all_works |>
  select(author, book, text) |>
  group_by(author, book) |>
  summarise(text = str_c(text, collapse = " "), .groups = "drop") |>
  mutate(text = normalise_greek(text))


# 5. WORT-TOKENISIERUNG ----------------------------------------------------

all_tokens <- all_books_collapsed |>
  unnest_tokens(word, text, to_lower = FALSE)

total_words <- nrow(all_tokens)

# Globale Wortzählung
word_counts <- all_tokens |>
  count(word, sort = TRUE) |>
  mutate(
    percentage     = n / total_words * 100,
    cumulative_pct = cumsum(n) / total_words * 100,
    rank           = row_number()
  )

top_50_words  <- word_counts |> slice_head(n = 50) |> pull(word)
top_200_words <- word_counts |> slice_head(n = 200)

# Pro Autor
tokens_per_author <- all_tokens |>
  count(author, word, name = "n") |>
  group_by(author) |>
  mutate(
    author_total = sum(n),
    pct          = n / author_total * 100
  ) |>
  ungroup()

# Pro Autor × Buch
tokens_per_book <- all_tokens |>
  count(author, book, word, name = "n") |>
  group_by(author, book) |>
  mutate(
    book_total = sum(n),
    pct        = n / book_total * 100
  ) |>
  ungroup()


# 6. BUCHSTABEN-TRIGRAMME (skip = 1) ---------------------------------------

trigrams_per_book <- all_books_collapsed |>
  mutate(trigrams = extract_skip_trigrams(text, skip = 1)) |>
  select(author, book, trigrams) |>
  unnest(trigrams) |>
  rename(trigram = trigrams)

total_trigrams <- nrow(trigrams_per_book)

# Tokenzahl pro Autor (Trigramme = Tokens im Sinne von stylo-ah-online)
tokens_pro_autor <- trigrams_per_book |>
  count(author, name = "token_count")

cat("Tokenzahl (= Trigramme, skip = 1) pro Autor:\n")
print(tokens_pro_autor)
cat(sprintf("Gesamt: %s\n", format(total_trigrams, big.mark = ".")))

# Globale Trigramm-Zählung
trigram_counts <- trigrams_per_book |>
  count(trigram, sort = TRUE) |>
  mutate(
    percentage     = n / total_trigrams * 100,
    cumulative_pct = cumsum(n) / total_trigrams * 100,
    rank           = row_number()
  )

top_50_trigrams  <- trigram_counts |> slice_head(n = 50)
top_200_trigrams <- trigram_counts |> slice_head(n = 200)

# Pro Autor
trigrams_per_author <- trigrams_per_book |>
  count(author, trigram, name = "n") |>
  group_by(author) |>
  mutate(
    author_total = sum(n),
    pct          = n / author_total * 100
  ) |>
  ungroup()

# Pro Autor × Buch
trigrams_book_counts <- trigrams_per_book |>
  count(author, book, trigram, name = "n") |>
  group_by(author, book) |>
  mutate(
    book_total = sum(n),
    pct        = n / book_total * 100
  ) |>
  ungroup()


# 7. VISUALISIERUNGEN ------------------------------------------------------

dir.create(here("plots"), showWarnings = FALSE)

author_colors <- c("Herodot" = "#2c3e50", "Thukydides" = "#e74c3c")

theme_greek <- function(base_size = 14) {
  theme_minimal(base_size = base_size) %+replace%
    theme(text = element_text(family = greek_font))
}


# --- WORT-BASIERTE PLOTS ---

# A. Die 50 häufigsten Wörter pro Autor (Facetten)
p_facets_words <- tokens_per_author |>
  filter(word %in% top_50_words) |>
  ggplot(aes(pct, author, fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = author_colors) +
  facet_wrap(~word, ncol = 5, scales = "free_x") +
  theme_greek(base_size = 9) +
  labs(
    x     = "Anteil (%)",
    y     = NULL,
    title = "Die 50 häufigsten Wörter: Herodot vs. Thukydides"
  )

p_facets_words

ggsave(here("plots", "hdt_thuc_top50_woerter_facets.png"),
       p_facets_words, width = 12, height = 14, dpi = 300)


# B. Kumulative Textabdeckung (Top 200 Wörter, Gesamtkorpus)
#    mit k-Wert: Rang, bei dem 50 % Textabdeckung erreicht werden

k_words_global <- word_counts |>
  filter(cumulative_pct >= 50) |>
  slice_head(n = 1)

cat(sprintf("k-Wert (Wörter, global): Rang %d ('%s') erreicht %.1f %%\n",
            k_words_global$rank, k_words_global$word,
            k_words_global$cumulative_pct))

p_cumul_words <- ggplot(top_200_words, aes(rank, cumulative_pct)) +
  geom_line(linewidth = 1.2, color = "#2c3e50") +
  geom_point(size = 1, alpha = 0.5, color = "#2c3e50") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = k_words_global$rank, linetype = "dashed",
             color = "firebrick") +
  annotate("text", x = k_words_global$rank + 5, y = 52,
           label = sprintf("k = %d", k_words_global$rank),
           hjust = 0, color = "firebrick", size = 4.5,
           family = greek_font) +
  labs(
    x     = "Rang (1–200)",
    y     = "Kumulative Abdeckung (%)",
    title = "Kumulative Textabdeckung der 200 häufigsten Wörter",
    subtitle = sprintf("k-Wert: %d Wörter decken 50 %% des Textes ab",
                       k_words_global$rank)
  ) +
  theme_greek()

p_cumul_words

ggsave(here("plots", "hdt_thuc_kumulative_woerter.png"),
       p_cumul_words, width = 8, height = 5, dpi = 300)


# B2. k-Wert pro Autor (Wörter): Vergleich
k_words_per_author <- tokens_per_author |>
  group_by(author) |>
  arrange(desc(n), .by_group = TRUE) |>
  mutate(
    rank           = row_number(),
    cumulative_pct = cumsum(n) / first(author_total) * 100
  ) |>
  filter(cumulative_pct >= 50) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(author, k_wert = rank, bei_pct = cumulative_pct)

cat("\nk-Wert (Wörter) pro Autor:\n")
print(k_words_per_author)

# Kumulative Kurven pro Autor mit k-Wert
cumul_per_author_words <- tokens_per_author |>
  group_by(author) |>
  arrange(desc(n), .by_group = TRUE) |>
  mutate(
    rank           = row_number(),
    cumulative_pct = cumsum(n) / first(author_total) * 100
  ) |>
  filter(rank <= 200) |>
  ungroup()

p_cumul_words_author <- ggplot(cumul_per_author_words,
                               aes(rank, cumulative_pct, color = author)) +
  geom_line(linewidth = 1.0) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  geom_point(data = k_words_per_author |>
               rename(rank = k_wert, cumulative_pct = bei_pct),
             size = 4, shape = 18) +
  geom_text(data = k_words_per_author,
            aes(x = k_wert + 5, y = bei_pct + 2,
                label = sprintf("k = %d", k_wert)),
            hjust = 0, size = 4, show.legend = FALSE,
            family = greek_font) +
  scale_color_manual(values = author_colors, name = "Autor") +
  labs(
    x     = "Rang (1–200)",
    y     = "Kumulative Abdeckung (%)",
    title = "Kumulative Textabdeckung: Herodot vs. Thukydides (Wörter)",
    subtitle = "k-Wert = Rang, bei dem 50 % des Textes abgedeckt sind"
  ) +
  theme_greek()

p_cumul_words_author

ggsave(here("plots", "hdt_thuc_kumulative_woerter_kval.png"),
       p_cumul_words_author, width = 8, height = 5, dpi = 300)


# C. Die 50 häufigsten Wörter (Balkendiagramm, Gesamtkorpus)
p_bar50_words <- word_counts |>
  slice_head(n = 50) |>
  mutate(word = fct_reorder(word, percentage)) |>
  ggplot(aes(percentage, word)) +
  geom_col(fill = "steelblue") +
  labs(
    x     = "Anteil am Gesamttext (%)",
    y     = NULL,
    title = "Die 50 häufigsten Wörter (Herodot + Thukydides)"
  ) +
  theme_greek() +
  theme(panel.grid.major.y = element_blank())

p_bar50_words

ggsave(here("plots", "hdt_thuc_top50_woerter_bar.png"),
       p_bar50_words, width = 8, height = 10, dpi = 300)


# D. Heatmap: Top 20 Wörter × Autor
top_20_words <- word_counts |> slice_head(n = 20) |> pull(word)

p_heat_words <- tokens_per_author |>
  filter(word %in% top_20_words) |>
  mutate(word = fct_relevel(word, top_20_words)) |>
  ggplot(aes(word, author, fill = pct)) +
  geom_tile() +
  scale_fill_viridis(option = "D", name = "Anteil (%)") +
  labs(
    x     = NULL,
    y     = NULL,
    title = "Die 20 häufigsten Wörter: Herodot vs. Thukydides"
  ) +
  theme_greek() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p_heat_words

ggsave(here("plots", "hdt_thuc_heatmap_woerter.png"),
       p_heat_words, width = 10, height = 4, dpi = 300)


# E. Zipf-Verteilung (Wörter, pro Autor)
zipf_words_author <- tokens_per_author |>
  group_by(author) |>
  arrange(desc(n), .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  ungroup()

p_zipf_words <- ggplot(zipf_words_author,
                       aes(log10(rank), log10(n), color = author)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = author_colors, name = "Autor") +
  labs(
    x     = expression(log[10](Rang)),
    y     = expression(log[10](Häufigkeit)),
    title = "Zipf-Verteilung der Wörter: Herodot vs. Thukydides"
  ) +
  theme_greek()

p_zipf_words

ggsave(here("plots", "hdt_thuc_zipf_woerter.png"),
       p_zipf_words, width = 8, height = 5, dpi = 300)

zipf_exp_words <- zipf_words_author |>
  group_by(author) |>
  summarise(zipf_exponent = coef(lm(log10(n) ~ log10(rank)))[2],
            .groups = "drop")
print(zipf_exp_words)


# --- TRIGRAMM-BASIERTE PLOTS ---

# F. Die 50 häufigsten Trigramme pro Autor (Facetten)
p_facets_tri <- trigrams_per_author |>
  filter(trigram %in% top_50_trigrams$trigram) |>
  ggplot(aes(pct, author, fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = author_colors) +
  facet_wrap(~trigram, ncol = 5, scales = "free_x") +
  theme_greek(base_size = 9) +
  labs(
    x     = "Anteil (%)",
    y     = NULL,
    title = "Die 50 häufigsten Trigramme (skip = 1): Herodot vs. Thukydides"
  )

p_facets_tri

ggsave(here("plots", "hdt_thuc_top50_trigramme_facets.png"),
       p_facets_tri, width = 12, height = 14, dpi = 300)


# G. Die 50 häufigsten Trigramme (Balkendiagramm, Gesamtkorpus)
p_bar50_tri <- top_50_trigrams |>
  mutate(trigram = fct_reorder(trigram, percentage)) |>
  ggplot(aes(percentage, trigram)) +
  geom_col(fill = "#8e44ad") +
  labs(
    x     = "Anteil an allen Trigrammen (%)",
    y     = NULL,
    title = "Die 50 häufigsten Trigramme (skip = 1)",
    subtitle = "Gesamtkorpus Herodot + Thukydides"
  ) +
  theme_greek(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

p_bar50_tri

ggsave(here("plots", "hdt_thuc_top50_trigramme_bar.png"),
       p_bar50_tri, width = 8, height = 12, dpi = 300)


# H. Kumulative Abdeckung (Top 200 Trigramme) mit k-Wert

k_tri_global <- trigram_counts |>
  filter(cumulative_pct >= 50) |>
  slice_head(n = 1)

cat(sprintf("\nk-Wert (Trigramme, global): Rang %d ('%s') erreicht %.1f %%\n",
            k_tri_global$rank, k_tri_global$trigram,
            k_tri_global$cumulative_pct))

p_cumul_tri <- ggplot(top_200_trigrams, aes(rank, cumulative_pct)) +
  geom_line(linewidth = 1.2, color = "#8e44ad") +
  geom_point(size = 1, alpha = 0.5, color = "#8e44ad") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = k_tri_global$rank, linetype = "dashed",
             color = "firebrick") +
  annotate("text", x = k_tri_global$rank + 5, y = 52,
           label = sprintf("k = %d", k_tri_global$rank),
           hjust = 0, color = "firebrick", size = 4.5,
           family = greek_font) +
  labs(
    x     = "Rang (1–200)",
    y     = "Kumulative Abdeckung (%)",
    title = "Kumulative Abdeckung der 200 häufigsten Trigramme (skip = 1)",
    subtitle = sprintf("k-Wert: %d Trigramme decken 50 %% ab",
                       k_tri_global$rank)
  ) +
  theme_greek()

p_cumul_tri

ggsave(here("plots", "hdt_thuc_kumulative_trigramme.png"),
       p_cumul_tri, width = 8, height = 5, dpi = 300)


# H2. k-Wert pro Autor (Trigramme): Vergleich
k_tri_per_author <- trigrams_per_author |>
  group_by(author) |>
  arrange(desc(n), .by_group = TRUE) |>
  mutate(
    rank           = row_number(),
    cumulative_pct = cumsum(n) / first(author_total) * 100
  ) |>
  filter(cumulative_pct >= 50) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(author, k_wert = rank, bei_pct = cumulative_pct)

cat("\nk-Wert (Trigramme) pro Autor:\n")
print(k_tri_per_author)

cumul_per_author_tri <- trigrams_per_author |>
  group_by(author) |>
  arrange(desc(n), .by_group = TRUE) |>
  mutate(
    rank           = row_number(),
    cumulative_pct = cumsum(n) / first(author_total) * 100
  ) |>
  filter(rank <= 200) |>
  ungroup()

p_cumul_tri_author <- ggplot(cumul_per_author_tri,
                             aes(rank, cumulative_pct, color = author)) +
  geom_line(linewidth = 1.0) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  geom_point(data = k_tri_per_author |>
               rename(rank = k_wert, cumulative_pct = bei_pct),
             size = 4, shape = 18) +
  geom_text(data = k_tri_per_author,
            aes(x = k_wert + 5, y = bei_pct + 2,
                label = sprintf("k = %d", k_wert)),
            hjust = 0, size = 4, show.legend = FALSE,
            family = greek_font) +
  scale_color_manual(values = author_colors, name = "Autor") +
  labs(
    x     = "Rang (1–200)",
    y     = "Kumulative Abdeckung (%)",
    title = "Kumulative Abdeckung: Herodot vs. Thukydides (Trigramme skip = 1)",
    subtitle = "k-Wert = Rang, bei dem 50 % aller Trigramme abgedeckt sind"
  ) +
  theme_greek()

p_cumul_tri_author

ggsave(here("plots", "hdt_thuc_kumulative_trigramme_kval.png"),
       p_cumul_tri_author, width = 8, height = 5, dpi = 300)


# I. Heatmap: Top 20 Trigramme × Autor
top_20_tri <- trigram_counts |> slice_head(n = 20) |> pull(trigram)

p_heat_tri <- trigrams_per_author |>
  filter(trigram %in% top_20_tri) |>
  mutate(trigram = fct_relevel(trigram, top_20_tri)) |>
  ggplot(aes(trigram, author, fill = pct)) +
  geom_tile() +
  scale_fill_viridis(option = "C", name = "Anteil (%)") +
  labs(
    x     = NULL,
    y     = NULL,
    title = "Die 20 häufigsten Trigramme (skip = 1): Herodot vs. Thukydides"
  ) +
  theme_greek() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p_heat_tri

ggsave(here("plots", "hdt_thuc_heatmap_trigramme.png"),
       p_heat_tri, width = 10, height = 4, dpi = 300)


# J. Zipf-Verteilung (Trigramme, pro Autor)
zipf_tri_author <- trigrams_per_author |>
  group_by(author) |>
  arrange(desc(n), .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  ungroup()

p_zipf_tri <- ggplot(zipf_tri_author,
                     aes(log10(rank), log10(n), color = author)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = author_colors, name = "Autor") +
  labs(
    x     = expression(log[10](Rang)),
    y     = expression(log[10](Häufigkeit)),
    title = "Zipf-Verteilung der Trigramme (skip = 1): Herodot vs. Thukydides"
  ) +
  theme_greek()

p_zipf_tri

ggsave(here("plots", "hdt_thuc_zipf_trigramme.png"),
       p_zipf_tri, width = 8, height = 5, dpi = 300)

zipf_exp_tri <- zipf_tri_author |>
  group_by(author) |>
  summarise(zipf_exponent = coef(lm(log10(n) ~ log10(rank)))[2],
            .groups = "drop")
print(zipf_exp_tri)


# K. Ausgewählte Trigramme: Verlauf über die Bücher (Liniendiagramm)
# Die 6 häufigsten Trigramme im Detail
focus_trigrams <- trigram_counts |> slice_head(n = 6) |> pull(trigram)

p_focus_tri <- trigrams_book_counts |>
  filter(trigram %in% focus_trigrams) |>
  mutate(book_nr = parse_number(book)) |>
  ggplot(aes(book_nr, pct, color = author, shape = author)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = author_colors, name = "Autor") +
  scale_shape_manual(values = c("Herodot" = 16, "Thukydides" = 17),
                     name = "Autor") +
  scale_x_continuous(breaks = 1:9) +
  facet_wrap(~trigram, scales = "free_y", ncol = 2) +
  labs(
    x     = "Buch",
    y     = "Anteil am Buchtext (%)",
    title = "Ausgewählte Trigramme: Verlauf über die Bücher"
  ) +
  theme_greek(base_size = 12) +
  theme(
    strip.text      = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

p_focus_tri

ggsave(here("plots", "hdt_thuc_trigramme_verlauf.png"),
       p_focus_tri, width = 10, height = 8, dpi = 300)


# 8. ZUSAMMENFASSUNG -------------------------------------------------------
# Hinweis: "Tokenzahl" meint hier die Anzahl der Buchstaben-Trigramme
# (skip = 1), analog zur Zählweise in stylo-ah-online.

# Wörter pro Autor
word_summary <- all_tokens |>
  count(author, name = "woerter") |>
  left_join(
    tokens_per_author |>
      group_by(author) |>
      summarise(worttypen = n(), .groups = "drop"),
    by = "author"
  ) |>
  left_join(zipf_exp_words |> rename(zipf_words = zipf_exponent),
            by = "author")

# Trigramme pro Autor (= Tokenzahl im Sinne von stylo-ah-online)
tri_summary <- trigrams_per_book |>
  count(author, name = "tokens_trigramme") |>
  left_join(
    trigrams_per_author |>
      group_by(author) |>
      summarise(trigrammtypen = n(), .groups = "drop"),
    by = "author"
  ) |>
  left_join(zipf_exp_tri |> rename(zipf_tri = zipf_exponent),
            by = "author")

corpus_summary <- left_join(tri_summary, word_summary, by = "author") |>
  left_join(k_tri_per_author |> select(author, k_trigramme = k_wert),
            by = "author") |>
  left_join(k_words_per_author |> select(author, k_woerter = k_wert),
            by = "author")

cat("\n=== Korpus-Zusammenfassung ===\n")
cat("(Tokenzahl = Trigramme skip=1, vgl. stylo-ah-online)\n")
cat("(k-Wert = Rang, bei dem 50 % kumulative Abdeckung erreicht wird)\n")
cat("(primärer k-Wert: Trigramme; sekundär: Wörter zum Vergleich)\n\n")
print(corpus_summary)

