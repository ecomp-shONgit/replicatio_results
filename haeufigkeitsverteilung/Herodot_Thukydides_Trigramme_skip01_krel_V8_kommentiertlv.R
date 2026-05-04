# ==========================================================================
# Vergleich von Buchstaben-Trigrammen (skip = 0 und skip = 1)
# bei Herodot und Thukydides – mit relativen k-Werten
# Autor: Charlotte Schubert
# ==========================================================================

# 1. SETUP & BIBLIOTHEKEN --------------------------------------------------
library(tidyverse)
library(here)
library(xml2)
library(tidytext)
library(viridis)
library(stringi)
library(openxlsx)
library(sysfonts)
library(showtext)
font_add_google("Noto Sans", "noto")
showtext_auto()
greek_font <- "noto"


# 2. NORMALISIERUNGSPIPELINE -----------------------------------------------
# Kriterien nach stylo-ah-online

normalise_greek <- function(x) {
  x |>
    str_replace_all("([\\p{Greek}])[-\u2010-\u2015]\\s*([\\p{Greek}])", "\\1\\2") |>
    stri_trans_nfkd() |>
    str_replace_all("[\u2010-\u2015\u2212\uFE58\uFE63\uFF0D]", " ") |>
    str_remove_all("\\p{M}") |>
    str_remove_all("[†*⋖#§⁑]") |>
    str_replace_all("\u03C2", "\u03C3") |>
    str_remove_all("[()\\[\\]{}<>\u27E8\u27E9\u2329\u232A]") |>
    str_remove_all("\\p{P}") |>
    str_to_lower() |>
    str_squish()
}


# 3. HILFSFUNKTIONEN -------------------------------------------------------

import_text_tlg <- function(filepath, book_col = "l7") {
  data <- read_xml(filepath) |> xml_ns_strip()
  text <- xml_children(data) |> xml_text() |> str_trim()
  rename_vec <- c(author_id = "l0", work_id = "l1", work_title = "l2")
  rename_vec["book"] <- book_col
  xml_children(data) |> xml_attrs() |> bind_rows() |>
    mutate(text = text) |> rename(any_of(rename_vec))
}

# Trigramme auf vollem Text (inkl. Leerzeichen)
# skip = 0: Pos. i, i+1, i+2 (kontiguos)
# skip = 1: Pos. i, i+2, i+4 (ein Zeichen übersprungen)
extract_skip_trigrams <- function(text, skip = 0) {
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
# Export Resultate in Excel
export_results <- function(results, file_name,overwrite = FALSE) {
  sheet_names <- names(results)
  wb <- createWorkbook()
  walk(sheet_names, \(sheet){
    addWorksheet(wb, sheet)
    writeData(wb, sheet = sheet, x = results [[sheet]])
  })
  saveWorkbook(wb,file_name, overwrite)
}

# k-Wert-Berechnung: absolut und relativ
compute_k_values <- function(freq_tbl, group_col = "author",
                             count_col = "n", total_col = "author_total") {
  freq_tbl |>
    group_by(across(all_of(group_col))) |>
    arrange(desc(.data[[count_col]]), .by_group = TRUE) |>
    mutate(
      rank           = row_number(),
      cumulative_pct = cumsum(.data[[count_col]]) /
        first(.data[[total_col]]) * 100,
      n_types        = n(),
      n_tokens       = first(.data[[total_col]])
    ) |>
    filter(cumulative_pct >= 50) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(
      k_rel_types  = rank / n_types * 100,    # in %
      k_rel_tokens = rank / n_tokens * 1000   # in ‰
    ) |>
    select(all_of(group_col), k_abs = rank, bei_pct = cumulative_pct,
           n_types, n_tokens, k_rel_types, k_rel_tokens)
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
      bind_rows() |> mutate(author = author)
  }) |> bind_rows()

all_books_collapsed <- all_works |>
  select(author, book, text) |>
  group_by(author, book) |>
  summarise(text = str_c(text, collapse = " "), .groups = "drop") |>
  mutate(text = normalise_greek(text))


# 5. WORT-TOKENISIERUNG ----------------------------------------------------

all_tokens <- all_books_collapsed |>
  unnest_tokens(word, text, to_lower = FALSE)

total_words <- nrow(all_tokens)

word_counts <- all_tokens |>
  count(word, sort = TRUE) |>
  mutate(percentage = n / total_words * 100,
         cumulative_pct = cumsum(n) / total_words * 100,
         rank = row_number())

top_50_words  <- word_counts |> slice_head(n = 50)  
top_200_words <- word_counts |> slice_head(n = 200)

tokens_per_author <- all_tokens |>
  count(author, word, name = "n") |>
  group_by(author) |>
  mutate(author_total = sum(n), pct = n / author_total * 100) |>
  ungroup()

tokens_per_book <- all_tokens |>
  count(author, book, word, name = "n") |>
  group_by(author, book) |>
  mutate(book_total = sum(n), pct = n / book_total * 100) |>
  ungroup()


# 6. TRIGRAMME: SKIP = 0 UND SKIP = 1 -------------------------------------

# Beide Varianten in einer Schleife berechnen
skip_values <- c(0, 1)

trigram_data <- map(skip_values, \(s) {
  label <- sprintf("skip_%d", s)

  per_book <- all_books_collapsed |>
    mutate(trigrams = extract_skip_trigrams(text, skip = s)) |>
    select(author, book, trigrams) |>
    unnest(trigrams) |>
    rename(trigram = trigrams)

  total <- nrow(per_book)

  tokens_autor <- per_book |> count(author, name = "token_count")

  counts <- per_book |>
    count(trigram, sort = TRUE) |>
    mutate(percentage = n / total * 100,
           cumulative_pct = cumsum(n) / total * 100,
           rank = row_number())

  per_author <- per_book |>
    count(author, trigram, name = "n") |>
    group_by(author) |>
    mutate(author_total = sum(n), pct = n / author_total * 100) |>
    ungroup()

  per_book_counts <- per_book |>
    count(author, book, trigram, name = "n") |>
    group_by(author, book) |>
    mutate(book_total = sum(n), pct = n / book_total * 100) |>
    ungroup()

  k_per_author <- compute_k_values(per_author)

  list(
    skip          = s,
    label         = label,
    per_book      = per_book,
    total         = total,
    tokens_autor  = tokens_autor,
    counts        = counts,
    per_author    = per_author,
    book_counts   = per_book_counts,
    k_per_author  = k_per_author,
    top_50        = counts |> slice_head(n = 50),
    top_200       = counts |> slice_head(n = 200)
  )
}) |>
  set_names(sprintf("skip_%d", skip_values))

# Abkürzungen für direkten Zugriff
s0 <- trigram_data$skip_0
s1 <- trigram_data$skip_1
export_results(s0, "Trigramresults_skip0.xlsx", overwrite = T)
export_results(s1, "Trigramresults_skip1.xlsx", overwrite = T)


# 6b. EXPORT: Trigramm-Token-Liste (skip = 0), getrennt nach Autor --------
# Frequenzliste pro Autor: Trigramm | Häufigkeit | Anteil (%) | Rang
dir.create(here("export"), showWarnings = FALSE)

s0$per_author |>
  group_by(author) |>
  arrange(desc(n), .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  select(author, trigram, n, pct, rank) |>
  group_walk(\(data, key) {
    filename <- sprintf("trigramme_skip0_%s.csv",
                        unique(str_to_lower(key$author)))
    write_csv(data, here("export", filename))
    cat(sprintf("Exportiert: %s (%d Types, %d Tokens)\n",
                filename, nrow(data), sum(data$n)))
  })


# 7. WORT-K-WERTE ---------------------------------------------------------

k_words_global <- word_counts |>
  filter(cumulative_pct >= 50) |> slice_head(n = 1)

k_words_per_author <- compute_k_values(tokens_per_author)


# 8. VISUALISIERUNGEN ------------------------------------------------------
#
# Die folgenden Visualisierungen dienen dazu, die Häufigkeitsverteilungen
# auf Wort- und Trigramm-Ebene sowohl deskriptiv darzustellen als auch
# die stilistischen Unterschiede zwischen Herodot und Thukydides
# quantitativ sichtbar zu machen.

# Ausgabeordner für alle Visualisierungen
output_dir <- here("Output_V8")
dir.create(output_dir, showWarnings = FALSE)

author_colors <- c("Herodot" = "#2c3e50", "Thukydides" = "#e74c3c")
skip_colors   <- c("skip = 0" = "#8e44ad", "skip = 1" = "#e67e22")

theme_greek <- function(base_size = 14) {
  theme_minimal(base_size = base_size) %+replace%
    theme(text = element_text(family = greek_font))
}


# --- A. WORT-PLOTS -------------------------------------------------------

# A1. Top 50 Wörter pro Autor (Facetten)
# ZWECK: Zeigt für jedes der 50 häufigsten Wörter im Gesamtkorpus den
#   relativen Anteil (%) bei jedem Autor nebeneinander. So wird auf einen
#   Blick sichtbar, welche hochfrequenten Wörter bei welchem Autor
#   stärker oder schwächer vertreten sind (z.B. δέ bei Herodot, καί
#   bei Thukydides).
# METHODE: facet_wrap() erzeugt ein Panelgitter mit einem Subplot pro Wort.
#   semi_join() statt filter() stellt sicher, dass nur Wörter gezeigt
#   werden, die tatsächlich in der Top-50-Liste des Gesamtkorpus stehen.
#   scales = "free_x" erlaubt unterschiedliche x-Achsen-Skalen pro Panel,
#   da die Anteile zwischen και (~5 %) und selteneren Wörtern (~0.1 %)
#   stark variieren.
p_facets_words <- tokens_per_author |>
  semi_join(top_50_words, by = "word") |> 
  ggplot(aes(pct, author, fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = author_colors) +
  facet_wrap(~word, ncol = 5, scales = "free_x") +
  theme_greek(base_size = 9) +
  labs(x = "Anteil (%)", y = NULL,
       title = "Die 50 häufigsten Wörter: Herodot vs. Thukydides")

p_facets_words
ggsave(here("Output_V8", "hdt_thuc_top50_woerter_facets.png"),
       p_facets_words, width = 12, height = 14, dpi = 300)


# A2. Kumulative Abdeckung (Wörter) mit k-Wert pro Autor
# ZWECK: Zeigt, wie schnell die häufigsten Wörter den Gesamttext abdecken.
#   Die x-Achse ist der Rang (1 = häufigstes Wort, 2 = zweithäufigstes usw.),
#   die y-Achse die kumulative Abdeckung in Prozent.
#   Der k-Punkt (50 %-Schwelle) markiert, wie viele verschiedene Wörter
#   nötig sind, um die Hälfte des Textes abzudecken.
# METHODE: geom_line() zeichnet die kumulative Kurve pro Autor.
#   geom_hline(yintercept = 50) markiert die 50 %-Schwelle als horizontale
#   Referenzlinie; geom_vline() markiert den Rang, an dem diese Schwelle
#   erreicht wird (den k-Punkt). Die Facettierung nach Autor mit
#   scales = "free_x" erlaubt es, die k-Punkte direkt abzulesen, auch
#   wenn sie bei unterschiedlichen Rängen liegen.
# INTERPRETATION: Ein niedrigerer k-Punkt bedeutet, dass weniger Wörter
#   nötig sind, um 50 % des Textes abzudecken – das deutet auf einen
#   lexikalisch konzentrierteren Stil hin.
cumul_per_author_words <- tokens_per_author |>
  group_by(author) |> arrange(desc(n), .by_group = TRUE) |>
  mutate(rank = row_number(),
         cumulative_pct = cumsum(n) / first(author_total) * 100) |>
  filter(cumulative_pct < 52) |> ungroup()

p_cumul_words <- ggplot(cumul_per_author_words, aes(rank, cumulative_pct, colour = author)) +
  geom_line(linewidth = 1.0)+
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") + 
  geom_vline(aes(xintercept = rank),data = cumul_per_author_words |> 
               filter(cumulative_pct/50 >1) |> 
               group_by(author) |> 
               slice_min(rank, n=1),linetype = "dashed")+
  geom_label(
    aes(x = k_abs+2, y = bei_pct+2,label = k_abs, color = author),
    data = k_words_per_author, show.legend = FALSE)+
  facet_wrap(~ author, nrow = 1, scales = "free_x") +
  scale_color_manual(values = author_colors, name = "Autor") +
  labs(x = "Anzahl der Wörter nach Rang", y = "Kumulative Abdeckung (%)",
       title = "Kumulative Textabdeckung (Wörter) mit k-Wert") +
  theme_greek()

p_cumul_words
ggsave(here("Output_V8", "hdt_thuc_kumulative_woerter_kval.png"),
       p_cumul_words, width = 8, height = 5, dpi = 300)


# A3. Top 50 Wörter (Balken)
# ZWECK: Klassisches Rangfolge-Diagramm der 50 häufigsten Wörter im
#   Gesamtkorpus (beide Autoren zusammen). Zeigt die Zipf-artige Verteilung
#   visuell: wenige Wörter (και, δε, τε) dominieren, der Rest fällt steil ab.
# METHODE: fct_reorder() sortiert die y-Achse nach Häufigkeit, so dass
#   das häufigste Wort oben steht. Ein horizontales Balkendiagramm ist
#   hier gewählt, weil griechische Wörter als y-Achsen-Labels besser
#   lesbar sind als bei vertikaler Anordnung.
p_bar50_words <- word_counts |> slice_head(n = 50) |>
  mutate(word = fct_reorder(word, percentage)) |>
  ggplot(aes(percentage, word)) +
  geom_col(fill = "steelblue") +
  labs(x = "Anteil am Gesamttext (%)", y = NULL,
       title = "Die 50 häufigsten Wörter (Herodot + Thukydides)") +
  theme_greek() + theme(panel.grid.major.y = element_blank())

p_bar50_words
ggsave(here("Output_V8", "hdt_thuc_top50_woerter_bar.png"),
       p_bar50_words, width = 8, height = 10, dpi = 300)


# A4. Heatmap Top 20 Wörter × Autor
# ZWECK: Vergleicht die relativen Anteile der 20 häufigsten Wörter bei
#   beiden Autoren als Farbmatrix. Unterschiede werden als Farbkontraste
#   sichtbar: Ein Wort, das bei Herodot dunkel und bei Thukydides hell ist,
#   wird von Herodot überproportional häufig verwendet.
# METHODE: geom_tile() erzeugt eine Kachelmatrix, scale_fill_viridis()
#   verwendet eine wahrnehmungslineare Farbskala (Viridis), die auch für
#   Farbenblinde unterscheidbar ist. geom_text() schreibt die Prozentwerte
#   direkt in die Kacheln, damit die exakten Werte ablesbar sind.
top_20_words <- word_counts |> slice_head(n = 20) |> pull(word)

p_heat_words <- tokens_per_author |>
  filter(word %in% top_20_words) |>
  mutate(word = fct_relevel(word, top_20_words)) |>
  ggplot(aes(author,word, fill = pct,label = round (pct,digits = 3))) + geom_tile(col = "white") +
  geom_text (col = "white", size = 2)+
  scale_fill_viridis(option = "D", name = "Anteil (%)") +
  labs(x = NULL, y = NULL,
       title = "Die 20 häufigsten Wörter: Herodot vs. Thukydides") +
  theme_greek() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p_heat_words
ggsave(here("Output_V8", "hdt_thuc_heatmap_woerter.png"),
       p_heat_words, width = 10, height = 4, dpi = 300)


# A5. Zipf (Wörter)
# ZWECK: Überprüft, ob die Wortverteilung dem Zipf'schen Gesetz folgt.
#   Das Zipf-Gesetz besagt, dass die Häufigkeit eines Wortes umgekehrt
#   proportional zu seinem Rang ist: f(r) ∝ 1/r^α, wobei α ≈ 1 für
#   natürliche Sprache gilt. In der Log-Log-Darstellung (log10(Rang) vs.
#   log10(Häufigkeit)) wird aus dieser Potenzbeziehung eine Gerade mit
#   Steigung −α.
# METHODE: geom_point() zeigt alle Wort-Rang-Paare als Punktwolke
#   (alpha = 0.15 für Transparenz, da sich viele Punkte überlappen).
#   geom_smooth(method = "lm") berechnet die lineare Regression im
#   Log-Log-Raum und zeigt die empirische Zipf-Gerade pro Autor –
#   ihre Steigung ist der Zipf-Exponent.
#   geom_abline(intercept = 4, slope = -1) zeichnet die theoretische
#   Idealgerade mit Steigung −1 (perfektes Zipf-Gesetz) als Referenz.
#   Der Achsenabschnitt 4 ist ein Näherungswert (log10 der Häufigkeit
#   des häufigsten Wortes) und dient nur der visuellen Orientierung.
# INTERPRETATION: Wenn die empirischen Regressionsgeraden nahe an der
#   gestrichelten Ideallinie liegen, folgt die Verteilung Zipf.
#   Abweichungen im Kopf (häufigste Wörter) oder Tail (seltene Wörter)
#   sind normal und werden durch das Zipf-Mandelbrot-Gesetz modelliert.
#   Die Steigung (Zipf-Exponent) quantifiziert den Grad der Konzentration:
#   ein steilerer Wert (näher an −1) bedeutet eine stärkere Dominanz
#   weniger Wörter.
zipf_words_author <- tokens_per_author |>
  group_by(author) |> arrange(desc(n), .by_group = TRUE) |>
  mutate(rank = row_number()) |> ungroup()

p_zipf_words <- ggplot(zipf_words_author,
                       aes(log10(rank), log10(n), color = author)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(intercept = 4, slope = -1, linetype = "dashed")+
  scale_color_manual(values = author_colors, name = "Autor") +
  labs(x = expression(log[10](Rang)), y = expression(log[10](Häufigkeit)),
       title = "Zipf-Verteilung der Wörter") +
  theme_greek()

p_zipf_words
ggsave(here("Output_V8", "hdt_thuc_zipf_woerter.png"),
       p_zipf_words, width = 8, height = 5, dpi = 300)

zipf_exp_words <- zipf_words_author |>
  group_by(author) |>
  summarise(zipf_exponent = coef(lm(log10(n) ~ log10(rank)))[2],
            .groups = "drop")
print(zipf_exp_words)


# --- B. TRIGRAMM-PLOTS (je skip-Variante) --------------------------------
# Die folgenden Plots werden für jede skip-Variante (0 und 1) separat
# erzeugt. Die Logik ist analog zu den Wort-Plots (A1–A5), operiert
# aber auf Buchstaben-Trigrammen statt auf Wörtern. Da Trigramme
# morphologische und syntaktische Muster auf Zeichenebene erfassen,
# sind sie robuster gegenüber Flexionsvariation als Einzelwörter.

for (sd in trigram_data) {
  s      <- sd$skip
  s_lab  <- sprintf("skip = %d", s)
  s_tag  <- sprintf("skip%d", s)
  tri_col <- if (s == 0) "#8e44ad" else "#e67e22"

  # B1. Top 50 Trigramme pro Autor (Facetten)
  # ZWECK: Analog zu A1, aber für Trigramme. Zeigt, welche Zeichenmuster
  #   bei welchem Autor dominieren. Trigramme mit Leerzeichen (z.B. "αι ",
  #   " κα") erfassen Wortgrenzen-Übergänge und damit syntaktische Kontexte.
  p <- sd$per_author |>
    filter(trigram %in% sd$top_50$trigram) |>
    ggplot(aes(pct, author, fill = author)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = author_colors) +
    facet_wrap(~trigram, ncol = 5, scales = "free_x") +
    theme_greek(base_size = 9) +
    labs(x = "Anteil (%)", y = NULL,
         title = sprintf("Top 50 Trigramme (%s): Herodot vs. Thukydides", s_lab))

  print(p)
  ggsave(here("Output_V8", sprintf("hdt_thuc_top50_tri_%s_facets.png", s_tag)),
         p, width = 12, height = 14, dpi = 300)

  # B2. Top 50 Trigramme (Balken)
  # ZWECK: Analog zu A3. Zeigt die Rangfolge der häufigsten Trigramme
  #   im Gesamtkorpus. Die Farbe unterscheidet skip = 0 (lila) von
  #   skip = 1 (orange), damit die Plots visuell zugeordnet werden können.
  p <- sd$top_50 |>
    mutate(trigram = fct_reorder(trigram, percentage)) |>
    ggplot(aes(percentage, trigram)) +
    geom_col(fill = tri_col) +
    labs(x = "Anteil an allen Trigrammen (%)", y = NULL,
         title = sprintf("Top 50 Trigramme (%s)", s_lab)) +
    theme_greek(base_size = 12) +
    theme(panel.grid.major.y = element_blank())

  print(p)
  ggsave(here("Output_V8", sprintf("hdt_thuc_top50_tri_%s_bar.png", s_tag)),
         p, width = 8, height = 12, dpi = 300)

  # B3. Kumulative Abdeckung mit k-Wert pro Autor
  # ZWECK: Analog zu A2, aber für Trigramme. Der k-Punkt auf Trigramm-Ebene
  #   ist der zentrale Messwert dieser Analyse: Er zeigt, wie viele
  #   verschiedene Zeichenmuster nötig sind, um 50 % aller Trigramm-Tokens
  #   abzudecken. Ein niedrigerer k-Punkt deutet auf höhere formale
  #   Redundanz (= wenige Muster dominieren den Text).
  # METHODE: geom_point() mit shape = 18 (Raute) markiert den k-Punkt,
  #   geom_text() beschriftet ihn mit dem absoluten Rang.
  cumul <- sd$per_author |>
    group_by(author) |> arrange(desc(n), .by_group = TRUE) |>
    mutate(rank = row_number(),
           cumulative_pct = cumsum(n) / first(author_total) * 100) |>
    filter(rank <= 200) |> ungroup()

  p <- ggplot(cumul, aes(rank, cumulative_pct, color = author)) +
    geom_line(linewidth = 1.0) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
    geom_point(data = sd$k_per_author |>
                 rename(rank = k_abs, cumulative_pct = bei_pct),
               size = 4, shape = 18) +
    geom_text(data = sd$k_per_author,
              aes(x = k_abs + 5, y = bei_pct + 2,
                  label = sprintf("k = %d", k_abs)),
              hjust = 0, size = 4, show.legend = FALSE, family = greek_font) +
    scale_color_manual(values = author_colors, name = "Autor") +
    labs(x = "Rang (1–200)", y = "Kumulative Abdeckung (%)",
         title = sprintf("Kumulative Abdeckung Trigramme (%s) mit k-Wert", s_lab)) +
    theme_greek()

  print(p)
  ggsave(here("Output_V8", sprintf("hdt_thuc_kumul_tri_%s_kval.png", s_tag)),
         p, width = 8, height = 5, dpi = 300)

  # B4. Heatmap Top 20 Trigramme × Autor
  # ZWECK: Analog zu A4. Zeigt Farbunterschiede zwischen den Autoren
  #   für die häufigsten Trigramme. Trigramme, die bei einem Autor
  #   deutlich häufiger sind, deuten auf autorenspezifische morphologische
  #   oder syntaktische Muster hin (z.B. ionische vs. attische Endungen).
  top_20 <- sd$counts |> slice_head(n = 20) |> pull(trigram)

  p <- sd$per_author |>
    filter(trigram %in% top_20) |>
    mutate(trigram = fct_relevel(trigram, top_20)) |>
    ggplot(aes(trigram, author, fill = pct)) + geom_tile() +
    scale_fill_viridis(option = "C", name = "Anteil (%)") +
    labs(x = NULL, y = NULL,
         title = sprintf("Top 20 Trigramme (%s): Herodot vs. Thukydides", s_lab)) +
    theme_greek() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  print(p)
  ggsave(here("Output_V8", sprintf("hdt_thuc_heatmap_tri_%s.png", s_tag)),
         p, width = 10, height = 4, dpi = 300)

  # B5. Zipf (Trigramme)
  # ZWECK: Analog zu A5, aber für Trigramme. Die Zipf-Verteilung von
  #   Trigrammen fällt typischerweise steiler ab als die von Wörtern
  #   (Exponent ≈ −1.8 statt ≈ −0.9), weil das Trigramm-Vokabular
  #   kompakter ist: es gibt weniger verschiedene Trigramme als Wörter,
  #   aber die häufigsten Trigramme (Artikel-Endungen, Partikel-Übergänge)
  #   dominieren noch stärker.
  # METHODE: geom_smooth(method = "lm") berechnet den empirischen
  #   Zipf-Exponenten als Steigung der Regressionsgeraden im Log-Log-Raum.
  zipf <- sd$per_author |>
    group_by(author) |> arrange(desc(n), .by_group = TRUE) |>
    mutate(rank = row_number()) |> ungroup()

  p <- ggplot(zipf, aes(log10(rank), log10(n), color = author)) +
    geom_point(alpha = 0.15, size = 0.6) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    scale_color_manual(values = author_colors, name = "Autor") +
    labs(x = expression(log[10](Rang)), y = expression(log[10](Häufigkeit)),
         title = sprintf("Zipf-Verteilung Trigramme (%s)", s_lab)) +
    theme_greek()

  print(p)
  ggsave(here("Output_V8", sprintf("hdt_thuc_zipf_tri_%s.png", s_tag)),
         p, width = 8, height = 5, dpi = 300)

  # B6. Verlauf über Bücher (Top 6 Trigramme)
  # ZWECK: Zeigt die Stabilität der häufigsten Trigramme über die einzelnen
  #   Bücher hinweg. Wenn ein Trigramm bei beiden Autoren über alle Bücher
  #   hinweg einen ähnlichen Anteil hat, ist es gattungstypisch; wenn es
  #   bei einem Autor deutlich höher liegt, ist es autorenspezifisch.
  # METHODE: geom_line() + geom_point() erzeugen ein Liniendiagramm mit
  #   Buchnummer auf der x-Achse. facet_wrap() trennt nach Trigramm,
  #   scales = "free_y" erlaubt unterschiedliche y-Achsen, da die Anteile
  #   zwischen den Trigrammen stark variieren. shape = 16/17 (Kreis/Dreieck)
  #   unterscheidet die Autoren auch ohne Farbe (für S/W-Druck).
  focus <- sd$counts |> slice_head(n = 6) |> pull(trigram)

  p <- sd$book_counts |>
    filter(trigram %in% focus) |>
    mutate(book_nr = parse_number(book)) |>
    ggplot(aes(book_nr, pct, color = author, shape = author)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2.5) +
    scale_color_manual(values = author_colors, name = "Autor") +
    scale_shape_manual(values = c("Herodot" = 16, "Thukydides" = 17),
                       name = "Autor") +
    scale_x_continuous(breaks = 1:9) +
    facet_wrap(~trigram, scales = "free_y", ncol = 2) +
    labs(x = "Buch", y = "Anteil am Buchtext (%)",
         title = sprintf("Ausgewählte Trigramme (%s): Verlauf über die Bücher",
                         s_lab)) +
    theme_greek(base_size = 12) +
    theme(strip.text = element_text(size = 12, face = "bold"),
          legend.position = "bottom")

  print(p)
  ggsave(here("Output_V8", sprintf("hdt_thuc_tri_%s_verlauf.png", s_tag)),
         p, width = 10, height = 8, dpi = 300)
}


# --- C. VERGLEICHSPLOTS: SKIP 0 vs. SKIP 1 ------------------------------
# Diese Plots stellen die Ergebnisse der beiden skip-Varianten und der
# Wortebene einander gegenüber, um zu prüfen, ob die Autorenunterschiede
# über verschiedene Tokenisierungsparameter hinweg stabil sind.

# C1. Kumulative Abdeckung: skip 0 vs. skip 1 (pro Autor)
# ZWECK: Überlagert die kumulativen Kurven beider skip-Varianten in einem
#   Plot. So wird sichtbar, dass skip = 1 eine flachere Kurve erzeugt
#   (weil das Trigramm-Vokabular größer ist) und den k-Punkt nach rechts
#   verschiebt. Die Autorendifferenz (Herodot höher als Thukydides) bleibt
#   aber bei beiden skip-Varianten erhalten.
# METHODE: linetype unterscheidet skip = 0 (durchgezogen) von skip = 1
#   (gestrichelt), color unterscheidet die Autoren.
cumul_both <- map2(trigram_data, names(trigram_data), \(sd, nm) {
  sd$per_author |>
    group_by(author) |> arrange(desc(n), .by_group = TRUE) |>
    mutate(rank = row_number(),
           cumulative_pct = cumsum(n) / first(author_total) * 100) |>
    filter(rank <= 200) |> ungroup() |>
    mutate(skip = sprintf("skip = %d", sd$skip))
}) |> bind_rows()

p_cumul_compare <- ggplot(cumul_both,
                          aes(rank, cumulative_pct, color = author,
                              linetype = skip)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = author_colors, name = "Autor") +
  scale_linetype_manual(values = c("skip = 0" = "solid", "skip = 1" = "dashed"),
                        name = "Trigramm-Typ") +
  labs(x = "Rang (1–200)", y = "Kumulative Abdeckung (%)",
       title = "Kumulative Abdeckung: skip = 0 vs. skip = 1") +
  theme_greek()

p_cumul_compare
ggsave(here("Output_V8", "hdt_thuc_kumul_tri_skip_vergleich.png"),
       p_cumul_compare, width = 9, height = 5, dpi = 300)


# C2. Zipf-Exponenten: alle Ebenen im Vergleich
# ZWECK: Stellt die Zipf-Exponenten aller drei Analyseebenen (Wörter,
#   Trigramme skip = 0, Trigramme skip = 1) als Balkendiagramm dar.
#   Die gestrichelte Linie bei −1 markiert den theoretischen Idealwert
#   (perfektes Zipf-Gesetz). Wörter liegen nahe −1, Trigramme weichen
#   stärker ab (≈ −1.8), weil ihre Verteilung steiler ist.
# INTERPRETATION: Wenn beide Autoren auf derselben Ebene denselben
#   Exponenten haben, ist die Verteilungsstruktur gattungsbedingt gleich.
#   Unterschiede im Exponenten deuten auf autorenspezifische Variation
#   in der Häufigkeitskonzentration hin.
zipf_all <- map(trigram_data, \(sd) {
  sd$per_author |>
    group_by(author) |> arrange(desc(n), .by_group = TRUE) |>
    mutate(rank = row_number()) |> ungroup() |>
    group_by(author) |>
    summarise(zipf_exponent = coef(lm(log10(n) ~ log10(rank)))[2],
              .groups = "drop") |>
    mutate(ebene = sprintf("Trigramme (skip = %d)", sd$skip))
}) |> bind_rows() |>
  bind_rows(
    zipf_exp_words |> mutate(ebene = "Wörter")
  )

p_zipf_compare <- zipf_all |>
  mutate(label = str_c(author, "\n", ebene)) |>
  ggplot(aes(zipf_exponent, label, fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = author_colors) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "grey40") +
  labs(x = "Zipf-Exponent", y = NULL,
       title = "Zipf-Exponenten im Vergleich",
       subtitle = "Gestrichelte Linie: idealer Wert (−1)") +
  theme_greek()

p_zipf_compare
ggsave(here("Output_V8", "hdt_thuc_zipf_alle_ebenen.png"),
       p_zipf_compare, width = 8, height = 5, dpi = 300)


# C3. k-Werte: alle Ebenen im Vergleich (Balken)
# ZWECK: Fasst die k-Werte aller drei Ebenen in einem Balkendiagramm
#   zusammen. Dies ist der zentrale Ergebnisplot der Analyse: Er zeigt
#   auf einen Blick, dass Thukydides auf allen Ebenen einen niedrigeren
#   k-Wert hat als Herodot – seine Sprache ist stärker auf wenige
#   hochfrequente Muster konzentriert.
# METHODE: fct_reorder() sortiert die Balken nach k-Wert, geom_text()
#   schreibt den exakten Wert neben jeden Balken. Die Farbcodierung nach
#   Autor macht den Vergleich unmittelbar lesbar.
k_all <- map(trigram_data, \(sd) {
  sd$k_per_author |>
    mutate(ebene = sprintf("Trigramme (skip = %d)", sd$skip))
}) |> bind_rows() |>
  bind_rows(
    k_words_per_author |> mutate(ebene = "Wörter")
  )

p_k_compare <- k_all |>
  mutate(label = str_c(author, "\n", ebene)) |>
  ggplot(aes(k_abs, fct_reorder(label, k_abs), fill = author)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = k_abs), hjust = -0.2, size = 3.5,
            family = greek_font) +
  scale_fill_manual(values = author_colors) +
  labs(x = "k-Wert (absolut)", y = NULL,
       title = "k-Werte im Vergleich: Wörter, Trigramme skip = 0, skip = 1") +
  theme_greek() +
  theme(panel.grid.major.y = element_blank())

p_k_compare
ggsave(here("Output_V8", "hdt_thuc_kval_alle_ebenen.png"),
       p_k_compare, width = 9, height = 5, dpi = 300)


# 9. ZUSAMMENFASSUNG -------------------------------------------------------

cat("\n=== Tokenzahlen ===\n")
cat("Wörter:\n")
print(all_tokens |> count(author, name = "woerter"))
cat("\nTrigramme skip = 0:\n")
print(s0$tokens_autor)
cat("\nTrigramme skip = 1:\n")
print(s1$tokens_autor)

cat("\n=== Zipf-Exponenten ===\n")
print(zipf_all)

cat("\n=== k-Werte (absolut und relativ) ===\n")
cat("  k_abs         = absoluter Rang bei 50 % Abdeckung\n")
cat("  k_rel_types   = k / Types (in %)\n")
cat("  k_rel_tokens  = k / Tokens (in \u2030)\n\n")
k_all_detail <- k_all |>
  select(author, ebene, k_abs, k_rel_types, k_rel_tokens)
print(k_all_detail)
