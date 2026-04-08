#!/usr/bin/env python
# -*- coding: utf-8 -*-

from transformers import AutoTokenizer
import textnorm, json, codecs, unicodedata

#tokenizer_greek = AutoTokenizer.from_pretrained("nlpaueb/bert-base-greek-uncased-v1") #https://huggingface.co/nlpaueb/bert-base-greek-uncased-v1
#tokenizername = "wordpiece-greek-bert_" 

#tokenizer_greek = AutoTokenizer.from_pretrained("pranaydeeps/Ancient-Greek-BERT") #https://huggingface.co/pranaydeeps/Ancient-Greek-BERT
#tokenizername = "wordpiece-ancient-greek-bert_" 
#tokenizername = "wordpiece-ancient-greek-bert-nonorm_" 


#tokenizer_greek = AutoTokenizer.from_pretrained('bowphs/GreBerta') #https://huggingface.co/bowphs/GreBerta / BPE / RoBerta
#tokenizername = "greberta-bpe-ancient-greek-nonorm_"
#tokenizername = "greberta-bpe-ancient-greek_" 

tokenizer_greek = AutoTokenizer.from_pretrained('bowphs/GreTa') #https://huggingface.co/bowphs/GreTa / wordpiece / T5
tokenizername = "greta-wordpie-ancient-greek_" 
#tokenizername = "greta-wordpie-ancient-greek-nonorm_"

#insight to the config and basic norm and token building
print( vars( tokenizer_greek ))
teststring = "μὴ ἀποθανεῖν οὐκ ᾤετο λιπαρητέον εἶναι, ἀλλὰ καὶ καιρὸν ἤδη ἐνόμιζεν ἑαυτῷ τελευτᾶν. ὅτι δὲ οὕτως ἐγίγνωσκε καταδηλότερον ἐγένετο, ἐπειδὴ καὶ ἡ δίκη κατεψηφίσθη. πρῶτον μὲν γὰρ κελευόμενος ὑποτιμᾶσθαι οὔτε αὐτὸς ὑπετιμήσατο οὔτε τοὺς φίλους εἴασεν, ἀλλὰ καὶ ἔλεγεν ὅτι τὸ ὑποτιμᾶσθαι ὁμολογοῦντος εἴη ἀδικεῖν. ἔπειτα τῶν ἑταίρων"
#print( tokenizer_greek.backend_tokenizer.normalizer.normalize_str( teststring ) )

e = tokenizer_greek.convert_ids_to_tokens( tokenizer_greek.encode( unicodedata.normalize( "NFD", teststring ) ) )
f = tokenizer_greek.convert_ids_to_tokens( tokenizer_greek.encode( unicodedata.normalize( "NFKD", teststring ) ) )
g = tokenizer_greek.convert_ids_to_tokens( tokenizer_greek.encode( unicodedata.normalize( "NFC", teststring ) ) )
h = tokenizer_greek.convert_ids_to_tokens( tokenizer_greek.encode( unicodedata.normalize( "NFKC", teststring ) ) )
print(  "NFD", e ) 
print(   "NFKD", f  )
print(  "NFC", g ) 
print(   "NFKC", h  )

#run for input texts in the gr folder
fnames = ["tlg0059004phaedo.replicatio.v03.xml", "tlg0059008politicus.replicatio.v03.xml", "tlg0059002apologia.replicatio.v03.xml", "tlg0007096tranquillitate.replicatio.v03.xml", "tlg0007095ira.replicatio.v03.xml", "tlg0059038axiochus.replicatio.v03.xml", "tlg0032005apologia.replicatio.v03.xml", "tlg0059034leges.replicatio.v03.xml"]
print(tokenizername)
for fn in fnames:
    print( fn)
    rawtext = "";
    if( "nonorm" in tokenizername ):
        rawtext = textnorm.delgrkl( codecs.open( "gr/"+fn, encoding='utf-8').read( ) )
        print("nonorm version")
    else:
        rawtext = textnorm.deldiak( textnorm.delgrkl( textnorm.delunknown( codecs.open( "gr/"+fn, encoding='utf-8').read( ) ) ) )
        print("normed version")
    #
    input_ids = tokenizer_greek.encode( rawtext )
    tokensof = tokenizer_greek.convert_ids_to_tokens( input_ids )
    stringtok = "";
    count = 0
    for i in range( len( tokensof ) ):
        stringtok = stringtok + ", " + tokensof[ i ].replace("▁","##")
        if( count >= 100 ):
            count = 0
            stringtok = stringtok + "\n"
        count += 1
    file = codecs.open( fn+"-decomp.txt", "w", "utf-8" )
    file.write( stringtok )
    file.close()
print("End")
