
library(readxl)
library(tidyverse)

load("C:/Users/grig/scibo_swissness/test_swiss_corpus_R/corpus_clean_with_stop.Rdata") # for WIN
load("C:/Users/grig/scibo_swissness/test_swiss_corpus_R/ELTEC_de_clean_with_stop.Rdata") # for WIN
# load("C:/Users/grig/scibo_swissness/test_swiss_corpus_R/all_entities_new.Rdata") # for WIN


load("~/sciebo/test_swiss_corpus_R/corpus_clean_with_stop.Rdata") # for MAC
load("~/sciebo/test_swiss_corpus_R/ELTEC_de_clean_with_stop.Rdata") # for MAC
# load("~/sciebo/test_swiss_corpus_R/all_entities_new.Rdata") # for MAC

summer_school_corpus_info <- read_excel(
  "D:/GitHub/zurich-sommerschule22.github.io/corpus/summer_school_corpus_info.xlsx"
  # "~/Documents/GitHub/zurich-sommerschule22.github.io/corpus/summer_school_corpus_info.xlsx"
)

corpus_swiss <- corpus_clean_with_stop %>%
  select(-author, -title, -pub_date, -source) %>%
  left_join(summer_school_corpus_info, by = "doc_id") %>%
  filter(!is.na(first_name)) %>%
  mutate(collection = "CH")


corpus_german <- ELTEC_de_clean_with_stop %>%
  select(-author, -title, -pub_date) %>%
  left_join(summer_school_corpus_info, by = "doc_id") %>%
  filter(!is.na(first_name)) %>%
  mutate(collection = "DE")

remove(corpus_clean_with_stop, ELTEC_de_clean_with_stop)

gc()

corpus_meta <- bind_rows(
  corpus_swiss %>%
    select(doc_id, author, title, pub_date, first_name, collection) %>%
    distinct(),
  corpus_german %>%
    select(doc_id, author, title, pub_date, first_name, collection) %>%
    distinct()
)

save(corpus_meta, file="TS_corpus_meta.RData")

remove(summer_school_corpus_info)

# xlsx::write.xlsx(bind_rows(corpus_german_meta, corpus_swiss_meta), file = "C:/Users/grig/Desktop/corpusswiss_info.xlsx")


# bind_rows(corpus_german_meta, corpus_swiss_meta) %>%
#   arrange(author, `publication date`) %>%
#   select(-doc_id) %>%
#   kableExtra::kable("pipe") %>%
#   kableExtra::save_kable("summer_school_corpus_info.md")
#
# bind_rows(corpus_german_meta, corpus_swiss_meta) %>%
#   arrange(collection, author, pub_date) %>%
#   select(-doc_id) %>%
#   xlsx::write.xlsx("summer_school_corpus_info.xlsx")


# Baerwart - rosswiler 1918
# luisa otto - nürnberg 1875
# Ebner-Eschenbach, Marie von - Agave 1903
#

# remove(summer_school_corpus_info)

corpus_TS <- bind_rows(corpus_german, corpus_swiss)



save(corpus_german, file = "TS_DE_corpus.Rdata")
save(corpus_swiss, file = "TS_CH_corpus.Rdata")

# load(file = "TS_DE_corpus.Rdata")
# load(file = "TS_CH_corpus.Rdata")



remove(corpus_german, corpus_swiss)

gc()


corpus_TS <- corpus_TS %>%
  group_by(doc_id, sentence_id) %>%
  mutate(sentence_tokens_n = n())

corpus_TS <- corpus_TS %>%
  ungroup()

gc()

save(corpus_TS, file="TS_corpus.Rdata")






