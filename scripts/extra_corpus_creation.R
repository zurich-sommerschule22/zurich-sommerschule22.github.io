

library(tidyverse)
library(readxl)
library(tidytext)



# PREP -----------
## corpus token -------
# if you erased your corpus, run this to recreate it:

corpus_token_SA <- readtext("TS_corpus_txt", encoding = "UTF-8")  %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, drop = T) %>%
  as_tibble()  %>%
  group_by(doc_id) %>%
  mutate(sentence_id = seq_along(sentence)) %>%
  slice_sample(n = 100) %>% # random 150 sentences per text
  ungroup() %>%
  mutate(sentence = str_replace_all(sentence, pattern = "ſ", replacement = "s")) %>%
  left_join(read_excel(
    # "D:/GitHub/zurich-sommerschule22.github.io/corpus/summer_school_corpus_info.xlsx"
    "summer_school_corpus_info_new.xlsx"
  ) %>%
    select(doc_id, title, author, pub_date, first_name, surname, collection)) %>%
  filter(!is.na(author)) %>%
  select(collection,
         doc_id,
         author,
         title,
         first_name,
         pub_date,
         sentence,
         sentence_id) %>%
  unnest_tokens(input = "sentence", output = "token", to_lower = F, drop = F) %>%
  group_by(title, sentence_id) %>%
  mutate(token_id = seq_along(token)) %>%
  ungroup() %>%
  mutate(unique_word_id = seq_along(token)) %>%
  left_join(read.csv("SA_resources/SentiArt.dat",
                     dec = ",",
                     encoding = "UTF-8") %>%
              rename(token = 1) %>%
              mutate(ang_z = (ang_z - mean(ang_z))/sd(ang_z)) %>%
              select(-word)
  )

# stopwords list

stop_german <- tibble(word = stopwords::stopwords("de"))
stop_german2 <- stop_german
stop_german2$word <- str_to_sentence(stop_german2$word)
stop_german <- bind_rows(stop_german, stop_german2)
remove(stop_german2)
stop_german <- stop_german %>%
  rename(token = word)

# proper names list

german_names <- read_delim("Vornamen_2020_Koeln_edited.csv", 
                           delim = ";", escape_double = FALSE, 
                           col_types = cols(anzahl = col_skip(), 
                                            position = col_skip()
                           ), 
                           trim_ws = TRUE) %>%
  rename(first_name = vorname) %>%
  rename(gender = geschlecht) %>%
  distinct()

# let's make sure there are no double names

doubles_st <- german_names$first_name[duplicated(german_names$first_name)]

doubles_st <- german_names %>%
  filter(first_name %in% doubles_st) %>%
  distinct()

german_names <- german_names %>%
  anti_join(doubles_st)

remove(doubles_st)


corpus_token_SA <- corpus_token_SA %>%
  left_join(
    german_names
  )





###  corpus long -----------

corpus_aggr_long <- corpus_token_SA %>%
  select(collection,
         author,
         gender,
         title,
         doc_id,
         pub_date,
         sentence_id,
         sentence, 
         AAPz,
         fear_z,
         disg_z,
         hap_z,
         sad_z,
         surp_z,
         ang_z)  %>%
  dplyr::group_by(collection, 
                  author,
                  gender,
                  title,
                  doc_id, 
                  sentence_id,
                  sentence,
                  pub_date) %>%
  dplyr::summarise(words_sent = n(),
                   Sentiart_AAPz_mean = ifelse(!is.nan(mean(AAPz, na.rm = T)), mean(AAPz, na.rm = T), NA),
                   Sentiart_fear_z_mean = ifelse(!is.nan(mean(fear_z, na.rm = T)), mean(fear_z, na.rm = T), NA),
                   Sentiart_disg_z_mean = ifelse(!is.nan(mean(disg_z, na.rm = T)), mean(disg_z, na.rm = T), NA),
                   Sentiart_hap_z_mean = ifelse(!is.nan(mean(hap_z, na.rm = T)), mean(hap_z, na.rm = T), NA),
                   Sentiart_sad_z_mean = ifelse(!is.nan(mean(sad_z, na.rm = T)), mean(sad_z, na.rm = T), NA),
                   Sentiart_surp_z_mean = ifelse(!is.nan(mean(surp_z, na.rm = T)), mean(surp_z, na.rm = T), NA),
                   Sentiart_ang_z_mean = ifelse(!is.nan(mean(ang_z, na.rm = T)), mean(ang_z, na.rm = T), NA),
  ) %>%
  select(-words_sent)  %>%
  pivot_longer(c("Sentiart_AAPz_mean",
                 "Sentiart_fear_z_mean",
                 "Sentiart_disg_z_mean",
                 "Sentiart_hap_z_mean",
                 "Sentiart_sad_z_mean",
                 "Sentiart_surp_z_mean",
                 "Sentiart_ang_z_mean"), names_to = "sentiment", values_to = "sentiment_value" ) %>%
  rename(author_gender = gender)






# 
# library(readxl)
# library(tidyverse)
# 
# load("C:/Users/grig/scibo_swissness/test_swiss_corpus_R/corpus_clean_with_stop.Rdata") # for WIN
# load("C:/Users/grig/scibo_swissness/test_swiss_corpus_R/ELTEC_de_clean_with_stop.Rdata") # for WIN
# # load("C:/Users/grig/scibo_swissness/test_swiss_corpus_R/all_entities_new.Rdata") # for WIN
# 
# 
# load("~/sciebo/test_swiss_corpus_R/corpus_clean_with_stop.Rdata") # for MAC
# load("~/sciebo/test_swiss_corpus_R/ELTEC_de_clean_with_stop.Rdata") # for MAC
# # load("~/sciebo/test_swiss_corpus_R/all_entities_new.Rdata") # for MAC
# 
# summer_school_corpus_info <- read_excel(
#   "D:/GitHub/zurich-sommerschule22.github.io/corpus/summer_school_corpus_info.xlsx"
#   # "~/Documents/GitHub/zurich-sommerschule22.github.io/corpus/summer_school_corpus_info.xlsx"
# )
# 
# corpus_swiss <- corpus_clean_with_stop %>%
#   select(-author, -title, -pub_date, -source) %>%
#   left_join(summer_school_corpus_info, by = "doc_id") %>%
#   filter(!is.na(first_name)) %>%
#   mutate(collection = "CH")
# 
# 
# corpus_german <- ELTEC_de_clean_with_stop %>%
#   select(-author, -title, -pub_date) %>%
#   left_join(summer_school_corpus_info, by = "doc_id") %>%
#   filter(!is.na(first_name)) %>%
#   mutate(collection = "DE")
# 
# remove(corpus_clean_with_stop, ELTEC_de_clean_with_stop)
# 
# gc()
# 
# corpus_meta <- bind_rows(
#   corpus_swiss %>%
#     select(doc_id, author, title, pub_date, first_name, collection) %>%
#     distinct(),
#   corpus_german %>%
#     select(doc_id, author, title, pub_date, first_name, collection) %>%
#     distinct()
# )
# 
# save(corpus_meta, file="TS_corpus_meta.RData")
# 
# remove(summer_school_corpus_info)
# 
# # xlsx::write.xlsx(bind_rows(corpus_german_meta, corpus_swiss_meta), file = "C:/Users/grig/Desktop/corpusswiss_info.xlsx")
# 
# 
# # bind_rows(corpus_german_meta, corpus_swiss_meta) %>%
# #   arrange(author, `publication date`) %>%
# #   select(-doc_id) %>%
# #   kableExtra::kable("pipe") %>%
# #   kableExtra::save_kable("summer_school_corpus_info.md")
# #
# # bind_rows(corpus_german_meta, corpus_swiss_meta) %>%
# #   arrange(collection, author, pub_date) %>%
# #   select(-doc_id) %>%
# #   xlsx::write.xlsx("summer_school_corpus_info.xlsx")
# 
# 
# # Baerwart - rosswiler 1918
# # luisa otto - nürnberg 1875
# # Ebner-Eschenbach, Marie von - Agave 1903
# #
# 
# # remove(summer_school_corpus_info)
# 
# corpus_TS <- bind_rows(corpus_german, corpus_swiss)
# 
# 
# 
# save(corpus_german, file = "TS_DE_corpus.Rdata")
# save(corpus_swiss, file = "TS_CH_corpus.Rdata")
# 
# # load(file = "TS_DE_corpus.Rdata")
# # load(file = "TS_CH_corpus.Rdata")
# 
# 
# 
# remove(corpus_german, corpus_swiss)
# 
# gc()
# 
# 
# corpus_TS <- corpus_TS %>%
#   group_by(doc_id, sentence_id) %>%
#   mutate(sentence_tokens_n = n())
# 
# corpus_TS <- corpus_TS %>%
#   ungroup()
# 
# gc()
# 
# save(corpus_TS, file="TS_corpus.Rdata")
# 





