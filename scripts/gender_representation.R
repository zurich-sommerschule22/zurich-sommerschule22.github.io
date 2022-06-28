
library(tidyverse)
library(readxl)

# load(
#   "C:/Users/grig/scibo_swissness/test_swiss_corpus_R/all_entities_new.Rdata"
#   # "~/sciebo/test_swiss_corpus_R/all_entities_new.Rdata"
# )

# load("TS_corpus.Rdata")
# 
# corpus_TS <- corpus_TS %>%
#   filter(sentence_id >= 30 & sentence_id <= 130)
# 
# save(corpus_TS, file="TS_corpus_small.Rdata")
# 
# gc()

load("TS_corpus_small.Rdata")

corpus_meta <- corpus_meta %>%
  mutate(first_name = ifelse(first_name == "Bettina von", "Bettina", first_name))

corpus_TS <- corpus_TS %>%
  mutate(first_name = ifelse(first_name == "Bettina von", "Bettina", first_name))

corpus_meta <- corpus_meta %>%
  mutate(first_name = ifelse(first_name == "Ernst Adolf", "Adolf", first_name))

corpus_TS <- corpus_TS %>%
  mutate(first_name = ifelse(first_name == "Ernst Adolf", "Adolf", first_name))

# authors gender ---------------

german_names <- read_delim("Vornamen_2020_Koeln_edited.csv", 
                           delim = ";", escape_double = FALSE, 
                           col_types = cols(anzahl = col_skip(), 
                                            position = col_skip()
                                            ), 
                           trim_ws = TRUE) %>%
  rename(first_name = vorname) %>%
  rename(gender = geschlecht) %>%
  distinct()

doubles_st <- german_names$first_name[duplicated(german_names$first_name)]

doubles_st <- german_names %>%
  filter(first_name %in% doubles_st) %>%
  distinct()

german_names <- german_names %>%
  anti_join(doubles_st)
  
remove(doubles_st)

corpus_TS <- corpus_TS %>%
  left_join(
    german_names
  )

corpus_meta <- corpus_meta %>%
  left_join(
    german_names
  )

corpus_TS_total <- corpus_TS %>%
  select(doc_id, sentence_id, token_id) %>%
  distinct() %>%
  nrow()

female_total <- corpus_TS %>%
  select(doc_id, sentence_id, gender, token_id) %>%
  distinct() %>%
  filter(gender == "w") %>%
  nrow()

male_total <- corpus_TS %>%
  select(doc_id, sentence_id, gender, token_id) %>%
  distinct() %>%
  filter(gender == "m") %>%
  nrow()



corpus_meta %>%
  select(author, gender) %>%
  distinct() %>%
  group_by(gender) %>%
  count() %>%
  ggplot(aes(y=n, x=gender, fill=gender, label=n)) +
  geom_col() +
  geom_text(nudge_y = 2)

corpus_meta %>%
  select(author, gender, pub_date) %>%
  distinct() %>%
  group_by(gender, pub_date) %>%
  count() %>%
  ggplot(aes(y=n, x=pub_date, color=gender)) +
  geom_smooth()


german_sents <- syuzhet::get_sentiment_dictionary('nrc', language = "german") %>%
  select(-lang, -value)


corpus_TS_sentiment <- corpus_TS %>%
  left_join(german_sents, by = c("token"="word"))

remove(corpus_TS)
gc()

# overall proportion of sentiment words in corpus by gender

corpus_TS_sentiment %>%
  mutate(sentiment = as.factor(sentiment)) %>%
  mutate(sentiment_value = as.numeric(ifelse(!is.na(sentiment), 1, 0))) %>%
  group_by(gender, sentiment) %>%
  count() %>%
  group_by(sentiment, gender, n) %>%
  filter(!is.na(sentiment)) %>%
  summarise(sentiment_percent = ifelse(gender == "m", n*100/male_total, n*100/female_total),
            word_total = ifelse(gender == "m", male_total, female_total)) %>%
  ggplot(aes(x=sentiment, y=sentiment_percent, fill=gender)) +
  geom_col(position = "dodge")



# discrete emotions count per year

corpus_TS_sentiment %>%
  filter(sentiment != "positive" & sentiment != "negative") %>%
  mutate(sentiment = as.factor(sentiment)) %>%
  mutate(sentiment_value = as.numeric(ifelse(!is.na(sentiment), 1, 0))) %>%
  group_by(sentiment, pub_date, gender) %>%
  count() %>%
  group_by(sentiment, pub_date, n, gender) %>%
  summarise(sent_prop = n*100/corpus_TS_total) %>%
  ggplot(aes(x=pub_date, y=sent_prop, color=sentiment)) +
  geom_point() +
  facet_grid(. ~ gender)

corpus_TS_sentiment %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  mutate(sentiment = as.factor(sentiment)) %>%
  group_by(sentiment, pub_date) %>%
  count() %>%
  mutate(n = ifelse(sentiment == "positive", n, -n)) %>%
  group_by(sentiment, pub_date, n) %>%
  summarise(sent_prop = n*100/corpus_TS_total) %>%
  ggplot(aes(x=pub_date, y=sent_prop, color=sentiment)) +
  geom_point() +
  geom_smooth(se = F)


# positive/negative by year and gender

corpus_TS_sentiment %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  mutate(sentiment = as.factor(sentiment)) %>%
  group_by(sentiment, pub_date, gender) %>%
  count() %>%
  mutate(n = ifelse(sentiment == "positive", n, -n)) %>%
  group_by(sentiment, pub_date, gender, n) %>%
  summarise(sent_prop = n*100/corpus_TS_total) %>%
  ggplot(aes(x=pub_date, y=sent_prop, color=sentiment)) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(. ~ gender)


# represented gender -----------------

corpus_TS_aggregated <- corpus_TS_sentiment %>%
  filter(!is.na(sentiment)) %>%
  mutate(sentiment_value = 1) %>%
  # mutate(sentiment_item = ifelse(sentiment_value == 1, token, NA)) %>%

  dplyr::group_by(author,
                  title,
                  gender,
                  pub_date,
                  doc_id, 
                  sentence_id,
                  sentence,
                  sentiment) %>%
  
  summarise(sentiment_value = sum(sentiment_value, na.rm = T),
            # sentiment_item = paste0(list(sentiment_item[!is.na(sentiment_item)]))
            )






## proper names index ----------------


stop_german <- tibble(word = stopwords::stopwords("de"))

stop_german2 <- stop_german
stop_german2$word <- str_to_sentence(stop_german2$word)
stop_german <- bind_rows(stop_german, stop_german2)
remove(stop_german2)



### FEMALE corpus --------

corpus_gender_female <- corpus_TS_sentiment %>%
  select(author,
         title,
         doc_id,
         sentence,
         sentence_id,
         token,
         token_id) %>%
  distinct() %>%
  left_join(german_names %>%
              filter(gender == "w") %>%
              anti_join(stop_german, by = c("first_name" = "word")) %>%
              rename(name_gender = gender),
            by = c("token" = "first_name")) %>%
  filter(grepl("Frau", token) |
           grepl("Mutter", token) |
           # token == "sie" |
           grepl("Schwester", token) |
           grepl("Tante", token) |
           grepl("Mädchen", token) |
           grepl("Dame", token) |
           grepl("Tochter", token) |
           grepl("Lehrerin", token) |
           grepl("rerin", token) |
           grepl("Fräulein", token) |
           name_gender == "w") %>%
  mutate(is_gender_word = 1)  %>%
  mutate(gender_type = "female") %>%
  dplyr::group_by(author,
                  title,
                  doc_id,
                  sentence_id,
  ) %>%
  summarise(gender_words_n = sum(is_gender_word, na.rm=T),
            gender_item = paste0(list(token[!is.na(token)])),
            gender_type = "female",
            is_gender_word = 1)




corpus_gender_male <- corpus_TS_sentiment %>%
  select(author,
         title,
         doc_id,
         sentence,
         sentence_id,
         token,
         token_id) %>%
  distinct() %>%
  left_join(german_names %>%
              filter(gender == "m") %>%
              anti_join(stop_german, by = c("first_name" = "word")) %>%
              rename(name_gender = gender),
            by = c("token" = "first_name")) %>%
  filter(grepl("Herr", token) |
           grepl("Vater", token) |
           token == "er" |
           grepl("Bruder", token) |
           grepl("Onkel", token) |
           grepl("Ritter", token) |
           grepl("Sohn", token) |
           token == "Lehrer" |
           name_gender == "m") %>%
  mutate(is_gender_word = 1)  %>%
  mutate(gender_type = "male") %>%
  dplyr::group_by(author,
                  title,
                  doc_id,
                  sentence_id,
  ) %>%
  summarise(gender_words_n = sum(is_gender_word, na.rm=T),
            gender_item = paste0(list(token[!is.na(token)])),
            gender_type = "male",
            is_gender_word = 1)


corpus_gender <- bind_rows(corpus_gender_female, corpus_gender_male)
remove(corpus_gender_female, corpus_gender_male)

corpus_TS_aggregated <- corpus_TS_aggregated %>%
  left_join(corpus_gender)


remove(german_names, german_sents)


library(table1)

corpus_TS_aggregated %>%
  group_by(gender_type) %>%
  summarise(sentiment_value = mean(sentiment_value)) %>%
  ggplot(aes(y=sentiment_value, x=gender_type, fill=gender_type, label=round(sentiment_value, 3))) +
  geom_col(position="dodge") +
  geom_text(nudge_y = -.2)

table1::table1(~ sentiment_value | sentiment + gender_type, data=corpus_TS_aggregated)


corpus_TS_aggregated %>%
  group_by(gender_type, gender) %>%
  summarise(sentiment_value = mean(sentiment_value)) %>%
  ggplot(aes(y=sentiment_value, x=gender_type, fill=gender_type, label=round(sentiment_value, 3))) +
  geom_col(position="dodge") +
  facet_wrap(. ~ gender) +
  geom_text(nudge_y = -.2)



# keywords

library(quanteda)

toks <- corpus_TS_aggregated %>%
  select(sentence, sentence_id) %>%
  distinct()

toks <- toks$sentence

toks <- quanteda::corpus(toks)

quanteda::kwic(tokens(toks), pattern = "Frau")

quanteda::kwic(tokens(toks), pattern = "Mann")


head(tokens_ngrams(tokens(paste0(frau_kwic$pre, frau_kwic$post), remove_punct = TRUE)), 30)

