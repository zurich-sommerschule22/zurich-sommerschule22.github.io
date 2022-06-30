
library(tidyverse)
library(readxl)
library(tidytext)
library(readtext)
library(sjPlot)

# PREP -----------
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



###  corpus long -----

corpus_aggr_long <- corpus_token_SA %>%
  select(collection,
         author,
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
                 "Sentiart_ang_z_mean"), names_to = "sentiment", values_to = "sentiment_value" )


### ANIMAL corpus --------

animals <- read_excel("animals.xlsx", col_types = c("skip", "text")) %>%
  mutate(is_animal = 1) %>%
  mutate(animal_item = word) %>%
  rename(token = word)

corpus_animals <- corpus_token_SA %>%
  left_join(
    animals
  ) %>%
  dplyr::group_by(collection,
                  author,
                  title,
                  doc_id,
                  sentence,
                  sentence_id) %>%
  summarise(animal_count = sum(is_animal, na.rm=T),
            animal_item = paste0(list(animal_item[!is.na(animal_item)])))

corpus_animals$animal_item[corpus_animals$animal_item == "character(0)"] <- NA

corpus_animals <- corpus_animals %>%
  filter(!is.na(animal_item))


corpus_aggr_long <- corpus_aggr_long %>%
  left_join(corpus_animals)


remove(corpus_animals)

# mean sentiment by represented gender (Sentiart)

library(table1)

corpus_aggr_long %>%
  filter(!is.na(animal_item)) %>%
  group_by(title, sentiment) %>%
  summarise(sentiment_value = mean(sentiment_value, na.rm=T),
            animal_count = sum(animal_count, na.rm=T)) %>%
  ggplot(aes(y=sentiment_value, x=animal_count, fill=sentiment, label=round(sentiment_value, 3))) +
  geom_smooth() +
  facet_wrap(. ~ sentiment) +
  ggtitle("Mean sentiment values in sentences with animal words")




# and by author

corpus_aggr_long %>%
  group_by(author_gender, sentiment) %>%
  summarise(sentiment_value = mean(sentiment_value, na.rm=T)) %>%
  ggplot(aes(y=sentiment_value, x=author_gender, fill=author_gender, label=round(sentiment_value, 3))) +
  geom_col(position="dodge") +
  geom_text(nudge_y = -.2) +
  facet_wrap(. ~ sentiment) +
  ggtitle("Mean sentiment values in sentences by author gender")



# keywords

library(quanteda)

toks <- corpus_aggr_long %>%
  select(sentence, sentence_id) %>%
  distinct()

toks <- toks$sentence

toks <- quanteda::corpus(toks)

quanteda::kwic(tokens(toks), pattern = "Hund")

quanteda::kwic(tokens(toks), pattern = "Katze")

