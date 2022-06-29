# animal representaiton


library(readtext)
library(tidyverse)
library(readxl)

# CORPUS PREP  ------

# first, we want to create our corpus again. can you retrieve the code to do so?

# paste it here


# how are you going to identify/find animals in the corpus?
# you ll find that in the working directory there is a file that could help you


animals <- read_excel("animals.xlsx", col_types = c("skip", "text")) %>%
  mutate(is_animal = 1) %>%
  mutate(animal_item = word) %>%
  rename(token = word)

animal_sent <- corpus_token_SA %>%
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

animal_sent$animal_item[animal_sent$animal_item == "character(0)"] <- NA

animal_sent <- animal_sent %>%
  filter(!is.na(animal_item))

corpus_token_SA <- corpus_token_SA %>%
  left_join(animal_sent)


# let's add sentiments

german_sents <- syuzhet::get_sentiment_dictionary('nrc', language = "german") %>%
  select(-lang, -value)

corpus_token_SA <- corpus_token_SA %>%
  mutate(sentiment_value = ifelse(!is.na(sentiment), 1, 0))

corpus_token_SA %>%
  
  ggplot(aes(sentiment_value, animal_count)) +
  geom_smooth()
