# animal representaiton
# 
# load("TS_corpus.Rdata")
# 
# corpus_TS <- corpus_TS %>%
#   filter(sentence_id >= 30 & sentence_id <= 130) 
# 
# save(corpus_TS, file="TS_corpus_small.Rdata")
# 
# gc()


load("TS_corpus_small.Rdata")


library(readxl)

animals <- read_excel("animals.xlsx", col_types = c("skip", "text")) %>%
  mutate(is_animal = 1) %>%
  mutate(animal_item = word) %>%
  rename(token = word)

animal_sent <- corpus_TS %>%
  left_join(
    animals
  ) %>%
  dplyr::group_by(author,
                  title,
                  doc_id,
                  sentence,
                  sentence_id) %>%
  summarise(animal_count = sum(is_animal, na.rm=T),
            animal_item = paste0(list(animal_item[!is.na(animal_item)])))

animal_sent$animal_item[animal_sent$animal_item == "character(0)"] <- NA

animal_sent <- animal_sent %>%
  filter(!is.na(animal_item))

corpus_TS <- corpus_TS %>%
  left_join(animal_sent)


# let's add sentiments

german_sents <- syuzhet::get_sentiment_dictionary('nrc', language = "german") %>%
  select(-lang, -value)


corpus_TS_sentiment <- corpus_TS %>%
  left_join(german_sents, by = c("token"="word"))

remove(corpus_TS)

corpus_TS_sentiment <- corpus_TS_sentiment %>%
  mutate(sentiment_value = ifelse(!is.na(sentiment), 1, 0))

corpus_TS_sentiment %>%
  
  ggplot(aes(sentiment_value, animal_count)) +
  geom_smooth()
