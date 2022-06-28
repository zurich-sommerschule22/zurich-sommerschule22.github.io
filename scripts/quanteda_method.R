
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)

load(file = "TS_corpus.Rdata")


# corpus_sample with 100 sentences per text

corpus_TS <- corpus_TS %>%
  filter(sentence_id >= 30 & sentence_id <= 130)

gc()


corpus_q <- corpus_TS %>%
  select(doc_id, sentence, sentence_id, doc_id, author, title, pub_date) %>%
  distinct()
# 
corpus_q <- corpus_q %>%
  group_by(doc_id) %>%
  summarise(text = paste0(sentence, collapse = "/n "))


corpus_q <- corpus(corpus_q)

# save(corpus_q, file = "C:/Users/grig/sciebo_swissness/test_swiss_corpus_R/corpus_q.Rdata", compress = F)
# load(file = "C:/Users/grig/sciebo_swissness/test_swiss_corpus_R/corpus_q.Rdata")


# Corpus summary and plots ------------------

tokeninfo <- summary(corpus_q)

# tokeninfo$pub_date <- docvars(corpus_q, "pub_date")

tokeninfo %>% 
  ggplot(aes(x = pub_date, y = Tokens, group = 1)) + 
  geom_line() + geom_point()


# Tokens corpus ---------------

## remove stopwords, update language encoding (de) and remove names
# 
# library(readxl)
# 
# names_de <- read_excel("NER/geogr_terms_new.xlsx", sheet = "person_de") %>%
#   filter(label != "prof") %>%
#   select(word) %>%
#   filter(!is.na(word))
# 
# names_de_list <- names_de$word
# 
# corpus_tokens <- tokens(corpus_q, remove_punct = TRUE, remove_numbers = TRUE) %>% 
#   # tokens_remove(pattern = stopwords("german")) %>%
#   tokens_remove(names_de_list) %>%
#   tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")

corpus_tokens <- tokens(corpus_q)


# Kwic ----------------

## single words

kwic(corpus_tokens, pattern = "Berg", window = 50) %>% 
  head(20)  # show context of the first X occurrences

kwic(corpus_tokens, pattern = "Berg*") %>% 
  head()


## regular expressions

kwic(corpus_tokens, pattern = "Berg*", valuetype = "regex") %>% 
  head()

## phrases 

kwic(corpus_tokens, pattern = phrase("Die Sonne")) %>%
  head()

# remove(corpus_tokens)

gc()

# Topic models --------------

# create a dfm corpus

corpus_quant_dfm <- corpus_tokens %>%
  dfm()

corpus_quant_dfm <- dfm_trim(corpus_quant_dfm, min_termfreq = 4, max_docfreq = 10)

summary(corpus_quant_dfm)

# fit model and plot

set.seed(100)

if (require("stm")) {
  my_lda_fit20 <- stm(corpus_quant_dfm, K = 20, verbose = FALSE)
  plot(my_lda_fit20)
}


# Stats ---------------

## frequencies

textstat_frequency(corpus_quant_dfm, n = 5, groups = title)

textstat_frequency(corpus_quant_dfm, n = 100)

### plots

corpus_quant_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

### wordcould

textplot_wordcloud(corpus_quant_dfm, max_words = 100)

