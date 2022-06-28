
# preparation of lexicons for analysis

library(tidyverse)
library(tidytext)
library(sjPlot)
library(ggsci)
library(wesanderson)
library(wordcloud)
library(readtext)
library(readxl)
library(XML)

# GERMAN STOPWORDS -------------

stop_german <- tibble(word = stopwords::stopwords("de"))

# add sentence case stopwords

stop_german2 <- stop_german
stop_german2$word <- str_to_sentence(stop_german2$word)
stop_german <- bind_rows(stop_german, stop_german2)
remove(stop_german2)


# DICTIONARIES ---------------

# SentiWS

sentiws <- bind_rows(
  read.csv("SA_resources/SentiWS_v2.0_Negative.txt", encoding = "UTF-8", header = F, sep = c("|", " ")) %>%
    as_tibble() %>%
    mutate(sentiment = "negative"),
  read.csv("SA_resources/SentiWS_v2.0_Positive.txt", encoding = "UTF-8", header = F, sep = c("|", " ")) %>%
    as_tibble() %>%
    mutate(sentiment = "positive")
) %>%
  rename(word = V1)

sentiws[, c("type", "polarity_weight", "inflection")] <- str_split_fixed(string = sentiws$V2, pattern = "\t", n = 3)
sentiws$V2 <- NULL


## SentiART ----------

sentiart <- read.csv("SA_resources/SentiArt.dat",
                     dec = ",",
                     encoding = "UTF-8")

names(sentiart)[1] <- "word_upper"

sentiart <-  sentiart %>%
  mutate(ang_z = (ang_z - mean(ang_z))/sd(ang_z))

# sentiart_long <- sentiart %>%
#   select(-1) %>%
#   gather(emotion, value, -word)



## Plutchnik ----------

plutchik <- read.csv("SA_resources/Plutchik_Emotionslexikon.csv",
                     encoding = "UTF-8")

names(plutchik)[1] <- "word"
names(plutchik)[6] <- "emotion"
names(plutchik)[7] <- "emotion2"


plutchik <- plutchik %>%
  filter(emotion != "") %>%
  filter(emotion != "??") %>%
  filter(emotion != "??emotion??") 

plutchik <- plutchik %>%
  filter(!is.na(Wortart)) %>%
  filter(Wortart != "emoticon" & Wortart != "")

  
## LANG ----------

LANG_processed <- read.table("SA_resources/LANG_processed.csv",
                             encoding = "UTF-8",
                             header = T)


## BAWL -----------


BAWL <- read.csv("SA_resources/BAWL-R.csv", encoding = "UTF-8")

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

for(i in 1:length(BAWL$WORD)){
  
  if(BAWL$WORD_CLASS[i] == "N")
    BAWL$WORD_LOWER[i] <- CapStr(BAWL$WORD_LOWER[i])
  
}

BAWL_emotions <- data.frame(word = BAWL$WORD_LOWER, valence = BAWL$EMO_MEAN, arousal = BAWL$AROUSAL_MEAN, imageability = BAWL$IMAGE_MEAN, stringsAsFactors = F)

remove(BAWL, CapStr, i)



## Klinger ---------

ekel <- read_table("SA_resources/Klinger_emotion_lexicon/Ekel.txt", 
                   col_names = F) %>%
  mutate(emotion = "ekel") %>%
  rename(word = X1)

freude <- read_csv("SA_resources/Klinger_emotion_lexicon/Freude.txt", 
                   col_names = F) %>%
  mutate(emotion = "freude") %>%
  rename(word = X1)

furcht <- read_csv("SA_resources/Klinger_emotion_lexicon/Furcht.txt",
                   col_names = F) %>%
  mutate(emotion = "furcht") %>%
  rename(word = X1)

trauer <- read_csv("SA_resources/Klinger_emotion_lexicon/Trauer.txt",
                   col_names = F) %>%
  mutate(emotion = "trauer") %>%
  rename(word = X1)

ueberraschung <- read_csv("SA_resources/Klinger_emotion_lexicon/Ueberraschung.txt",
                          col_names = F) %>%
  mutate(emotion = "ueberraschung" ) %>%
  rename(word = X1)

verachtung <- read_csv("SA_resources/Klinger_emotion_lexicon/Verachtung.txt", 
                       col_names = F) %>%
  mutate(emotion = "verachtung") %>%
  rename(word = X1)

wut <- read_csv("SA_resources/Klinger_emotion_lexicon/wut.txt", 
                col_names = F) %>%
  mutate(emotion = "wut") %>%
  rename(word = X1)

klinger <- bind_rows(ekel, freude, furcht, trauer, ueberraschung, verachtung, wut)

remove(ekel, freude, furcht, trauer, ueberraschung, verachtung, wut)
