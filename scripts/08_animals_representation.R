
library(tidyverse)
library(readxl)
library(tidytext)
library(readtext)
library(sjPlot)

# PREP -----------
# if you erased your corpus, run this to recreate it. can you do that?
# use the oher scripts to do so

## remember, it's called corpus_token_SA

#





###  Now, as we said for gender and for space, we want aggregated values by sentence
# corpus long -----

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

# can you find a source of animanls in the files in our folder and use it?

#









# you can create a corpus of animals. use the FEMALE corpus script from the gender script and adapt it









# now, we can have a look at our data.


# what's the mean sentiment for sentences with animals? (Sentiart)

library(table1)







# can you also add a grouping factor author?







# remember quanteda?
# can you print our keywords in context for the words Hund and Ratte?

