library(readtext)
library(tidyverse)
library(stringr)
library(readxl)



# PREP -----------
# if you erased your corpus, run this to recreate it:

corpus_token_SA <- readtext("TS_corpus_txt", encoding = "UTF-8")  %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, drop = T) %>%
  as_tibble()  %>%
  group_by(doc_id) %>%
  mutate(sentence_id = seq_along(sentence)) %>%
  slice_sample(n = 150) %>% # random 150 sentences per text
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


# SPACE ----------

# if we want to analyse space, we need to "find" spatial items and label them.
# that can take some time and effort, so we did it for you.
# load the two files beloe: they contain labelled spatial items, and real coordinates of geolocations.

load("spatial_entities.RData")
load("geo_coordinates.Rdata")

## remove double entities (some have same multiple lon/lat) ------------

cooord_matches <- cooord_matches %>%
  group_by(admin, place) %>%
  mutate(top = seq_along(place)) %>%
  ungroup() %>%
  filter(top == 1) %>%
  anti_join(german_names %>%
              rename(place = first_name))



# now we can add that to our corpus, too

corpus_token_SA <- corpus_token_SA %>%
  left_join(all_entities %>%
              rename(token = word) %>%
              anti_join(stop_german) %>%
              # let's make sure the entities do not contain names or function words
              anti_join(german_names %>%
                          rename(token = first_name))
              
            )


# now we can see how many spatial entities match our corpus,
# and the proportion of the spatial presence in the german vs. swiss collection

# we can do this with dplyr 

corpus_token_SA %>%
  filter(!is.na(type)) %>%
  group_by(collection, type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(proportion = (n*100)/sum(n)) %>%
  arrange(type, collection)

# or with the janitor package

library(janitor)

corpus_token_SA %>%
  mutate(type = as.character(type)) %>%
  filter(!is.na(type)) %>%
  janitor::tabyl(type, collection)


# opr with visuals

corpus_token_SA %>%
  filter(!is.na(type)) %>%
  ggplot(aes(type, fill=type_grouped)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")  +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_sjplot("ipsum") +
  ggtitle("corpus") +
  ylim(... = c(0, 0.8)) +
  facet_wrap(.~collection)

# we can also have a look at which places are more present in the corpus

corpus_token_SA %>%
  filter(!is.na(type_grouped)) %>%
  filter(type == "geoloc_urb" | type == "geoloc_rur" | type == "geoloc_nat" ) %>%
  group_by(collection, token, category) %>%
  count() %>%
  ungroup() %>%
  select(collection, token, n, category) %>%
  rename(place = token)  %>%
  rename(token_count = n) %>%
  arrange(collection, category, desc(token_count)) %>%
  left_join(cooord_matches)
  
# and which spatial terms

corpus_token_SA %>%
  filter(!is.na(type_grouped)) %>%
  filter(type != "geoloc_urb" & type != "geoloc_rur" & type != "geoloc_nat" ) %>%
  group_by(collection, token, category) %>%
  count() %>%
  ungroup() %>%
  select(collection, token, n, category) %>%
  rename(place = token)  %>%
  rename(token_count = n) %>%
  arrange(desc(token_count), category, collection) 

# if we want to make a map, we only need to preserve the tokes that are alsp geolocation with lan/lon

corpus_space <- corpus_token_SA %>% 
  filter(category != "rural") %>%
  filter(category != "urban") %>%
  filter(category != "nat_terms") %>%
  filter(!is.na(type)) %>%
  group_by(collection, category, token) %>%
  count() %>%
  rename(place = token) %>%
  left_join(cooord_matches) %>%
  filter(!is.na(latitude) | !is.na(longitude))



# with spatial information from geonames.org, we can also plot space

library(tmap)
library(sf)
library(leaflet)

# maps ----------------------


DT_sf = st_as_sf(corpus_space, coords = c("longitude", "latitude"), crs = 4326)

tmap_mode("view")

tm_shape(DT_sf) +
  tm_dots(col = "category", size = "n", legend.show = T) +
  tm_facets(by = "collection")

# most frequent items

top_freq_space <- corpus_space %>%
  filter(n >= 10)

top_freq_space <- st_as_sf(top_freq_space, coords = c("longitude", "latitude"), crs = 4326)

tm_shape(top_freq_space) +
  tm_dots(col = "category", size = "n") +
  tm_text(text = "place") +
  tm_facets(by = "collection")

# mountains only


mountains_space <- corpus_space %>%
  filter(category == "mountain")

mountains_space <- st_as_sf(mountains_space, coords = c("longitude", "latitude"), crs = 4326)

tm_shape(mountains_space) +
  tm_dots(col = "category", size = "n") +
  # tm_text(text = "place") +
  tm_facets(by = "collection")




#---------- can we plot sentiment?

# we can average values per place in sentences that contain a geolocation,
# and obtain approximate sentiment values in relation to a location

corpus_space <- corpus_token_SA %>% 
  filter(category != "rural") %>%
  filter(category != "urban") %>%
  filter(category != "nat_terms") %>%
  filter(!is.na(type)) %>%
  group_by(token, AAPz) %>%
  summarise(n = n(),
            place = token,
            AAPz = mean(AAPz)) %>%
  filter(!is.na(AAPz)) %>%
  left_join(cooord_matches) %>%
  filter(!is.na(latitude) | !is.na(longitude)) %>%
  select(-token)


corpus_space <- st_as_sf(corpus_space, coords = c("longitude", "latitude"), crs = 4326)

tm_shape(corpus_space) +
  tm_dots(col = "AAPz", size = "AAPz")
  # tm_text(text = "place")

# we can also examine how the presence of entitiees compare to the entities in the space lists


GP_ent_long <- all_entities %>%
  mutate(type = as.character(type)) %>%
  filter(!is.na(type) & type != "rural" & type != "urban" & type != "natural" & type != "interior" ) %>%
  mutate(type = as.factor(type)) %>%
  pivot_longer(names_to = "country", cols = 4:8) %>%
  filter(!is.na(value))


table_df <- corpus_token_SA %>%
  filter(!is.na(type) & type != "rural" & 
           type != "urban" &
           type != "natural" &
           type != "interior" ) %>%
  pivot_longer(names_to = "country", cols = c("Italy", "France", "Germany", "Austria", "Switzerland")) %>%
  filter(!is.na(value))

table_df$type <- as.character(table_df$type)

table_df$type <- as.factor(table_df$type)




library(table1)
library(kableExtra)

rbind(
  table1( ~ country | type,
          data = GP_ent_long, overall=F) %>%
    as_tibble() %>%
    mutate(DF = ifelse(grepl("N", geoloc_nat), "Entities list", "")) %>%
    select(DF, everything()),
  
  table1( ~ country | type,
          data = table_df[table_df$collection == "CH",], overall=F) %>%
    as_tibble() %>%
    mutate(DF = ifelse(grepl("N", geoloc_nat), "Swiss Corpus", "")) %>%
    select(DF, everything()),
  
  table1( ~ country | type,
          data = table_df[table_df$collection == "DE",], overall=F) %>%
    as_tibble() %>%
    mutate(DF = ifelse(grepl("N", geoloc_nat), "German Corpus", "")) %>%
    select(DF, everything())
  ) %>%
  as.data.frame() %>%
  view()


table1(~ AAPz +
         fear_z +
         ang_z +
         hap_z +
         disg_z +
         surp_z +
         sad_z | collection * category, corpus_token_SA) 


# or have a more "qualitative" look at places and their values

