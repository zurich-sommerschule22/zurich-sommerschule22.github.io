library(readtext)
library(tidyverse)
library(stringr)
library(readxl)
library(tidytext)
library(sjPlot)


fs=9

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


# or with visuals

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
  ggtitle("space items percentages in CH and DE collections") +
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
  arrange(desc(token_count), category, collection)
  
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
  filter(n >= 5)

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
  group_by(token) %>%
  summarise(n = n(),
            place = token,
            AAPz = mean(AAPz),
            hap_z = mean(hap_z),
            ang_z = mean(ang_z),
            sad_z = mean(sad_z),
            surp_z = mean(surp_z),
            fear_z = mean(fear_z),
            disg_z = mean(disg_z)) %>%
  filter(!is.na(AAPz)) %>%
  left_join(cooord_matches) %>%
  filter(!is.na(latitude) | !is.na(longitude)) %>%
  select(-token)


corpus_space <- st_as_sf(corpus_space, coords = c("longitude", "latitude"), crs = 4326)

tm_shape(corpus_space) +
  tm_dots(col = "AAPz", size = "AAPz")
  # tm_text(text = "place")

tm_shape(corpus_space) +
  tm_dots(col = "hap_z", size = "hap_z")
# tm_text(text = "place")

tm_shape(corpus_space) +
  tm_dots(col = "fear_z", size = "fear_z")
# tm_text(text = "place")

tm_shape(corpus_space) +
  tm_dots(col = "disg_z", size = "disg_z")
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

# we can have a look graphically at the space item freuqencies

corpus_token_SA %>%
  filter(!is.na(category)) %>%
  group_by(token, category, collection
  ) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  head(50) %>%
  ungroup() %>%
  # pivot_wider(values_from = freq, names_from = type) %>%
  ggplot(aes(x=freq,
             y=reorder(token, freq),
             label=freq, color=category, fill=category)) +
  geom_bar(stat = "identity", width = .15) +
  geom_point() +
  geom_text(nudge_x = 10) +
  ylab("interior item") +
  xlab("number of total occurrences in corpus") +
  facet_wrap(. ~ collection)



# spatial items with higher AAPz value 

corpus_token_SA %>%
  group_by(type, token) %>%
  summarise(n = mean(AAPz)) %>%
  arrange(type, desc(n)) %>% # highest on top
  mutate(top = seq_along(token)) %>% # identify rank within group
  filter(top <= 15) %>% # keep top 15 frequent words
  ggplot(aes(x = -top, fill = type)) +
  # create barplot
  geom_bar(aes(y = n), stat = 'identity', width = .05) +
  geom_point(aes(y = n), stat = 'identity') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = 0.5,
                label = paste0(token, ", ", round(n, 3))), size = fs/3, hjust = "left", nudge_x = .55) +
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + # remove y text
  labs(title = "Highest avg_annotation_valence of interior items (sentence) by space type") +
  facet_grid(. ~ type,  # scales="free_x"
  ) + # separate plot for each sentiment
  coord_flip()  + # flip axes
  # scale_fill_sjplot("ipsum") +
  geom_hline(aes(yintercept=0)) +
  ylim(-1, 5)

