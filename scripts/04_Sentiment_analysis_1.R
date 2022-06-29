# Where do I start? Create your corpus and set up your data with R Studio -----



# This is an R script file, created by Giulia (reads like English "Julia")

# Everything written after an hashtag is a comment (normally appears in green). If you don't want to type the hash manually every time, you can type your comments normally, and after you finish, with the cursor on the sentence, press ctrl+shift+c. it will turn text into a comment and vice versa.

# Everything else is R code. To execute the code, place the cursor on the corresponding line and press Ctrl+Enter (windows)

# If you are a beginner, don't worry: for today's practice you will not need much knowledge of R. The scripts are provided for you. You will be guided through a simple case exploratory Sentiment Analysis, and then use those same scripts to experiment with data in your possess or of your choice.
# If you are unfamiliar with R language and basic operations and want to learn more about it, there is plenty of tutorials online. Have a look at the resources at the end of this script for a few recommendations.

# before you start, check the working directory!
# you can click on the Files panel, go to the folder you are going to work in, and once you are inside click on the little arrow near the "More" button, and select "Set as working directory"


# now we're ready to start!




# PS: Have you noticed that there is a little symbol on the top right of this panel, made of little horizontal lines? It shows and hide the document outline. if you write a comment (any text preceded by a "#" and a space) and put a series of --------- (any number, more than 4) after it, the comment will become the header of a section, which you can then see in the outline for an easier navigation of the script.



# Creating your dataset ----------

# Often one of the factors that prevents us humanists from doing computational analysis is that tutorials sometimes assume that a certain amount of knowledge is somehow pre-existing. Unfortunately, it is often not the case.
# So it happens that right when you want to finally try to adapt someone else's existing scripts to your lovely literary texts (yes, that's how we often do, and it's ok!), you are not really sure how to put those books into a shape that you can use and analayse.

# Here we will try and show how different text formats can be imported in R and made ready for some analysis.

## packages -----


# Before you begin you will need to load some packages. These allow you to execute specific operations.
# If you have not done so already, you have to install them first: it might take a few minutes and you only have to do it once. If R asks you whether you want to install dependencies for the packages, say yes.

install.packages("tidyverse")
install.packages("tidytext")
install.packages("readtext")
install.packages("readxl")
install.packages("syuzhet")

# Once you have installed the packages you can comment the installation code like this (as mentioned, with "# " at the beginning of a line):

# install.packages("blablabla")

# so this operation will not be execute again in the future.


library(tidyverse)
library(tidytext)
library(readtext)
library(readxl)
library(syuzhet)



# Importing data ----

## txt ----

# One easy way to import texts into R is to start from txt files.

# You might have more than one, so it is important that you store them all together in one folder, and ideally with a consistent filename. Information in the filename can be used later on to add metadata to your dataset. The format "surname_title_year.txt" could be a good option, for example, where the surname and the title have to be one word.

# In order to import a txt file, you can use the "read.delim" function from base R (which means you do not need to install extra packages). 

# let's try it out. As you can see in the files panels, there is a folder called "samples", where some texts in different formats are stored.

# before you execute the code, make sure the working directory is set to your main repository folder (the one "above" the /samples folder)


federer_pilatus <- read.delim("samples/federer_pilatus_1912.txt", # this is the url to your file
                              fileEncoding = "utf-8",  # we want to read it as unicode text
                              header = F) %>% # we do not want the first line as a header 
  rename(text = V1) # we can name the column text

# your file has been imported! in this case, it looks just fine.
# It could be that your texts has lost the sentence structure and it's just one very long string of text. If so, you can split it into sentences, for instance with packages tidytext (the result will be a dataframe), with the formula below:

# federer_sentences <- tidytext::unnest_sentences(federer, input = "text", output = "sentence", to_lower = F)

head(federer_pilatus)


# YOUR TURN 1 ---------

# can you create a corpus with another file in the samples folder?







## multiple txt files ----------

# if you have more than one text, you probably won't want to repeat this operations manually several times.
# you can then proceed as follows:
# (this is just one way but there are many out there)

# run the "readtext" function from the "readtext" package, simply indicating the folder in which your texts are stored, and the format preceded by "*." (this means "all files that have this extension").

corpus <- readtext("samples/*.txt", encoding = "UTF-8")  %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, drop = T) %>%
  as_tibble()

head(corpus)


# let's see which files are in our corpus:

corpus %>% 
  select(doc_id) %>%
  distinct()


# now, as we mentioned you might want to use the information in the filename to create more variables (that's how "columns" are called in R) in our corpus

corpus <- corpus %>%
  mutate(sentence = str_squish(sentence)) %>% # eliminate unwanted extra spaces
  separate(doc_id, into = c("author", "title", "year"), sep = "_", remove = T) %>% # and separate the metadata
  mutate(year = str_remove(str_trim(year, side = "both"), ".txt")) # and make sure there are no extra spaces before/after the words

corpus$year <- as.numeric(corpus$year)

# let's see how it looks

head(corpus)

# Neat, right?

# you might also want to add an identification number for the sentences, which can be useful for later analysis

corpus <- corpus %>%
  group_by(title) %>%
  mutate(sentence_id = seq_along(sentence)) %>% # this means "sequence along the column sentence"
  ungroup()

# and we might want then to split the text into tokens. we can easily use the unnest_tokens function of tidytext:

corpus_token <- unnest_tokens(corpus, input = "sentence", output = "token", to_lower = F, drop = F)

# as we did for sentences, we might want to preserve the position of the tokens insider sentences, and add a token_id index

corpus_token <- corpus_token %>%
  group_by(title, sentence) %>%
  mutate(token_id = seq_along(token)) %>% # this means "sequence along the column "token"
  ungroup()


# so let's see how does it look now

head(corpus_token, 10)

# splitting into tokes can be useful if we want to match our corpus to lists of 
# labelled data (for instance, locations or sentiment lexicons).
# We'll talk about this during the SA session.





## csv and xslx ----

# another common format for texts is csv or xlsx. Importing a this is very easy, because R understands the csv and xslx formats well. You can either use code, or click directly on the file you want to import in the files panel.
# R studio will ask if you want to import it, and you will be able to determine options with a friendly interface.

# navigate into the samples folder and click on the file small_corpus.xlsx. or 
# execute the following code

pride_excel <- read_excel("samples/pride.xlsx")

# have a look at it

head(pride_excel)


## multiple files ----

# the procedure similar to the one we saw for the txt files, except it has read_excel as function, and it does not need to add a header or other variables

corpus_source <- readtext("samples/*.xlsx")

head(corpus_source)

## epubs ----------

## another format that is quite popular today, especially for books, is the epub
## for these files you will need a specific package, calle "epubr"

# install.packages("epubr")

library(epubr)

kafka_all <- epubr::epub("samples/kafka.epub")

# have a look a the dataset "kafka_all"

structure(kafka_all)

# epubs often have a more complex internal structure, and you might need to modify your dataset according to your needs

kafka_werke <- kafka_all[[9]][[1]]

head(kafka_werke)

# ---------------- SENTIMENT ANALYSIS


# for the next part we only need our corpus of multiple texts that we have already shaped in sentences. let's remove what is unnecessary:

remove(corpus_source, corpus_files, federer_pilatus, kafka_all, kafka_werke, pride_excel)

# you should now only have the dataframes "corpus" and "corpus_token" in your environment.
# these two are the same, just in different shapes.
# you can check the content of both one more time to be sure

corpus %>% 
  select(author, title, year) %>%
  distinct()

corpus_token %>% 
  select(author, title, year) %>%
  distinct()

# with this same scripts we can have a look at which and how many authors there are in our corpus

corpus %>% 
  select(author) %>%
  distinct() 

corpus %>% 
  select(author) %>%
  distinct() %>%
  nrow()


# YOUR TURN 2

# can you figure out how to print out which years are present in our corpus,
# and how many they are?



# Importing sentiment lexicons --------

# What you might want to do at thius point, is to import the lexicons you will use for your sentiment analysis. 
# Lexicons are often available as part of several packages.
# for example, you may use the syuzhet package to import the nrc lexicons, which has a German version,

syuzhet_nrc <- get_sentiment_dictionary("nrc", language = "german")

# or even the lexicon provided by syuzhet itself

syuzhet_de <- get_sentiment_dictionary(language = "german")

# Notice that these measure different things. unless you want to compare preformances,

# You might need to decide which lexicon works best for you, and be aware of how it was created.


# simple SA with syuzhet -----

# syuzhet allows you to run basic SA operations very easily, like extracting the sentiment value per sentence into a vector of values for the text by federer.

federer_sents <- syuzhet::get_sentiment(corpus$sentence[grepl("federer", corpus$author)], language = "german")

# if you want to check that it did really took values for sentences, you can verify that the number of sentiment values in the federer_sents vector equals the number of sentences in the corpus that have "federer" as an author:

nrow(corpus[grepl("federer", corpus$author), "sentence"]) == length(federer_sents)



# syuzhet produces also plots

plot(
  federer_sents, 
  type="p", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence",
)


# I prefer to have a more "visual" (though more memory intensive) approach, applying the sentiment lexicon directly to our corpus:

# for instance we can se the average sentiment per text

corpus_token %>%
  mutate(word = tolower(token)) %>%
  left_join(syuzhet_de) %>%
  group_by(title) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>%
  ggplot(aes(title, sentiment_value)) +
  geom_point(size=3)


# or per pub_year

corpus_token %>%
  mutate(word = tolower(token)) %>%
  left_join(syuzhet_de) %>%
  group_by(year) %>%
  summarise(sentiment_value = mean(value, na.rm=T)) %>%
  ggplot(aes(year, sentiment_value)) +
  geom_point(size=3)

# let's remove again unnecessary files

remove(syuzhet_de, syuzhet_nrc, corpus, federer_sents)

# and free empty cells

gc()


# Also, it is important to remember that dictionaries within packages can be updated or change over time (or become unavailable). 

# You can then decide that you prefer to store them locally, so that you can be sure they do not change and that you will be able to access them whether you are online or not.

# for example, if you go to the official source of the NRC lexicon developed by Saif Mohammad, https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm , you will be able to download the lexicon in several formats.

# We have gathered several sentiment lexicons for german in the "SA_resources" folder.
# The biggest (and presumabely more reliable one) is SentiArt, created by Arthur Jacobs.
# let's import it and have a look at it



## SentiART ----------

sentiart <- read.csv("SA_resources/SentiArt.dat",
                     dec = ",",
                     encoding = "UTF-8")

names(sentiart)[1] <- "word_upper"

sentiart <-  sentiart %>%
  mutate(ang_z = (ang_z - mean(ang_z))/sd(ang_z))

head(sentiart)


# each word has several values that indicate the polarity (AAPz) and the intensity of discrete emotions for each word in the lexicon.


# let's see the average AAPz per text

corpus_token %>%
  mutate(word = tolower(token)) %>%
  left_join(sentiart) %>%
  group_by(title) %>%
  summarise(sentiment_value = mean(AAPz, na.rm=T)) %>%
  ggplot(aes(title, sentiment_value)) +
  geom_point(size=3)


# and per sentence

corpus_token %>%
  mutate(word = tolower(token)) %>%
  left_join(sentiart) %>%
  group_by(title, sentence_id) %>%
  summarise(sentiment_value = mean(AAPz, na.rm=T)) %>%
  ggplot(aes(sentence_id, sentiment_value, color=title)) +
  geom_point() +
  geom_smooth(color="black") +
  facet_wrap(. ~ title, scales = "free_x")


# or per pub_year

corpus_token %>%
  mutate(word = tolower(token)) %>%
  left_join(sentiart) %>%
  group_by(year) %>%
  summarise(sentiment_value = mean(AAPz, na.rm=T)) %>%
  ggplot(aes(year, sentiment_value)) +
  geom_point(size=3)


# you can already see that syuzhet and sentiart appear to have very similar results, but remember that sometimes lexicons have different outcomes


# YOUR TURN 3 ---------------

# we have only looked at the AAPz value, can you print some plots for the other sentiment vlues?




