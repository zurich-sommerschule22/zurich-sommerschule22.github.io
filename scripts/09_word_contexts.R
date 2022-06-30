# The research question:
# i would be interested to see the possibilities of extracting data about word relations. 
# For example i am interested in literary spaces so it would be interesting to compare the surroundings of certain words, 
# like which words are found around the word 'city' and which around the word 'mountain'. 

# Possible answers: 
# 1. keyword in context
# ...can be found in Voyant: https://voyant-tools.org/docs/#!/guide/contexts
# 2. word collocations
# ...good example: https://www.dwds.de/wp/?q=Stadt

# but we need to do this in our corpus
# using R
# ...and working together... :)

# we will be using the udpipe package
install.packages("udpipe")
library(udpipe)
# ...and tidyverse
library(tidyverse)

# first, some examples of how udpipe works

# choose a text
my_text <- "To be or not to be. That is the question, my dear Watson."

# annotate it with udpipe!
x <- udpipe(x = my_text, object = "english")
View(x)

# of course, you can also work on different languages
my_text <- "Nel mezzo del cammin della mia vita, mi ritrovai con il mio caro Watson."

# annotate it with udpipe!
x <- udpipe(x = my_text, object = "italian")
View(x)

# then we can work with our German/Swiss texts
my_text <- readLines("scripts/TS_corpus_txt/CH/federer_wunder_1919.txt")
my_text <- paste(my_text, collapse = "\n\n")

p <- udpipe(x = my_text, object = "german")

# load the corpus
# already prepared by Giulia :)
load("scripts/samples/TS_corpus_small.RData")

# let's explore it a bit
View(corpus_small)

# how many books are there?
corpus_small %>% 
  select(doc_id) %>%
  distinct()

# now, we can start working on it!!

# first, let's find the appearances of a certain word in the text
lemma_found <- which(corpus_small$lemma== "Stadt")
lemma_found

### Our Turn (1) - start

# visualize keyword in context 
for(i in 1:length(lemma_found)){
       
}

### Our Turn (1) - end


### Our Turn (2) - start

# calculate word co-occurrences
# suggestion: take a look here:
# https://bnosac.github.io/udpipe/docs/doc5.html
# NOTE: it's just the art of copy-pasting :)

### Our Turn (2) - end


### Our Turn (3) - start

# calculate significant word co-occurrences
# suggestion: take a look here:
# https://tm4ss.github.io/docs/Tutorial_5_Co-occurrence.html
# NOTE: it's the art of copy-pasting
# ...and adapting...

# In the end, we need to adapt this:
# [to calculate] the significance of the joint occurrence of a term i (coocTerm) with any other term j: 
# [we need:]
# * k - Number of all context units in the corpus 
# * ki - Number of occurrences of coocTerm 
# * kj - Number of occurrences of comparison term j 
# * kij - Number of joint occurrences of coocTerm and j

# let's start from k
k <- ...

# then ki
my_lemma <- "Auge"
ki <- ...

# kj (a bit more complicated)
# initialize the variable
kj <- numeric()

# reduce the corpus to just nouns and adjectives (as above)
corpus_small <- corpus_small %>% filter(upos %in% c("NOUN", "ADJ"))

# then run a loop
for(i in 1:length(unique(corpus_small$lemma))){
  
  kj[i] <- ...
  
  # ...and print the progress, just to see how it goes
  print(i/length(unique(corpus_small$lemma)))
  
}

# assign names to kj
names(kj) <- ...
# let's see it!
kj

# and finally... kij!
# initialize 
kij <- rep(0, length(kj))
names(kij) <- names(kj)

# we can work on cooc1, by reducing it...
cooc_tmp <- cooc1[...]

# and run a loop on this dataframe
for(i in 1:length(cooc_tmp$term1)){
  
  # problem: we need to get the lemma that is not our "coocTerm"
  my_term_id <- which(...)
  my_term <- cooc_tmp[...]
  kij[which(names(kij) == my_term)] <- ...
  
}

# let's see the result!!!
kij
head(sort(kij, decreasing = T))

# then we can start with stats!
# just copy/pasting :)

# https://tm4ss.github.io/docs/Tutorial_5_Co-occurrence.html
