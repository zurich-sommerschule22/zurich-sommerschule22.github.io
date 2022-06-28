# Stylometry on Swiss/German corpus

# Install and call the package
install.packages("stylo")
library(stylo)

# Important note:
# Stylo will work by default with the files in the "corpus" folder
# I have prepared some working documents in the "scripts/stylo" folder
# for this reason, you need to change working directory
setwd("/cloud/project/scripts/stylo")

# First analysis (dendrogram with 100 MFW and classic Delta distance)
stylo(corpus.format="plain",
      corpus.lang="German", 
      mfw.min=100, 
      mfw.max=100,
      mfw.incr=0,
      distance.measure="dist.delta",
      analysis.type="CA",
      write.jpg.file=T,
      plot.custom.height=16,
      plot.custom.width=9)

# Second analysis (dendrogram with 100 MFW and Cosine Delta distance)
stylo(corpus.format="plain",
      corpus.lang="German", 
      mfw.min=100, 
      mfw.max=100,
      mfw.incr=0,
      distance.measure="dist.wurzburg",
      analysis.type="CA",
      write.jpg.file=T,
      plot.custom.height=16,
      plot.custom.width=9)

# Third analysis (dendrogram with 2,000 MFW)
stylo(corpus.format="plain",
      corpus.lang="German", 
      mfw.min=2000, 
      mfw.max=2000,
      mfw.incr=0,
      distance.measure="dist.wurzburg",
      analysis.type="CA",
      write.jpg.file=T,
      plot.custom.height=16,
      plot.custom.width=9)

# Fourth analysis (Consensus tree with 2000 MFW)
stylo(corpus.format="plain",
      corpus.lang="German", 
      mfw.min=100, 
      mfw.max=2000,
      mfw.incr=100,
      distance.measure="dist.wurzburg",
      analysis.type="BCT",
      write.jpg.file=T,
      plot.custom.height=16,
      plot.custom.width=16)

# Fifth analysis (dendrogram, working on the entire corpus)
stylo(corpus.format="plain",
      corpus.lang="German", 
      mfw.min=2000, 
      mfw.max=2000,
      mfw.incr=0,
      distance.measure="dist.wurzburg",
      analysis.type="CA",
      write.jpg.file=T,
      plot.custom.height=20,
      plot.custom.width=10,
      plot.font.size = 5,
      frequencies="full_corpus_dtm.txt")
# here files are not actually read from the "corpus" folder
# as word frequencies are already provided in "full_corpus_dtm.txt" file

# Sixth analysis (consensus tree, working on the entire corpus)
stylo(corpus.format="plain",
      corpus.lang="German", 
      mfw.min=100, 
      mfw.max=2000,
      mfw.incr=100,
      distance.measure="dist.wurzburg",
      analysis.type="BCT",
      write.jpg.file=T,
      plot.custom.height=20,
      plot.custom.width=20,
      plot.font.size = 5,
      frequencies="full_corpus_dtm.txt")

# Here is an overview of the options to put between the brackets
# 
# # Type of the corpus
# 
# corpus.format = ...
# - you can choose between "plain", "xml", "xml.drama", "html", and many others
# [Example]
# stylo(corpus.format = "plain")
#
# # Language of the corpus
# 
# corpus.lang = ...
# - you can choose between "English", "German", "Italian", "Latin", and many others
# [Example]
# stylo(corpus.format = "plain", corpus.lang = "Italian")
# 
# # Most frequent words
# 
# mfw.min = ...
# - any integer number
# mfw.max = ...
# - any integer number
# mfw.incr = ...
# - any integer number
# 
# [Example]
# stylo(corpus.format = "plain", corpus.lang = "Italian", mfw.min = 100, mfw.max = 1000, mfw.incr = 100)
# - (this will perform 10 analyses with 100, 200, 300, etc. MFW)
# 
# # Distance measures 
# 
# distance.measure = "..."
# - you can choose between the following:
#   - "dist.delta"
#   - "dist.euclidean"
#   - "dist.manhattan"
#   - "dist.argamon"
#   - "dist.eder"
#   - "dist.simple"
#   - "dist.canberra"
#   - "dist.cosine"
#   - "dist.wurzburg"
#   - "dist.minmax"
# 
# [Example]
# stylo(corpus.format = "plain", corpus.lang = "Italian", mfw.min = 100, mfw.max = 1000, mfw.incr = 100, distance.measure = "dist.wurzburg")
# - (this will perform 10 analyses with 100, 200, 300, etc. MFW, using the Wurzburg distance, i.e., Cosine Delta)
# 
# # Analysis type (i.e. visualization)
# 
# analysis.type = 
# - you can choose between the following:
#   - "CA"
#     - (that is cluster analysis, i.e. dendrograms)
#   - "BCT"
#     - (that is bootstrap consensus tree)
# 
# [Example]
# stylo(corpus.format = "plain", corpus.lang = "Italian", mfw.min = 100, mfw.max = 1000, mfw.incr = 100, distance.measure = "dist.wurzburg", analysis.type = "BCT")
# - (this will perform 10 analyses with 100, 200, 300, etc. MFW, using the Wurzburg distance, i.e., Cosine Delta, and will use them to generate a single consensus tree)
# 
# Much more details are available here: https://github.com/computationalstylistics/stylo_howto/blob/master/stylo_howto.pdf
# Note that if you will install Rstudio in your laptop, stylo will also have a graphical interface to set up these features

# Oppose analysis
oppose(text.slice.length = 3000,
       oppose.method = "craig.zeta",
       write.png.file = TRUE)
# this function takes the texts from two folders:
# "primary_set" and "secondary_set"

# Analysis on full corpus will make RStudio Cloud run out of memory
# The result of such analysis has been saved as "Oppose_full_analysis.png"
# In the "stylo" folder