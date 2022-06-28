### NRC_quality_check

# load
my_nrc <- read.csv("SA_resources/NRC-Emotion-Lexicon-v0.92-InManyLanguages-web_jgm.csv", stringsAsFactors = F)

# remove empty annotations
my_nrc <- my_nrc[my_nrc$Agree. != "",]

# calcualte proportions (percentages)
table(my_nrc$Agree.)/length(my_nrc$Agree.)*100
