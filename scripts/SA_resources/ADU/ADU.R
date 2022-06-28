###Function to calculate emotion types for text files###
ADU <- function(path.wd="wd",path.lex="ADU-unicode-R.csv",
                   path.TT="TreeTagger",
                   chunks="auto",plot.format="png",plot.res = "png(width = 7, height = 7, units = 'in', res = 300)",file.choose="auto",
                   text.file="auto.choose",
                   use.TreeTagger=FALSE){
  
  
  ##Call packages
  require(dplyr)
  require(ggplot2)
  require(koRpus)
  #require(RColorBrewer) #for fixed colors
  
  ##Resolve path issues
  #introduce condition for "direction" of slashes
  path.to.wd <- getwd()
  #choose file (either text or lemma list)
  temp <- ifelse(text.file=="auto.choose",
                 file.choose(),
                 text.file
                  )
  
  
  if(length(grep("[\\\\]",temp))>0){
    #Set path to dictionary file
    lex.file <- ifelse(path.lex=="ADU-unicode-R.csv",
                       paste(path.to.wd,"\\","ADU-unicode-R.csv",sep=""),
                       paste(path.lex))
    #Set path to TreeTagger
    path.to.TT <- ifelse(path.TT=="TreeTagger",
                         paste(path.to.wd,"\\","TreeTagger",sep=""),
                         paste(path.TT))
  }else{
    if(length(grep("[/]",temp))>0){
      #Set path to dictionary file
      lex.file <- ifelse(path.lex=="ADU-unicode-R.csv",
                         paste(path.to.wd,"/","ADU-unicode-R.csv",sep=""),
                         paste(path.lex))
      #Set path to TreeTagger
      path.to.TT <- ifelse(path.TT=="TreeTagger",
                           paste(path.to.wd,"/","TreeTagger",sep=""),
                           paste(path.TT))
    }}
  
  ##Get TreeTagger list 
  if(use.TreeTagger==FALSE){
    tt <- read.table(temp,header = FALSE,sep="\t",fill=TRUE,quote=NULL)
    names(tt) <- c("token","tag","lemma")
  }else{
    if(use.TreeTagger==TRUE){
      #[from: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/]
      #tag text file (POS, lemmatization)  
      text.tagged <- treetag(temp,
                             treetagger = "manual", #settings for TreeTagger 
                             lang = "de", #set language
                             TT.options = list(path = path.to.TT, #set path to TreeTagger
                                               preset = "de", #set language
                                               params="german-utf8.par")) #specify parameter file
      #reformat object to data frame
      tt <- as.data.frame(text.tagged@TT.res)
    }
  }
  
  
  
  ##Import emotion type list
  EL.List <- read.table(lex.file, #define file and path
                      sep=",", #define field separator character
                      header=TRUE, #specify that the first row contains names of variables
                      col.names = c("Word","association","rating")) #specify that the first column contains rownames
                      #skip = 46) Wozu sollte das gut sein?
  
  ##Text Editing
  #edit treetagger results
  #add column for word order
  tt$wo <- rep(1:nrow(tt))
  #remove punctuations, numbers, symbols
  wordclass.ex <- c("fullstop","number","punctuation","symbol", "SENT","POS","CARD",":",",","(",")","'","‘","’","`","´","``","''","„","“","»","«","›","‹") #set categories to remove     
  tt <- droplevels(subset(tt, !tag %in% wordclass.ex))  #subset data frame
  #set categories to remove  
  tag.ex <- c("CARD",#Cardinal number
              "ART",#Determiner
              "KON",#Co-subordinating conjunction
              "KOKOM",#Comparing conjunction (als + wie) 
              "KOUI",#subordinating Conjunction (zu+Inf)
              "KOUS",#subordinating Conjunction (sentence)
              "NE",#Eigennamen
              "APPR",#Präposition
              "APPRART",#Präposition mit Artikel
              "PAV",#Pronominaladverb
              "PDAT",#attribuierendes Demonstrativpronomen
              "PDS",#substituierendes Demonstrativpronomen
              "PIDAT",#Indefinitpronomen
              "PIAT",#Indefinitpronomen
              "PPOSS",#substituierendes Possessivpronomen
              "PPOSAT",#attribuierendes Possessivpronomen
              "PRF", #reflexives Personalpronomen
              "PPER",#Personalpronomen
              "") #somehow empty tags occure (seem to be some sort of symbol marker)
  
  
  tt.sub <- droplevels(subset(tt, !tag %in% tag.ex)) #subset data frame
  tt.sub.neg <- droplevels(subset(tt, tag %in% tag.ex)) #subset data frame to get the lemmas renoved before
  #remove NA columns (inserted in TreeTagger)
  tt.sub$stop <- NULL
  tt.sub$stem <- NULL
  
  ##Function Matching    
  #prepare new data frame of matched words (lemma) and emotions
  tx.va.1 <- merge(tt.sub,EL.List,by.x = "lemma",by.y = "Word",all.y = FALSE, all.x = FALSE)
  #reorder df to order in source text
  tx.va.1 <- tx.va.1[order(tx.va.1$wo),]
  #extract outtakes
  tx.out <- droplevels(subset(tt.sub, !token %in% tx.va.1$token))
  
  ##Summary Statistics
  #define chunks
  #get unique word list
  tx.unique <- tx.va.1 %>%
    group_by(wo,token) %>%
    summarise() %>%
    arrange(as.numeric(wo)) %>%
    mutate(lab=paste(wo,token,sep=".")) #adds interaction variable
  
  chunks.fin <- ifelse(chunks=="auto",
                       ifelse(nrow(tx.unique) <= 5000,
                              100,
                              1000),
                       chunks)
  #add chunk variable to data frame
  #calculate bin values / no. of words
  s <- nrow(tx.unique)/chunks.fin
  s2 <- ceiling(s) #returns a numeric vector containing the largest integers not less than the corresponding elements of x
  s3 <- floor(s)
  s4 <- nrow(tx.unique)-s3*chunks.fin
  #add dummy variable for chunks
  tx.unique$chunk <- c(rep(1:s3, each=chunks.fin),rep(s2, s4))
  #add interaction variable
  tx.va.1$lab <- paste(tx.va.1$wo,tx.va.1$token,sep=".") 
  #add chunks ID to tx.va
  tx.va <- merge(tx.va.1,tx.unique[,c(3,4)],by="lab",all.x = TRUE)
  #re-order df
#   tx.va <- tx.va %>%
#     arrange(wo)
  tx.va <- tx.va[order(tx.va$wo),]
  #remove interaction variable
  tx.va$lab <- NULL
  
  #summarize data, add correlation statistics
  tx.summary <- tx.va %>%
    #select(lemma:V.Mean.Sum,A.Mean.Sum,chunk) %>%
    group_by(chunk,association) %>%
    summarise(sum.rating=sum(rating))%>%
    arrange(chunk,-sum.rating)

  #calculate word occurrences and order data frame accordingly  
  word.counts <- tx.unique %>%
    select(token) %>%
    group_by(token) %>%
    summarise(occurrence=n()) %>%
    arrange(-occurrence)
  
  #calculate words used in the analysis  
  words.iA <- tx.va %>%
    select(token,wo) %>%
    group_by(token,wo) %>%
    unique.data.frame()
  
  #extract words without rating
  words.wor <- tx.va %>%
    group_by(token,wo,lemma,tag,chunk) %>%
    summarise(sum.rating=sum(rating)) %>%
    filter(sum.rating==0) %>%
    select(token:chunk)
  
  words.wor <- words.wor[order(words.wor$wo),]

  #calculate unique words without rating
  words.wor.unique <- words.wor[!duplicated(words.wor$lemma),3]
  words.wor.unique <- words.wor.unique[order(words.wor.unique$lemma),]
  
  
  #calculate relative association frequencies
  #define emotions
  cat1 <- c("Liebe","Begeisterung","Zufriedenheit","Erleichterung","Freude",
            "Stolz","Zorn","Furcht","Depressivitaet","Schuld","Aengstlichkeit","Scham")
  #for sentiments
  #tx.senti <- tx.va %>%
    #filter(association %in% cat1) %>%
    #filter(rating >= 1) %>%
    #mutate(cat="Sentiments",
           #sum.senti=sum(rating))%>%
    #group_by(association,cat) %>%
    #summarise(rel.asso=sum(rating)/mean(sum.senti)*100)
  #for emotions
  tx.emo <- tx.va %>%
    #filter(!association %in% cat1) %>% #not needed since all associations are found and have a value = 1
    #filter(rating >= 1) %>%
    mutate(cat="Emotions",
           sum.senti=sum(rating))%>%
    group_by(association,cat) %>%
    summarise(rel.asso=sum(rating)/mean(sum.senti)*100)
  
  #re-combine associations to df (also for final bar chart)
  tx.pie <- rbind(tx.emo)
  
  summary.asso <- tx.pie %>%
    select(association,rel.asso) %>%
    mutate(PARAMETER=paste(association,": ",round(rel.asso,1),sep=""))
  # order
  summary.asso <- summary.asso[order(-summary.asso$rel.asso),]
  
  #make global summary
  word.classes <- tx.va %>%
    select(tag,wo,token) %>%
    unique.data.frame() %>% #reduces multiple tokens
    group_by(tag) %>%
    summarise(word.class=n()) %>%
    arrange(-word.class) %>%
    mutate(PARAMETER=paste(as.character(tag),": ",word.class,sep="")) %>%
    select(PARAMETER)
  
  #calculate text length
  #define categories to remove
    #remove.pun <- c("comma","fullstop","punctuation")
  #remove rows
    #tt.length <- subset(tt, !wclass %in% remove.pun)
  
  #make summary file  
  head.wclass <- data.frame(PARAMETER=paste("OCCURENCE-WORD-CLASSES"))
  head.asso <- data.frame(PARAMETER=paste("RELATIVE ASSOCIATIONS OF EMOTIONS"))
  head.words <- data.frame(PARAMETER=paste("WORDS"))
  word.sums <- data.frame(PARAMETER=paste("Percentage of words used in the analysis: ",round(nrow(words.iA)/(nrow(tt))*100,1),sep=""))
  Excluded.words <- data.frame(PARAMETER=paste("Excluded word classes: ","APPR, ","APPRART, ","ART, ","CARD, ","KON, ","KOKOM, ","KOUS, ","KOUI, ","NE, ","PAV, ","PDAT, ","PDS, ","PIDAT, ","PIAT, ","PPOSS, ","PPOSAT ,","PPER, ","PRF",sep=""))
  Excluded.words.n <- data.frame(PARAMETER=paste("Excluded words: ",nrow(tt.sub.neg),sep=""))
  Analyzed.words.n <- data.frame(PARAMETER=paste("Total words used in the analyses: ",nrow(words.iA),sep=""))
  Analyzed.words.unique <- data.frame(PARAMETER=paste("Unique words used in the analyses: ",length(unique(words.iA$token)),sep=""))
  Not.recognized.words.n <- data.frame(PARAMETER=paste("Unrecognized words: ",nrow(tx.out),sep=""))
  #Pearson.Ratings.R <- data.frame(PARAMETER=paste("Valence Pearson Estimate: ",round(cor.test(tx.va$chunk,tx.va$rating)$estimate,2),sep=""))
  #Pearson.Ratings.p <- data.frame(PARAMETER=paste("Valence Pearson p-value: ",round(cor.test(tx.va$chunk,tx.va$rating)$p.value,3),sep=""))
  text.length <- data.frame(PARAMETER=paste("Total words in Text: ",nrow(tt),sep=""))
  
  sum.all <- rbind(head.asso,
                   summary.asso[,3],
                   #Pearson.Valence.R,
                   #Pearson.Valence.p,
                   #Pearson.Arousal.R,
                   #Pearson.Arousal.p,
                   head.words,
                   text.length,
                   Analyzed.words.n,
                   Analyzed.words.unique,
                   word.sums,
                   Not.recognized.words.n,
                   Excluded.words,
                   Excluded.words.n,
                   head.wclass,
                   word.classes)
  
  ## Write Outputs
  #make output folder
  dir.name.1 <- ifelse(length(grep("[/]",temp))>0,
                       gsub("^.*[/^]", "", temp),
                       gsub("^.*[\\\\^]", "", temp))
  dir.name <- gsub(".txt","",dir.name.1)
  
  #dir.name <- paste(path.to.wd,"/","ValAro-output-",gsub(".txt","",dir.name),sep="")
  #dir.create(dir.name)
  #set wd to export files  
  #setwd(out.dir)
  #export summary statistics
  write.csv(tx.summary,file = paste(dir.name,"ADU_summary-statistics.csv",sep="-"),row.names=FALSE)
  #export word frequencies  
  write.csv(word.counts,file = paste(dir.name,"ADU_word-frequencies.csv",sep="-"),row.names=FALSE)
  #export outtakes  
  write.csv(tx.out,file = paste(dir.name,"ADU_outtakes.csv",sep="-"),row.names=FALSE)
  #export edited word list (raw.data)  
  write.csv(tx.va,file = paste(dir.name,"ADU_raw-words.csv",sep="-"),row.names=FALSE)
  #export edited word list (raw.data)  
  write.csv(sum.all,file = paste(dir.name,"ADU_global-summary.csv",sep="-"),row.names=FALSE)
  #export excluded word list (raw.data)  
  write.csv(tt.sub.neg,file = paste(dir.name,"ADU_excluded-words.csv",sep="-"),row.names=FALSE)
  
  ## Make plots
    #add positive negative variable
  tx.summary$cat <- ifelse(tx.summary$association %in% cat1,
                           "Emotions")
    # change order
  tx.summary$association <- factor(tx.summary$association, 
                                   levels=c("Liebe","Begeisterung","Zufriedenheit",
                                            "Erleichterung","Freude","Stolz","Zorn",
                                            "Furcht","Depressivitaet","Schuld",
                                            "Aengstlichkeit","Scham"
                                              ))
##############################################################################################
    # add fixed color values to emotions
      # line.Colors <- brewer.pal(12,"Paired")
      # names(line.Colors) <- levels(tx.summary$association)
      # colScale.line <- scale_colour_manual(name = "association",values = line.Colors)
      
    # make plot  
  p.val <- ggplot(tx.summary, aes(x=chunk,y=sum.rating,color=association))+
    geom_point()+
    geom_line(aes(color=association))+
    scale_color_manual(values = c("#A6CEE3","#1F78B4","#B2DF8A",
                                  "#FFFF99","#B15928","#FDBF6F","#FB9A99",
                                  "#FF7F00","#CAB2D6","#6A3D9A",
                                  "#5E4FA2","#4D4D4D"))+
    #colScale.line+
    #geom_smooth(aes(color=association),method = plot.method,se = FALSE)+
    theme_bw()+
    theme(legend.title=element_blank())+#remove legend title
    ggtitle(paste("Word order vs. summed ratings of associations,","\n",
                  "   Words per chunk:"," '",chunks.fin,"'",sep=""))+
    facet_wrap(~cat,nrow = 2)
  ggsave(filename = paste(dir.name,"-","Ratings-per-chunk-Plot.",plot.format,sep=""),
         plot = p.val,
         device = plot.format)

# make bar plot  
  #change order
  tx.pie$association <- factor(tx.pie$association, 
                                   levels=c("Liebe","Begeisterung","Zufriedenheit",
                                            "Erleichterung","Freude","Stolz","Zorn",
                                            "Furcht","Depressivitaet","Schuld",
                                            "Aengstlichkeit","Scham"
                                   ))
  # bar.Colors <- brewer.pal(12,"Paired")
  # names(bar.Colors) <- levels(tx.pie$association)
  # colScale.bar <- scale_fill_manual(name = "association",values = bar.Colors)
  
  tx.pie$cat <- factor(tx.pie$cat, 
                               levels=c("Emotions"))
  
  p.pie <- ggplot(tx.pie, aes(x=cat,y=rel.asso,fill=association))+
    geom_bar(stat = "identity", aes(fill=association))+#width = 1,
    #colScale.bar+
    scale_fill_manual(values =  c("#A6CEE3","#1F78B4","#B2DF8A",
                                  "#FFFF99","#B15928","#FDBF6F","#FB9A99",
                                  "#FF7F00","#CAB2D6","#6A3D9A",
                                  "#5E4FA2","#4D4D4D"))+
    coord_flip()+
    ylab("Relative Percentages")+
    theme_bw()+
    theme(legend.title=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_text(angle=90,face="bold"),
          axis.ticks=element_blank())+#remove legend title
    ggtitle(paste("Relative percentages of associations,","\n",
                  "   Words per chunk:"," '",chunks.fin,"'",sep=""))
  
  ggsave(filename = paste(dir.name,"-","Global-ratings-Plot.",plot.format,sep=""),
         plot = p.pie,
         device = plot.format)

}
###########################################################################################################
