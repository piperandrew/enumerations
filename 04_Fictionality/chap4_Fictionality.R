### Chap 4 Fictionality ###
#This chapter assesses the uniqueness of fictional texts when compared to non-fiction.
#it identifies features that are distinctive of fictional writing under a variety of conditions
library(tm)
library("kernlab")
library("caret")
library(splitstackshape)


### 4.1 ###
### Testing how many words allow for proper classification of fiction and non-fiction ###

#first step is to extract text files of fixed lengths in incremental steps (100, 200, 300 etc)
#demonstration only
filenames1<-list.files("[DIRECTORY 1]")
filenames2<-list.files("[DIRECTORY 2]")
window<-2000
for (i in 1:length(filenames2)){
  #set working directory 1
  setwd("[DIRECTORY 1]")
  #ingest work
  work1<-scan(filenames1[i], what="character", quote="")
  #subset by window
  work1.sub<-work1[1:window]
  text.whole1<-paste(work1.sub, collapse=" ") # collapse into single chunk
  text.char1<-as.String(text.whole1)
  #set working directory 2
  setwd("[DIRECTORY 2]")
  #ingest work
  work2<-scan(filenames2[i], what="character", quote="")
  #subset by window
  work2.sub<-work2[1:window]
  text.whole2<-paste(work2.sub, collapse=" ") # collapse into single chunk
  text.char2<-as.String(text.whole2)
  setwd("[OUTPUT DIRECTORY]")
  file.use1<-paste(window, filenames1[i], sep="_")
  write(text.char1, file=file.use1)
  file.use2<-paste(window, filenames2[i], sep="_")
  write(text.char2, file=file.use2)
}

#run LIWC tables on each increment
#this gives you a LIWC table for every segment length for every text
#output = "LIWC_Novel_Segments"

#then run classifier on each LIWC table for each increment
#data included in repository
filenames<-list.files("LIWC_Novel_Segments")
final.df<-NULL
for (i in 1:length(filenames)){
  print(i)
  #ingest the LIWC table
  a<-read.csv(filenames[i], sep = "\t")
  #Label your data by corpus
  label.df<-data.frame(a$Filename)
  colnames(label.df)<-c("filenames")
  label.df<-cSplit(label.df, 'filenames', sep="_", type.convert=FALSE)
  label.df<-cSplit(label.df, 'filenames_5', sep=".", type.convert=FALSE)
  a$corpus<-label.df$filenames_5_1
  a<-a[a$corpus == "CT" | a$corpus == "HIST", ]
  df<-a
  #create folds for cross-validation
  folds<-createFolds(df$corpus, k=10) # k = number of folds (1-10) where 10 is best
  #run 10-fold cross validation
  cv.results<-lapply(folds, function(x){
    df.train<-df[-x,]
    df.test<-df[x,]
    df.model<-ksvm(corpus ~ ., data=df.train, kernel="rbfdot")
    df.pred<-predict(df.model, df.test)
    con.matrix<-confusionMatrix(df.pred, df.test$corpus, positive = "CT")
    f1<-con.matrix$byClass[[7]] 
    #p<-con.matrix$overall[[6]]
  })
  #calculate the avg F1 score for all folds for each segment
  F1.score<-mean(unlist(cv.results))
  seg<-as.numeric(label.df$filenames_1)[1]
  temp.df<-data.frame(seg, F1.score)
  final.df<-rbind(final.df, temp.df)
}
final.sort<-final.df[order(final.df$seg),]
write.csv(final.sort, file="Novel_Segment.csv")

### Fig. 4.1 ###
final.sort<-read.csv("Novel_Segment.csv")
ggplot(final.sort, aes(x=seg, y=F1.score)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(shape=22, size=2, fill="grey") +
  geom_line() +
  ylim(0.75,1.0) +
  labs(x="Number of Words", y="Accuracy (F1)", caption="\nFig. 4.1 Accuracy of predicting fictional texts using an increasing\n number of words from the beginning of a document.\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")
  #labs(x="Words", y="Accuracy")

### 4.2 ###
### Classification ###
#this script takes as input a feature table with each corpus labeled
#this chapter uses the LIWC feature sets
#each comparison is run separately; make sure to change positive identifier
#feature tables are located in "LIWC_Feature_Tables"

#load feature tables
a<-read.csv("LIWC_HATHI_FIC.csv")
b<-read.csv("LIWC_HATHI_TALES.csv")
#clean
#b<-b[,-2]
#b$corpus<-c("TALES")
#check to see if columns match
which(colnames(a) != colnames(b))
#select random subset of larger corpus
#a.all<-a
#a<-a.all[sample(nrow(a.all), nrow(b)),]
#combine into single data frame
df<-rbind(a, b)
#remove word counts to control for length as factor (we don't want that)
df<-df[,-1]
#create folds
folds<-createFolds(df$corpus, k=10) # k = number of folds (1-10) where 10 is best
#run 10-fold cross validation
cv.results<-lapply(folds, function(x){
  df.train<-df[-x,]
  df.test<-df[x,]
  df.model<-ksvm(corpus ~ ., data=df.train, kernel="rbfdot")
  df.pred<-predict(df.model, df.test)
  con.matrix<-confusionMatrix(df.pred, df.test$corpus, positive = "HATHI_FIC") #change this identifier for each corpus
  f1<-con.matrix$byClass[[7]] 
  #p<-con.matrix$overall[[6]]
})
unlist(cv.results) #this shows you the F1 score for each fold
mean(unlist(cv.results)) #this is the average F1 score for all folds

#to train on one data set and run on another
#load training data and relabel corpus
a<-read.csv("LIWC_HATHI_FIC.csv")
a$corpus<-c("NOV")
b<-read.csv("LIWC_HATHI_NON.csv")
b$corpus<-c("NON")
#check to see if columns match
which(colnames(a) != colnames(b))
#combine into single data frame
df.train<-rbind(a, b)
df.train<-df.train[,-1]
df.train<-df.train[,-1]
#load test data and relabel corpus
a<-read.csv("LIWC_CONT_NOV.csv")
a$corpus<-c("NOV")
b<-read.csv("LIWC_CONT_NON.csv")
b$corpus<-c("NON")
#check to see if columns match
which(colnames(a) != colnames(b))
#combine into single data frame
df.test<-rbind(a, b)
df.test<-df.test[,-3]
df.test<-df.test[,-1]
#train model
df.model<-ksvm(corpus ~ ., data=df.train, kernel="rbfdot")
#test model
df.pred<-predict(df.model, df.test)
con.matrix<-confusionMatrix(df.pred, df.test$corpus, positive = "NOV")
con.matrix$byClass[[7]]

### 4.3 ###

### Feature Comparison ###
#this script compares each feature between two populations of works
#it uses a rank-sum test to identify which features are significantly different in each population
#it ranks the features according to 2 criteria:
#1. raw increase in the median of feature X in target population compared to the other collection
#2. percentage increase of median of feature X in target population
#all comparison tables are located in 

a<-read.csv("LIWC_CONT_NOV_3P_NoQuotes.csv")
b<-read.csv("LIWC_CONT_HIST_NoQuotes.csv")
which(colnames(a) != colnames(b))
#remove corpus column for Hathi Data
#a<-a[,-ncol(a)]
#b<-b[,-ncol(b)]
sig.df<-NULL
for (i in 3:ncol(a)){    # change integer depending on column structure!!!
  wil.test<-wilcox.test(a[,i], b[,i])
  a.median<-median(a[,i])
  b.median<-median(b[,i])
  wilcox.p<-wil.test[[3]]
  diff<-a.median-b.median
  #per.increase<-abs(((a.median-b.median)/min(a.median, b.median))*100)
  ratio1<-a.median/b.median
  ratio2<-b.median/a.median
  ratio<-max(ratio1, ratio2)
  feature<-colnames(a)[i]
  temp.df<-data.frame(feature, a.median, b.median, ratio, diff, wilcox.p)
  sig.df<-rbind(sig.df, temp.df)
}
sig.final<-sig.df[order(-sig.df[,4]), ]
#sig.final<-sig.final[sig.final[,6] < 0.05,]
#write.csv(sig.final, "LIWC_Comparison_CONT_NOV_3P_v_HIST_NoQuotes.csv")

#compare the canonical collection to the larger Hathi Trust collection
#using random sampling
a<-read.csv("LIWC_19C_EN_NOV_3P.csv")
b<-read.csv("LIWC_HATHI_TALES.csv")
a<-a[,-2]
#a$corpus<-c("EN_FIC")
b<-b[,-2]
which(colnames(a) != colnames(b))
start.col<-3
sig.df<-matrix(0, nrow = (ncol(a)-1)-(start.col-1), ncol = 5)
for (j in 1:100){
  b.samp<-b[sample(nrow(b), nrow(a)),]
  trial.df<-NULL
  for (i in start.col:(ncol(a)-1)){    # change integer depending on column structure!!!
    wil.test<-wilcox.test(a[,i], b.samp[,i])
    a.median<-median(a[,i])
    b.median<-median(b.samp[,i])
    wilcox.p<-wil.test[[3]]
    diff<-a.median-b.median
    #per.increase<-abs(((a.median-b.median)/min(a.median, b.median))*100)
    ratio1<-a.median/b.median
    ratio2<-b.median/a.median
    ratio<-max(ratio1, ratio2)
    feature<-colnames(a)[i]
    temp.df<-data.frame(a.median, b.median, ratio, diff, wilcox.p)
    trial.df<-rbind(trial.df, temp.df)
    trial.matrix<-as.matrix(trial.df)
  }
  sig.df<-sig.df+trial.matrix
}
final.df<-sig.df/100
sig.final<-as.data.frame(final.df)
feature.list<-c(colnames(a)[start.col:(ncol(a)-1)])
sig.final<-cbind(feature.list, sig.final)
sig.final<-sig.final[order(-sig.final$ratio),]
#write.csv(sig.final, file="LIWC_Comparison_EN_NOV_3P_v_HATHI_TALES.csv")

### 4.4 ###
### Transforming tables into wordclouds ###
library(wordcloud)

### Fig. 4.2 ###
#ingest table of interest
#all tables located in "Wordcloud_Tables"

a<-read.csv("Piper_Supplement_Table4.1_LIWC_19C_Fiction.csv")
a$Feature<-tolower(a$Feature)
#make wordcloud
nlevels(a$Category)
basecolors = c("black", "gray50", "gray50", "gray50")
group = as.character(a$Category)
# find position of group in list of groups, and select that matching color...
colorlist = basecolors[ match(group,unique(group)) ]
wordcloud(a$Feature, a$ratio, min.freq = 0, rot.per = 0, random.order = F, ordered.colors=TRUE, colors=colorlist)

### Fig. 4.3 ###
#ingest table of interest
a<-read.csv("Piper_Supplement_Table4.2_LIWC_19C_Fiction_DialogueRemoved.csv")
a$Feature<-tolower(a$Feature)
#remove exclamation marks!
a<-a[-2,]
#make wordcloud
nlevels(factor(a$Category))
basecolors = c("black", "gray50", "black", "gray50", "gray50", "gray50", "gray50")
group = as.character(a$Category)
# find position of group in list of groups, and select that matching color...
colorlist = basecolors[ match(group,unique(group)) ]
wordcloud(a$Feature, a$Ratio, min.freq = 0, rot.per = 0, random.order = F, ordered.colors=TRUE, colors=colorlist)

### 4.5 ###
## Named Entity Comparison ##
#this script compares two data sets for the frequency of named entities
#19C_EN_HIST and 19C_EN_FIC

require("NLP")
library("openNLP")
library("openNLPdata")
library("tm")
filenames<-list.files("19C_EN_HIST")
#filenames<-list.files("19C_EN_FIC")

persons.df<-NULL
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
for (i in 1:length(filenames)) {
  print(i)
  work<-scan(filenames[i], what="character", quote="") # load novel, separate chunks by line breaks
  if (length(work) > 30000) {
    if (length(work) < 250000) {
      word.count<-length(work)
      text.whole<-paste(work, collapse=" ") # collapse into single chunk
      text.char<-as.String(text.whole)
      a2 <- annotate(text.char, list(sent_token_annotator, word_token_annotator))
      entity_annotator <- Maxent_Entity_Annotator(language = "en", kind = "person", probs = FALSE, model = NULL)
      a4<-annotate(text.char, entity_annotator, a2)
      a4w <- subset(a4, type == "entity")
      persons<-text.char[a4w]
      #persons.v<-append(persons.v, persons)
      persons.unique<-unique(persons)
      no.persons<-length(persons.unique)
      no.persons.per10Kword<-(no.persons/word.count)*10000
      temp.df<-data.frame(filenames[i], word.count, no.persons, no.persons.per10Kword)
      persons.df<-rbind(persons.df, temp.df)
    }
  }
}
#write.csv(persons.df, file="NER_19C_EN.csv")
#write.csv(persons.df, file="NER_19C_HIST.csv")

### Fig. 4.4 ###
#ingest tables of interest
a<-read.csv("Piper_Supplement_Table4.3_LIWC_19C_Novels_3P_v_Histories.csv")
a$Feature<-tolower(a$Feature)
#remove exclamation marks!
a<-a[-1,]
#make wordcloud
nlevels(factor(a$Category))
basecolors.a = c("black", "gray50", "black", "gray50", "gray50", "gray50", "gray50")
group.a = as.character(a$Category)
# find position of group in list of groups, and select that matching color...
colorlist.a = basecolors.a[ match(group.a,unique(group.a)) ]

#19C German
b<-read.csv("Piper_Supplement_Table4.4_LIWC_19C_German_NoDialogueRemoved.csv")
b$Feature<-tolower(b$Feature)
#remove exclamation marks
b<-b[-2,]
#make wordcloud
nlevels(factor(b$Category))
basecolors.b = c("gray50", "black", "gray50", "black", "gray50", "gray50")
group.b = as.character(b$Category)
# find position of group in list of groups, and select that matching color...
colorlist.b = basecolors.b[ match(group.b,unique(group.b)) ]

#21C English
c<-read.csv("Piper_Supplement_Table4.5_LIWC_Contemporary_Fiction_DialogueRemoved.csv")
c$Feature<-tolower(c$Feature)
c<-c[1:20,]
c$Category<-factor(c$Category)
#make wordcloud
nlevels(c$Category)
basecolors.c = c("gray50", "black", "black", "gray50", "gray50")
group.c = as.character(c$Category)
# find position of group in list of groups, and select that matching color...
colorlist.c = basecolors.c[ match(group.c,unique(group.c)) ]

#combine all 3
par(mfrow=c(3,1))
wordcloud(a$Feature, a$Ratio, scale=c(3,0.05), min.freq = 0, rot.per = 0, random.order = F, ordered.colors=TRUE, colors=colorlist.a)
wordcloud(b$Feature, b$Ratio, scale=c(4,1.5),min.freq = 0, rot.per = 0, random.order = F, ordered.colors=TRUE, colors=colorlist.b)
wordcloud(c$Feature, c$Ratio, min.freq = 0, rot.per = 0, random.order = F, ordered.colors=TRUE, colors=colorlist.c)

### 4.6 ###
## Compare limited sense perception vocabulary ##
#this script uses a smaller list of perception vocabulary developed by my lab
#compare 19C_EN_NOV_3P_NoQuotes and 19C_EN_HIST_NoQuotes

library("tm")
library("SnowballC")

#load dictionary
dict<-read.csv("dict_sense_stem_all.csv", header=F, stringsAsFactors = F)

#load corpuses for comparison
#read in corpus1
corpus1 <- VCorpus(DirSource("19C_EN_NOV_3P_NoQuotes"), readerControl=list(language="English"))
#clean data
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")
#create document term matrix
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#perform transformations on the raw counts
scaling1<-rowSums(corpus1.matrix) #get total word counts for each work

#read corpus 2
corpus2 <- VCorpus(DirSource("19C_EN_HIST_NoQuotes"), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, content_transformer(removeNumbers))
corpus2 <- tm_map(corpus2, content_transformer(removePunctuation))
corpus2 <- tm_map(corpus2, stemDocument, language = "english")
#create DTM
corpus2.dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
#scale
scaling2<-rowSums(corpus2.matrix) #get total word counts for each work

#subset tables by sense perception verbs and compare samples
corpus1.sub<-corpus1.matrix[,colnames(corpus1.matrix) %in% dict$V1]
corpus2.sub<-corpus2.matrix[,colnames(corpus2.matrix) %in% dict$V1]

#sum and scale values
corpus1.sum<-data.frame(rowSums(corpus1.sub))
corpus2.sum<-data.frame(rowSums(corpus2.sub))
corpus1.scaled<-corpus1.sum/scaling1
corpus2.scaled<-corpus2.sum/scaling2

#check for normality
hist(corpus1.scaled[,1])
shapiro.test(corpus1.scaled[,1])
hist(corpus2.scaled[,1])
shapiro.test(corpus2.scaled[,1])

#rank-sum test
#use sampling to make both samples the same size
wilcox.test(corpus1.scaled[,1], corpus2.scaled[,1])

#compare medians
median(corpus1.scaled[,1])/median(corpus2.scaled[,1])

### Fig. 4.5 ###
#ingest table of interest
#comparing 19C_EN_NOV_3P to 19C_HATHI_TALES
a<-read.csv("Piper_Supplement_Table4.6_LIWC_Novels_3P_v_Hathi_Tales.csv")
a$Feature<-tolower(a$Feature)
#make wordcloud
nlevels(a$Category)
basecolors = c("grey50", "grey50", "black", "gray50", "gray50")
group = as.character(a$Category)
# find position of group in list of groups, and select that matching color...
colorlist = basecolors[ match(group,unique(group)) ]
wordcloud(a$Feature, a$Ratio^2, min.freq = 0, rot.per = 0, random.order = F, ordered.colors=TRUE, colors=colorlist)

### 4.7 ###
## Compare Distinctive Words of 19C_EN_NOV_3P and 19C_HATHI_TALES##
library("tm")
library("SnowballC")

#Step 1: Create a document term matrix for each corpus using a LIWC dictionary

#ingest and clean data
#corpus 1 = novels
corpus1 <- VCorpus(DirSource("19C_EN_NOV_3P", encoding = "UTF-8"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, stemDocument, language = "english") # stem your words
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#corpus 2 = other-fiction
corpus2 <- VCorpus(DirSource("19C_HATHI_TALES", encoding = "UTF-8"), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, content_transformer(removeNumbers))
corpus2 <- tm_map(corpus2, content_transformer(removePunctuation))
corpus2 <- tm_map(corpus2, stemDocument, language = "english") # stem your words
corpus2.dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)

#step 2: subset by dictionaries
#load dictionaries
discrep<-read.csv("dict_discrepancy_stem_english.csv", stringsAsFactors=F, header=F)
insight<-read.csv("dict_insight_stem.csv", stringsAsFactors=F, header=F)
negate<-read.csv("dict_negate_stem_english.csv", stringsAsFactors=F, header=F)
tent<-read.csv("dict_tentative_stem.csv", stringsAsFactors=F, header=F)
#select dictionary
dict<-tent #change accordingly
corpus1.dic<-corpus1.matrix[,colnames(corpus1.matrix) %in% dict$V1]
corpus2.dic<-corpus2.matrix[,colnames(corpus2.matrix) %in% dict$V1]
corpus1.dic<-corpus1.dic[,colnames(corpus1.dic) %in% colnames(corpus2.dic)]
corpus2.dic<-corpus2.dic[,colnames(corpus2.dic) %in% colnames(corpus1.dic)]

#Step 3: identify distinctive words
#this script runs the following tests for distinctiveness
#1. Wilcoxon rank sum test
#2. Fisher's Exact test
#3. Mutual Information
#4. Dunning's G2

#create functions
#mutual information
MI = function(counts){
  N = sum(counts)
  sum(sapply(1:2, function(i) sum(sapply(1:2, function(j)(counts[i,j]/N)*log(counts[i,j]*N/((counts[i,j]+counts[i,3-j])*(counts[i,j]+counts[3-i,j])))))))
}
#Dunnings G2
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}

#prepare data frames
corpus1.dic<-data.frame(corpus1.dic)
corpus2.dic<-data.frame(corpus2.dic)
corpus1.dic$corpus<-c("a")
corpus2.dic$corpus<-c("b")
df<-rbind(corpus1.dic, corpus2.dic)
attach(df)

# Aggregate word count for each group
WC.agg = c(sum(corpus1.matrix), sum(corpus2.matrix))

# Set up results data frame
word = colnames(df)[1:(ncol(df)-1)]
results = data.frame(word = word,
                     freq.a = (colSums(corpus1.dic[,1:(ncol(corpus1.dic)-1)])/WC.agg[1])*100000,
                     freq.b = (colSums(corpus2.dic[,1:(ncol(corpus2.dic)-1)])/WC.agg[2])*100000,
                     med.a = apply(df[corpus=="a",1:(ncol(df)-1)],2, median),
                     med.b = apply(df[corpus=="b",1:(ncol(df)-1)],2, median),
                     wilcox.p = rep(0,length(word)),
                     MI.results = rep(0,length(word)),
                     G1 = rep(0,length(word)),
                     G2 = rep(0,length(word)),
                     fisher.OR = rep(0,length(word)),
                     fisher.p = rep(0, length(word)))

for(j in 1:(length(word)-1)){
  print(j)
  # Integer counts of word occurrences in each text
  freq.counts = df[,j+(ind1-1)]
  
  # Construct contingency table
  n11 = round(sum(freq.counts[corpus=="a"]),digits=0)
  n21 = round(sum(freq.counts[corpus=="b"]),digits=0)
  n12 = WC.agg[1] - n11
  n22 = WC.agg[2] - n21
  cont.table = data.frame(array(c(n11,n12,n21,n22),dim=c(2,2)))
  
  # Dunning's g^2
  LLR = 2*sum(cont.table)*(H(cont.table)-H(rowSums(cont.table))-H(colSums(cont.table)))
  results$G1[j] = LLR
  
  # Fisher Exact  
  # OR estimate = factor by which it is more likely for the word to be in a as it is to be in b
  fish = fisher.test(cont.table)
  results$fisher.OR[j] = fish$estimate
  results$fisher.p[j] = fish$p.value
  
  # Rank-sum test 
  results$wilcox.p[j] = wilcox.test(df[,j+(ind1-1)]~df$corpus)$p.value
  
  results$MI.results[j] = MI(cont.table)
  results$G2[j] = 2*sum(cont.table)*results$MI.results[j]
}
results$med.ratio = results$med.a/results$med.b

#combine each dictionary into a single table
results$dictionary = c("tent")
row.names(results)=NULL
#results.final<-results
results.final<-rbind(results.final, results)
results.df<-results.final[results.final$fisher.p < 0.05,]
results.df<-results.df[results.df$fisher.OR > 1,]
#write.csv(results.df, file="MDW_EN_NOV_3P_v_HATHI_TALES.csv")

### Fig. 4.6 ###
#files located in MDW Directory
a<-read.csv("MDW_Discrepancy.csv")
b<-read.csv("MDW_Insight.csv")
c<-read.csv("MDW_Negate.csv")
d<-read.csv("MDW_Tentative.csv")
par(mfrow=c(2,2))
wordcloud(a$Discrepancy, a$G2, min.freq = 0, rot.per = 0, random.order = F)
wordcloud(b$Insight, a$G2, min.freq = 0, rot.per = 0, random.order = F)
wordcloud(c$Negate, a$G2, min.freq = 0, rot.per = 0, random.order = F)
wordcloud(d$Tentative, a$G2, min.freq = 0, rot.per = 0, random.order = F)

### 4.8 ###
#Measuring changes in novels over time for emotion and sense perception
#data used NOVEL_19C and NOVEL_20C
#for count data load NOVEL_19C_DTM.RData and NOVEL_20C_DTM.RData

#list of functions used to prepare count data
library(slam)
library(tm)
#load dictionaries
sense.dic<-read.csv("dict_sense_stem_all.csv", header=F, stringsAsFactors = F)
sense.dic<-sense.dic$V1
emotion.dic<-read.csv("dict_emotions_stem.csv", header=F, stringsAsFactors = F)
emotion.dic<-emotion.dic$V1
#load corpora
corpus1 <- VCorpus(DirSource("19C_Stanford"), readerControl=list(language="English"))
#corpus1 <- VCorpus(DirSource("KLAB_RANDOM"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
scaling1<-row_sums(corpus1.dtm)

#or load workspaces with DTMs and continue
#NOVEL_19C_DTM.RData and NOVEL_20C_DTM.RData
sense.m<-as.matrix(corpus1.dtm[,which(colnames(corpus1.dtm) %in% sense.dic)])
sense<-rowSums(sense.m)/scaling1
emotion.m<-as.matrix(corpus1.dtm[,which(colnames(corpus1.dtm) %in% emotion.dic)])
emotion<-rowSums(emotion.m)/scaling1
df<-cbind(emotion, sense)

#or start here
#load frequency tables
stan<-read.csv("Emotion_Sense_19C.csv")
klab<-read.csv("Emotion_Sense_20C.csv")

#load metadata
stan.meta<-read.csv("LIWC_19C_STANFORD.csv")
klab.meta<-read.csv("LIWC_20C_KLAB_RANDOM.csv")

#align and clean
stan.sub<-stan[which(as.character(stan$X) %in% as.character(stan.meta$Filename)), ]
stan.sub<-stan.sub[order(as.character(stan.sub$X)),]
stan.meta<-stan.meta[order(as.character(stan.meta$Filename)),]
which(as.character(stan.sub$X) != as.character(stan.meta$Filename))
stan.sub<-cbind(stan.sub, stan.meta$date)
klab.sub<-klab[which(as.character(klab$X) %in% as.character(klab.meta$Filename)), ]
klab.sub<-klab.sub[order(as.character(klab.sub$X)),]
klab.meta<-klab.meta[order(as.character(klab.meta$Filename)),]
which(as.character(klab.sub$X) != as.character(klab.meta$Filename))
klab.sub<-cbind(klab.sub, klab.meta$date)

#combine
colnames(stan.sub)<-c("filename", "emotion", "sense", "date")
colnames(klab.sub)<-c("filename", "emotion", "sense", "date")
all.df<-rbind(stan.sub, klab.sub)
all.df<-all.df[which(all.df$date > 1799),]
#get yearly avgs
final.df<-NULL
for (i in 1:nlevels(factor(all.df$date))){
  sub<-all.df[all.df$date == levels(factor(all.df$date))[i],]
  perception<-mean(sub$sense)*10000
  emotion<-mean(sub$emotion)*10000
  score<-c(perception, emotion)
  type<-c("perception", "emotion")
  year<-sub$date[1]
  temp.df<-data.frame(year, score, type)
  final.df<-rbind(final.df, temp.df)
}

### Fig. 4.7 ###
library(ggplot2)
ggplot(final.df, aes(x=year, y=score, shape=type)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  theme(legend.key = element_rect(fill = NA)) +
  theme(legend.text=element_text(size=12)) +
  geom_point() +
  scale_shape_manual(values=c(17,1)) +
  #geom_line() +
  labs(x="Year", y="Words (Per 10K)", caption="\nFig. 4.7 Frequency of words related to emotions and sense perception\nin English novels, 1800-2000\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)") +
  #labs(x="Year", y="Words (Per 10K)") +
  stat_smooth(method=loess, colour="black")

#test correlation between txtLAB emotion dictionary and NRC
klab.txt<-read.csv("Emotion_Sense_20C.csv")
stan.txt<-read.csv("Emotion_Sense_19C.csv")
klab.nrc<-read.csv("Emotion_NRC_20C.csv")
stan.nrc<-read.csv("Emotion_NRC_19C.csv")
#check alignment
which(as.character(stan.nrc$X) != as.character(stan.txt$X))
which(as.character(klab.nrc$X) != as.character(klab.txt$X))
#check correlation between dictionaries
cor.test(stan.nrc$x, stan.txt$emotion)
cor.test(klab.nrc$x, klab.txt$emotion)
plot(stan.nrc$x, stan.txt$emotion, xlim=c(0.05, 0.15))
plot(klab.nrc$x, klab.txt$emotion)
#combined
cor.test(c(stan.nrc$x,klab.nrc$x), c(stan.txt$emotion, klab.txt$emotion))
plot(c(stan.nrc$x,klab.nrc$x), c(stan.txt$emotion, klab.txt$emotion), xlim=c(0.05, 0.15))


### 4.9 ###
### Comparing 21C Novel to 19C Novel ###
a<-read.csv("LIWC_CONT_NOV.csv")
b<-read.csv("LIWC_19C_STANFORD.csv")
which(colnames(a) != colnames(b))
b<-b[,-1]
b<-b[,-ncol(b)]
a<-a[,-1]
a<-a[,-1]
which(colnames(a) != colnames(b))
sig.df<-matrix(0, nrow = ncol(a), ncol = 5)
for (j in 1:100){
  b.samp<-b[sample(nrow(b), nrow(a)),]
  trial.df<-NULL
  for (i in 1:ncol(a)){    # change integer depending on column structure!!!
    wil.test<-wilcox.test(a[,i], b.samp[,i])
    a.median<-median(a[,i])
    b.median<-median(b.samp[,i])
    wilcox.p<-wil.test[[3]]
    diff<-a.median-b.median
    #per.increase<-abs(((a.median-b.median)/min(a.median, b.median))*100)
    ratio1<-a.median/b.median
    ratio2<-b.median/a.median
    ratio<-max(ratio1, ratio2)
    feature<-colnames(a)[i]
    temp.df<-data.frame(a.median, b.median, ratio, diff, wilcox.p)
    trial.df<-rbind(trial.df, temp.df)
    trial.matrix<-as.matrix(trial.df)
  }
  sig.df<-sig.df+trial.matrix
}
final.df<-sig.df/100
sig.final<-as.data.frame(final.df)
feature.list<-c(colnames(a)[1:ncol(a)])
sig.final<-cbind(feature.list, sig.final)
for (i in 1:nrow(sig.final)){
  if(sig.final$diff[i] < 0){sig.final$ratio[i]<--sig.final$ratio[i]}
}
sig.final<-sig.final[order(-sig.final$ratio),]



