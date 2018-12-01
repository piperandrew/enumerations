############ CHAPTER 2 ################
library("stringr")
library("tm")
library("SnowballC")
library("proxy")
library("cluster")
library("ggplot2")

### 2.1 ###

###### Augustine's Confessions #######
#this test explores the degree to which Augustine's Confessions  significantly expands or contracts its
#vocabulary over the course of the narrative
#load corpus
corpus1 <- VCorpus(DirSource("AugustineLatinChapter"), readerControl=list(language="Latin"))
#run transformations
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
scaling1<-rowSums(corpus1.matrix)
#remove Latin stopwords
words<-c("a", "ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero")
corpus1 <- tm_map(corpus1, removeWords, words)
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
#remove sparse terms
corpus1.dtm.sparse<-removeSparseTerms(corpus1.dtm, .4)
corpus1.sparse.matrix<-as.matrix(corpus1.dtm.sparse, stringsAsFactors=F)
#scale
corpus.scaled<-corpus1.sparse.matrix/scaling1
#create distance matrix
corpus.dist<-dist(corpus.scaled, method = "Euclidean")

### Fig. 2.1 ###
#artifically set clusters around pre- and post-conversion books
cluster.no<-c(1,2,2,2,2,1,1,1,1,1,1,1,2)
clusplot(corpus.dist, clus=cluster.no, axes=FALSE,diss=TRUE, xlab="",ylab="", labels=3, lines = 0, main="", span=TRUE, color=FALSE, sub=NULL, add=FALSE, col.clus="black", col.p="black", font.main=1, lty=c(1,2), cex=0)
title(main="Fig. 2.1 Semantic relationships between the thirteen\nbooks of Augustine's Confessions", sub="Source: Andrew Piper, Enumerations:\nData and Literary Study (2018)")
Axis(side=2, at=seq(-0.010,0.010, by=0.005), labels=c(-0.010, -0.005, 0.000, 0.005, 0.010))
Axis(side=1, at=seq(-0.010,0.015, by=0.005), labels=c(-0.010, -0.005, 0.000, 0.005, 0.010, 0.015))
box()

### 2.2 ###

##### Contraction Test #####
#this script measures the degree of lexical contraction in a novel
#it does so by measuring the average dissimilarity of all parts of the first half of a novel
#to each other and comparing that distribution to all parts of the second half of a novel
#it uses the non-parametric wilcoxon rank sum test to test whether the difference in in-group
#dissimilarity is significantly greater or smaller between the two halves
#it then provides the percentage change of the median in-group dissimilarities relative to each other, so that one can tell
#by how much a novel contracts/expands its vocabulary relative to other novels
#higher negative numbers refer to greater lexical contraction, i.e. less dissimilarity and greater similarity between the parts of
#the second half when compared to the parts of the first half
#i.e. a score of 0.25 = a 25% increase in dissimilarity between the first and second halves
#while -0.25 = a 25% decrease in dissimilarity, or a 25% increase in lexical similarity in the second half

### Notes: ### 
#1. the script takes as input a directory of novels that have been divided into 20 equal parts
#and stored into separate directories. You can use the accompanying python script twenty_directory.py to perform this. Or you
#can use the accompanying data labeled txtlab_Novel450_German20, etc. located in the supplementary data
#2. stopwords are removed and only those words that appear in a majority of the novel's parts are included
#3. word counts are normalized by scaling relative to the length of the novel
#4. the results show no correlation between novel length and contraction score for German and French;
#for English, there is some inverse correlation (-0.35), suggesting that longer novels tend to be more contractive 
#5. the script needs to be run separately for different languages -- make sure to change
#language for TM package 3x -- 1x each for a) language of texts, b) stemming, and c) stopword lists
final.df<-NULL
for (k in 1:150) {
  print(k)
  #insert your own directory
  dir <- paste("[INSERT DIRECTORY HERE]", as.character(k), sep="")
  #load corpus
  corpus1 <- VCorpus(DirSource(dir), readerControl=list(language="English")) #CHANGE LANGUAGE
  #run transformations
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  #remove problems per language
  #problems<-c("apparat","datumsangaben","seite","page","erläuterungen", "kommentar", "kapitel")
  problems<-c("chapter")
  #problems<-c("a", "des")
  corpus1 <- tm_map(corpus1, removeWords, problems)
  #get scaling value
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  scaling1<-rowSums(corpus1.matrix)
  #remove stopwords
  corpus1 <- tm_map(corpus1, removeWords, stopwords("en")) #CHANGE LANGUAGE
  #stem
  corpus1 <- tm_map(corpus1, stemDocument, language = "english") #CHANGE LANGUAGE
  #remake DTM
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  #remove sparse terms
  corpus1.dtm.sparse<-removeSparseTerms(corpus1.dtm, .4)
  corpus1.sparse.matrix<-as.matrix(corpus1.dtm.sparse, stringsAsFactors=F)
  #scale
  conversion.scaled<-corpus1.sparse.matrix/scaling1
  #this remakes the dtm in proper order
  nword=dim(conversion.scaled)[2]
  freq.dat<-array(0,c(20,nword))
  freq.dat[1,]=conversion.scaled[1,]
  freq.dat[2,]=conversion.scaled[12,]
  for (j in 3:9){
    freq.dat[j,]=conversion.scaled[11+j,]
  }
  for (j in 10:19){
    freq.dat[j,]=conversion.scaled[j-8,]
  }
  freq.dat[20,]=conversion.scaled[13,]
  #create distance table
  conversion.dist<-dist(freq.dat, method = "Euclidean")
  freq.dist<-as.matrix(conversion.dist)
  #create groups for 1st and 2nd halves
  group1<-vector()
  for (m in 1:9) {
    for (n in m:9)
      group1<-append(group1, freq.dist[m,n+1])
  }
  group2<-vector()
  for (m in 11:19) {
    for (n in m:19)
      group2<-append(group2, freq.dist[m,n+1])
  }
  #comparing in-group similarities using wilcoxon test
  sim.wil<-wilcox.test(group1, group2)
  #extract p-value of the test
  in.diff.p<-sim.wil$p.value
  #calculate percentage change of median value of each half
  #subtract the first half from the second half and divide by the first half
  in.diff<-(summary(group2)[[3]]-summary(group1)[[3]])/summary(group1)[[3]]
  #calculate the absolute value, i.e. ignore whether the novel contracts or expands, just observe change
  in.diff.abs<-abs(in.diff)
  #get word count of the novel
  word.count<-sum(scaling1)
  filename<-row.names(corpus1.matrix)[1]
  temp.df<-data.frame(k, filename, word.count, in.diff, in.diff.abs, in.diff.p)
  final.df<-rbind(final.df, temp.df)
}
#get z.score for each novel relative to its language group for both the regular and absolute values
final.df$in.z<-scale(final.df$in.diff, center = T, scale=T)
final.df$in.abs.z<-scale(final.df$in.diff.abs, center = T, scale=T)
#rerun for each language and append tables to each other

#final table stored as:
#Contraction_All_Trial1.csv


#### Fig. 2.2 ####
#set k = the number of the novel
k=114
dir <- paste("[INSERT DIRECTORY]", as.character(k), sep="")
corpus1 <- VCorpus(DirSource(dir), readerControl=list(language="German")) #CHANGE LANGUAGE
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
problems<-c("apparat","datumsangaben","seite","page","erläuterungen", "kommentar", "kapitel")
corpus1 <- tm_map(corpus1, removeWords, problems)
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
scaling1<-rowSums(corpus1.matrix)
corpus1 <- tm_map(corpus1, removeWords, stopwords("de")) #CHANGE LANGUAGE
corpus1 <- tm_map(corpus1, stemDocument, language = "german") #CHANGE LANGUAGE
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.dtm.sparse<-removeSparseTerms(corpus1.dtm, .4)
corpus1.sparse.matrix<-as.matrix(corpus1.dtm.sparse, stringsAsFactors=F)
contraction.scaled<-corpus1.sparse.matrix/scaling1
nword=dim(contraction.scaled)[2]
freq.dat<-array(0,c(20,nword))
freq.dat[1,]=contraction.scaled[1,]
freq.dat[2,]=contraction.scaled[12,]
for (j in 3:9){
  freq.dat[j,]=contraction.scaled[11+j,]
}
for (j in 10:19){
  freq.dat[j,]=contraction.scaled[j-8,]
}
freq.dat[20,]=contraction.scaled[13,]
contraction.dist<-dist(freq.dat, method = "Euclidean")
#artificially set clusters as first and second halves of a novel
cluster.no<-c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2)
clusplot(contraction.dist, clus=cluster.no, axes=FALSE, diss=TRUE, xlab="",ylab="", labels=0, lines = 0, main="", span=TRUE, color=FALSE, sub=NULL, add=FALSE, col.clus="black", col.p="black", font.main=1, lty=c(1,2))
title(main="Fig. 2.2 Theodore Fontane, Irrungen,Wirrungen", sub="Source: Andrew Piper, Enumerations:\nData and Literary Study (2018)")
legend("topright", c("Half1", "Half2"), pch=c(1,2), box.col="white")
Axis(side=2, at=seq(-0.005,0.005, by=0.005), labels=c(-0.005, 0.000, 0.005))
Axis(side=1, at=seq(-0.02,0.01, by=0.010), labels=c(-0.020, -0.010, 0.000, 0.010))
box()

### 2.3 ###

#### Identify distinctive words for a given novel ####
## All outputs located in MDW directory
k=114 #set which novel
dir <- paste("[INSERT DIRECTORY]", as.character(k), sep="")
corpus1 <- VCorpus(DirSource(dir), readerControl=list(language="French")) #CHANGE LANGUAGE
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
#problems<-c("apparat","datumsangaben","seite","page","erläuterungen", "kommentar", "kapitel")
#problems<-c("chapter") #English
problems<-c("a", "des") #French
corpus1 <- tm_map(corpus1, removeWords, problems)
corpus1 <- tm_map(corpus1, removeWords, stopwords("fr")) #CHANGE LANGUAGE
corpus1 <- tm_map(corpus1, stemDocument, language = "french") #CHANGE LANGUAGE
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.dtm.sparse<-removeSparseTerms(corpus1.dtm, .6)
corpus1.sparse.matrix<-as.matrix(corpus1.dtm.sparse, stringsAsFactors=F)
conversion.scaled<-corpus1.sparse.matrix
nword=dim(conversion.scaled)[2]
freq.dat<-array(0,c(20,nword))
freq.dat[1,]=conversion.scaled[1,]
freq.dat[2,]=conversion.scaled[12,]
for (j in 3:9){
  freq.dat[j,]=conversion.scaled[11+j,]
}
for (j in 10:19){
  freq.dat[j,]=conversion.scaled[j-8,]
}
freq.dat[20,]=conversion.scaled[13,]
colnames(freq.dat)<-colnames(conversion.scaled)
#run Fisher's exact test to find distinctive words of second half
word1=colSums(freq.dat[11:20,])
word2=colSums(freq.dat[1:10,])
all1=sum(word1)
all2=sum(word2)
fisher.df<-data.frame(word=colnames(freq.dat), half2=word1, half1=word2, fish.odds=0, fish.p=0)
for (i in 1:ncol(freq.dat)){
  cont.table<-data.frame(c(word1[i], all1-word1[i]), c(word2[i], all2-word2[i]))
  fish<-fisher.test(cont.table)
  fisher.df[i, c("fish.odds","fish.p")]<-c(fish$estimate[[1]], fish$p.value)
}
#remove insignificant words
fisher.final<-fisher.df[fisher.df$fish.p < 0.05,]
fisher.final$diff<-fisher.final$half2-fisher.final$half1
fisher.final<-fisher.final[order(-fisher.final$diff),]
#write.csv(fisher.final, file="MDW_AntonReiser.csv", row.names = F)

### 2.4 ###

##### Social Network Analysis #####
#this observes the social relationships between characters by studying their co-occurrence per page.
#this takes as input a directory of a novel that has been divided into page-units
#it generates a table of character co-occurrences per text window
#and calculates the social network in fourths or halves

#create a list of entities
chars<-c("lene", "botho", "dörr", "nimptsch", "käthe", "schloß", "balafré", "franke", "pitt", "serge", "isabeau", "johanna", "margot", "balkon", "garten")
#ingest your novel
corpus1 <- VCorpus(DirSource("Fontane_250"), readerControl=list(language="German"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
corpus.sub<-corpus1.matrix[,colnames(corpus1.matrix) %in% chars]
#attach characters who appear together on the same page
final.df<-NULL
for (i in 1:nrow(corpus.sub)){
  sub<-as.data.frame(corpus.sub[i,])
  sub<-subset(sub, sub$`corpus.sub[i, ]` > 0)
  if (nrow(sub) > 1) {
  for (j in 1:(nrow(sub)-1)){
    for (k in (j+1):nrow(sub)){
      source<-row.names(sub)[j]
      target<-row.names(sub)[k]
      temp.df<-data.frame(source, target)
      final.df<-rbind(final.df, temp.df)
    }
  }
  }
}
#divide the work into N parts to observe changes in the social network over time
#this writes each section as a separate weighted edge list
n=4
sec.length<-floor(nrow(final.df)/n)
start<-1
for (i in 1:n){
  sec<-final.df[start:(i*sec.length),]
  sec.t<-data.frame(table(sec))
  colnames(sec.t)<-c("source", "target", "weight")
  sec.t$type<-c("undirected")
  sec.t<-sec.t[sec.t$weight !=0,]
  file.id<-paste("Fontane", "_", i, ".csv", sep="", collapse=NULL)
  write.csv(sec.t, file=file.id)
  start<-(i*sec.length)+1
  #g<-graph.data.frame(sec.t[,1:3], directed=FALSE, vertices=NULL)
  #plot.igraph(g, edge.width=sec.t$weight)
}

### Fig. 2.3 ###
#social networks for Fontane's Irrungen,Wirrungen
library(igraph)
#Q1
edge1<-read.csv("Fontane_1.csv", header=T)
edge.sub1<-edge1[,2:3]
g1<-graph.data.frame(edge.sub1, directed=FALSE, vertices=NULL)
E(g1)$weight<-edge1$weight
#Q2
edge2<-read.csv("Fontane_2.csv", header=T)
edge.sub2<-edge2[,2:3]
g2<-graph.data.frame(edge.sub2, directed=FALSE, vertices=NULL)
E(g2)$weight<-edge2$weight
#Q3
edge3<-read.csv("Fontane_3.csv", header=T)
edge.sub3<-edge3[,2:3]
g3<-graph.data.frame(edge.sub3, directed=FALSE, vertices=NULL)
E(g3)$weight<-edge3$weight
#Q4
edge4<-read.csv("Fontane_4.csv", header=T)
edge.sub4<-edge4[,2:3]
g4<-graph.data.frame(edge.sub4, directed=FALSE, vertices=NULL)
E(g4)$weight<-edge4$weight

#plot using GEM layout
#plot as single figure
par(mfrow=c(2,2))
plot.igraph(g1, layout=layout_with_gem, edge.width=E(g1)$weight/2,vertex.size=rep(30,length(V(g1))),
            edge.color="black", vertex.color="white", vertex.label.color="black")
text(0,-1.5, "Q1")
plot.igraph(g2, layout=layout_with_gem, edge.width=E(g2)$weight/2,vertex.size=rep(30,length(V(g2))),
            edge.color="black", vertex.color="white", vertex.label.color="black")
text(0,-1.5, "Q2")
plot.igraph(g3, layout=layout_with_gem, edge.width=E(g3)$weight/2,vertex.size=rep(30,length(V(g3))),
            edge.color="black", vertex.color="white", vertex.label.color="black")
text(0,-1.5, "Q3")
plot.igraph(g4, layout=layout_with_gem, edge.width=E(g4)$weight/2,vertex.size=rep(35,length(V(g4))),
            edge.color="black", vertex.color="white", vertex.label.color="black")
text(0,-1.5, "Q4")
title(main="Fig. 2.3 Social networks of Fontane's Irrungen,Wirrungen", sub="Source: Andrew Piper, Enumerations:\nData and Literary Study (2018)")

### 2.5 ###
### TTR ###
#this method takes 100 windows of 1,000 continous words and calculates the avg. TTR score for a given novel
#this method shows little correlation between TTR score and work length (r = 0.027)
#derived data = TTR_1000_Modern_Sample.csv

#select directory to grab filenames from
filenames<-list.files()
s.size<-999 #adjust for segment size
ttr.df<-NULL
for (i in 1:length(filenames)) {
  print(i)
  #load text & clean
  work<-scan(filenames[i], what="character", quote="")
  #require novels to be more than 35K words
  if (length(work) > 35000){ 
  work.clean<- gsub("\\d", "", work)
  work.clean<-gsub("\\W", "", work)
  work.clean<- tolower(work.clean)
  work.clean<-work.clean[work.clean != ""]
  ttr.v<-vector()
  for (j in 1:100) {
    beg<-sample(1:(length(work.clean)-s.size),1)
    test<-work.clean[beg:(beg+s.size)]
    ttr.sample<-length(unique(test))/length(test)
    ttr.v<-append(ttr.v, ttr.sample)
  }
  avg.ttr<-mean(ttr.v)
  med.ttr<-median(ttr.v)
  sd.ttr<-sd(ttr.v)
  work.name<-as.character(filenames[i])
  word.count<-length(work.clean)
  temp.df<-data.frame(work.name, word.count, avg.ttr, sd.ttr)
  ttr.df<-rbind(ttr.df, temp.df)
  }
}

#test to see if length correlates w TTR ratio
#if there is a positive or negative correlation then results aren't valid
#it means you are tracking length not anything specific to the novel
cor.test(ttr.df$word.count, ttr.df$avg.ttr)

### Fig. 2.4 ###
require(scales)
corpus<-read.csv("TTR_1000_Modern_Sample.csv")
ggplot(corpus, aes(x=avg.ttr, y=word.count)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.position="none") +
  geom_point(size=1, position=position_jitter(w = NULL, h = 0.1), alpha=0.5) +
  geom_text(aes(label=Label), size=3, hjust=0.6, vjust=-0.5) +
  xlim(c(.30, .60)) +
  scale_y_continuous(limits=c(35000, 350000), labels = comma) +
  geom_vline(xintercept = (corpus$avg.ttr[corpus$Label=="Ulysses"]-0.001), lty = 3) +
  labs(x="Type-Token Ratio", y="Word Count", caption="\nFig. 2.4 Type-Token Ratio in 1,000 novels published between 1880-1930\n\nSource: Andrew Piper, Enumerations:\nData and Literary Study (2018)")
  #labs(x="Type-Token Ratio", y="Word Count")

#inspect novels with greater TTR than Ulysses
which(corpus$Label == "Ulysses")
corpus[267,]
corpus.sub<-corpus[corpus$avg.ttr > 0.49906,]

### 2.6 ###
#### TTR over time ###
#this script takes as input a novel broken into smaller units and reports the type-token ratio over novel time
filenames<-list.files("Joyce_Ulysses_1922_Edition_Chunk_500", pattern="*.txt", full.names=FALSE)
ttr.df<-NULL
segment<-0
for (i in 1:length(filenames)) {
  #load text & clean
  work<-scan(filenames[i], what="character", quote="")
  work.clean<- gsub("\\d", "", work)
  work.clean<-gsub("\\W", "", work)
  work.clean<- tolower(work.clean)
  work.clean<-work.clean[work.clean != ""]
  ttr<-length(unique(work.clean))/length(work.clean)
  segment<-segment+(length(work))
  temp.df<-data.frame(segment, ttr)
  ttr.df<-rbind(ttr.df, temp.df)
}

### Calculate synonymy per page ###
#synonymy scores derived using the script calc_synonyms_antonyms.py

### Fig. 2.5 ###
#plot TTR and Synonymy over novel time in Joyce
library("zoo")
joyce<-read.csv("Joyce_TTR_Synonymy.csv")
#normalize to plot on the same graph
joyce$ttr.norm<-scale(joyce$ttr, center=T, scale=T)
joyce$syn.norm<-scale(joyce$syn2, center=T, scale=T)
joyce$ttr.rollmean<-rollmean(joyce$ttr.norm, k=20, na.pad=T)
joyce$syn.rollmean<-rollmean(joyce$syn.norm, k=20, na.pad=T)
df1<-data.frame(joyce$X, joyce$ttr.rollmean, c("Vocab Richness"))
df2<-data.frame(joyce$X, joyce$syn.rollmean,c("Synonymy"))
colnames(df1)<-c("segment", "value", "score")
colnames(df2)<-c("segment", "value", "score")
df3<-rbind(df1, df2)
#df3[is.na(df3)]<-0
ggplot(df3, aes(x=segment, y=value)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line(aes(linetype=score)) +
  scale_x_continuous(labels=comma) +
  labs(x="Pages", y="Scale", caption="\nFig. 2.5 Relationship between vocabulary richness and synonymy\nin James Joyce's Ulysses\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)") +
  #labs(x="Pages in Ulysses", y="Scale") +
  theme(legend.position="top", legend.title=element_blank())

#to plot correlation between TTR and Synonymy
cor.test(joyce$ttr, joyce$syn2)

### 2.7 ###
#test correlation w random pages between TTR and Synonymy
a<-read.csv("Joyce_TTR_Synonymy_10000_Page_Sample.csv")
cor.test(a$synonymy, a$ttr)

### 2.8 ###

#### MODEL 2 ####
#this trial removes all proper names in a 2 step process
#this takes two dictionaries
#the first is a dictionary of words for each language that are present in 60% of all novels for that language
#this is the base dictionary and gets rid of most proper names
#the second is a list of proper names and general character names that are removed and can be added to as need be
#correlation between length and score for German and French is insignificant, slight for English (-.19)
#all dictionaries are located in "ProperNamesDictionaries"
dict0<-read.csv("Dict_German_NovelWords_3000.csv", header=F, stringsAsFactors = F)
dict3<-read.csv("Dict_German_NamesPlus.csv", header=F, stringsAsFactors = F)
final.df<-NULL
for (k in 3:152) {
  print(k)
  dir <- paste("[INSERT DIRECTORY]", as.character(k), sep="")
  #load corpus
  corpus1 <- VCorpus(DirSource(dir), readerControl=list(language="German")) #CHANGE LANGUAGE
  #run transformations
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  #remove problems
  problems<-c("apparat","datumsangaben","seite","page","erläuterungen", "kommentar", "kapitel", "mutter", "vater")
  #problems<-c("chapter", "mother", "father")
  #problems<-c("a", "des", "mère", "père")
  corpus1 <- tm_map(corpus1, removeWords, problems)
  #get scaling value
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  scaling1<-rowSums(corpus1.matrix)
  #remove stopwords
  corpus1 <- tm_map(corpus1, removeWords, stopwords("de")) #CHANGE LANGUAGE
  #remake DTM
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  #keep only words in general dictionary
  corpus1.sub<-corpus1.dtm[,colnames(corpus1.dtm) %in% dict0$V1]
  #keep top 1000 terms
  keep.freqs<-sort(colSums(as.matrix(corpus1.sub)), decreasing = T)[1:1000]
  corpus1.matrix<-as.matrix(corpus1.sub[, colnames(corpus1.sub) %in% names(keep.freqs)])
  #remove proper names
  corpus1.matrix<-corpus1.matrix[, !colnames(corpus1.matrix) %in% dict3$V1]
  #scale
  conversion.scaled<-corpus1.matrix/scaling1
  nword=dim(conversion.scaled)[2]
  freq.dat<-array(0,c(20,nword))
  freq.dat[1,]=conversion.scaled[1,]
  freq.dat[2,]=conversion.scaled[12,]
  for (j in 3:9){
    freq.dat[j,]=conversion.scaled[11+j,]
  }
  for (j in 10:19){
    freq.dat[j,]=conversion.scaled[j-8,]
  }
  freq.dat[20,]=conversion.scaled[13,]
  #create distance table
  conversion.dist<-dist(freq.dat, method = "Euclidean")
  freq.dist<-as.matrix(conversion.dist)
  #create groups for 1st and 2nd halves
  group1<-vector()
  for (m in 1:9) {
    for (n in m:9)
      group1<-append(group1, freq.dist[m,n+1])
  }
  group2<-vector()
  for (m in 11:19) {
    for (n in m:19)
      group2<-append(group2, freq.dist[m,n+1])
  }
  #comparing in-group similarities
  sim.wil<-wilcox.test(group1, group2)
  in.diff.p<-sim.wil$p.value
  #compares ratio of in-group medians of dissimilarity
  in.diff<-(summary(group2)[[3]]-summary(group1)[[3]])/summary(group1)[[3]]
  in.diff.abs<-abs(in.diff)
  word.count<-sum(scaling1)
  filename<-row.names(corpus1.matrix)[1]
  temp.df<-data.frame(k, filename, word.count, in.diff, in.diff.abs, in.diff.p)
  final.df<-rbind(final.df, temp.df)
}
final.df$in.z<-scale(final.df$in.diff, center = T, scale=T)
final.df$in.abs.z<-scale(final.df$in.diff.abs, center = T, scale=T)

### 2.9 ###
#discover distinctive words between the first and second halves using a Fisher's Exact Test
dict0<-read.csv("Dict_English_NovelWords_3000.csv", header=F, stringsAsFactors = F)
dict3<-read.csv("Dict_English_NamesPlus.csv", header=F, stringsAsFactors = F)
#set k to number of novel
k=85
dir <- paste("[INSERT DIRECTORY]", as.character(k), sep="")
corpus1 <- VCorpus(DirSource(dir), readerControl=list(language="English")) #CHANGE LANGUAGE
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
#remove problems
#problems<-c("apparat","datumsangaben","seite","page","erläuterungen", "kommentar", "kapitel", "mutter", "vater")
problems<-c("chapter", "mother", "father")
#problems<-c("a", "des", "mère", "père")
corpus1 <- tm_map(corpus1, removeWords, problems)
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
scaling1<-rowSums(corpus1.matrix)
corpus1 <- tm_map(corpus1, removeWords, stopwords("en")) #CHANGE LANGUAGE
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.sub<-corpus1.dtm[,colnames(corpus1.dtm) %in% dict0$V1]
keep.freqs<-sort(colSums(as.matrix(corpus1.sub)), decreasing = T)[1:1000]
corpus1.matrix<-as.matrix(corpus1.sub[, colnames(corpus1.sub) %in% names(keep.freqs)])
corpus1.matrix<-corpus1.matrix[, !colnames(corpus1.matrix) %in% dict3$V1]
#turn scaling on/off for Fisher's (on)
#conversion.scaled<-corpus1.matrix/scaling1
conversion.scaled<-corpus1.matrix
nword=dim(conversion.scaled)[2]
freq.dat<-array(0,c(20,nword))
freq.dat[1,]=conversion.scaled[1,]
freq.dat[2,]=conversion.scaled[12,]
for (j in 3:9){
  freq.dat[j,]=conversion.scaled[11+j,]
}
for (j in 10:19){
  freq.dat[j,]=conversion.scaled[j-8,]
}
freq.dat[20,]=conversion.scaled[13,]
conversion.dist<-dist(freq.dat, method = "Euclidean")
colnames(freq.dat)<-colnames(conversion.scaled)
#run Fisher's exact test to find distinctive words of second half
word1=colSums(freq.dat[11:20,])
word2=colSums(freq.dat[1:10,])
all1=sum(word1)
all2=sum(word2)
fisher.df<-data.frame(word=colnames(freq.dat), half2=word1, half1=word2, fish.odds=0, fish.p=0)
for (i in 1:ncol(freq.dat)){
  cont.table<-data.frame(c(word1[i], all1-word1[i]), c(word2[i], all2-word2[i]))
  fish<-fisher.test(cont.table)
  fisher.df[i, c("fish.odds","fish.p")]<-c(fish$estimate[[1]], fish$p.value)
}
#remove insignificant words
fisher.final<-fisher.df[fisher.df$fish.p < 0.05,]
fisher.final$diff<-fisher.final$half2-fisher.final$half1
fisher.final<-fisher.final[order(-fisher.final$diff),]

### 2.10 ###
#inspect Woolf's Mrs Dalloway for proper names
#this script extracts proper names for the two halves of Woolf's Mrs Dalloway
#the question is whether there are more frequent use of proper names in the second half versus the first
library("NLP")
library("openNLP")
library("openNLPdata")
library(tm)

#restart R to get this to work. It undoes other packages masking certain functions.
##Loop for extracting POS for directory of works and keeping only words of a certain POS
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en")
work<-scan("EN_1925_Woolf,Virginia_Mrs.Dalloway_Novel.txt", what="character", quote="")
prop.v<-vector()
for (i in seq(1, (length(work)-1000), 100)) {
  print(i)
  #load section of the novel
  sub<-work[i:(i+999)]
  #clean
  work.clean<- gsub("\\d", "", sub)
  #collapse into a single chunk
  text.whole<-paste(work.clean, collapse=" ")
  text.char<-as.String(text.whole)
  #POS tag
  a2 <- annotate(text.char, list(sent_token_annotator, word_token_annotator))
  a3 <- annotate(text.char, pos_tag_annotator, a2)
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  tags.keep<-grep("NNP", tags)
  percent.proper.names<-length(tags.keep)/length(tags)
  prop.v<-append(prop.v, percent.proper.names)
}
library("zoo")
#take rolling mean for window of 5,000 words
test<-rollmean(prop.v, 10)
test.df<-data.frame(test)

### Fig. 2.6 ####
library(ggplot2)
ggplot(test.df, aes(x=as.numeric(row.names(test.df)), y=test.df$test*100)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  labs(x="Narrative Progression", y="Percentage", caption="\nFig. 2.6 Percentage of Proper Names in Woolf's Mrs. Dalloway\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)") +
  #labs(x="Narrative Progression", y="Percentage") +
  geom_vline(xintercept=floor(nrow(test.df)/2), lty=3)

#test for significance between the first and second halves of the novel.
#first test for normality
hist(prop.v[1:(length(prop.v)/2)])
shapiro.test(prop.v[1:(length(prop.v)/2)])
hist(prop.v[(length(prop.v)/2):length(prop.v)])
shapiro.test(prop.v[(length(prop.v)/2):length(prop.v)])
#because the data is not normal
wilcox.test(prop.v[1:(length(prop.v)/2)], prop.v[(length(prop.v)/2):length(prop.v)])
median(prop.v[1:(length(prop.v)/2)])
median(prop.v[(length(prop.v)/2):length(prop.v)])
#to visualize (not used)
boxplot(prop.v[1:(length(prop.v)/2)], prop.v[(length(prop.v)/2):length(prop.v)])

### Fig. 2.7 ###
#plot an individual novel in this case Henry James, Portrait of a Lady
library("tm")
library("SnowballC")
library("cluster")
library("proxy")

dict0<-read.csv("Dict_English_NovelWords_3000.csv", header=F, stringsAsFactors = F)
dict3<-read.csv("Dict_English_NamesPlus.csv", header=F, stringsAsFactors = F)
#setwd()
corpus1 <- VCorpus(DirSource("James_Portrait_20"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
#remove problems
problems<-c("chapter", "mother", "father")
corpus1 <- tm_map(corpus1, removeWords, problems)
#get scaling value
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
scaling1<-rowSums(corpus1.matrix)
#remove stopwords
corpus1 <- tm_map(corpus1, removeWords, stopwords("en"))
#remake DTM
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
#keep only words in general dictionary
corpus1.sub<-corpus1.dtm[,colnames(corpus1.dtm) %in% dict0$V1]
#keep top 1000 terms
keep.freqs<-sort(colSums(as.matrix(corpus1.sub)), decreasing = T)[1:1000]
corpus1.matrix<-as.matrix(corpus1.sub[, colnames(corpus1.sub) %in% names(keep.freqs)])
#remove proper names
corpus1.matrix<-corpus1.matrix[, !colnames(corpus1.matrix) %in% dict3$V1]
#scale
conversion.scaled<-corpus1.matrix/scaling1
nword=dim(conversion.scaled)[2]
freq.dat<-array(0,c(20,nword))
freq.dat[1,]=conversion.scaled[1,]
freq.dat[2,]=conversion.scaled[12,]
  for (j in 3:9){
    freq.dat[j,]=conversion.scaled[11+j,]
  }
  for (j in 10:19){
    freq.dat[j,]=conversion.scaled[j-8,]
  }
  freq.dat[20,]=conversion.scaled[13,]
  #create distance table
conversion.dist<-dist(freq.dat, method = "Euclidean")
#plot
cluster.no<-c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2)
clusplot(conversion.dist, clus=cluster.no,  axes=FALSE, diss=TRUE, xlab="",ylab="", labels=0, lines = 0, main="", span=TRUE, color=FALSE, sub=NULL, add=FALSE, col.clus="black", col.p="black", font.main=1, lty=c(1,2))
title(main="Fig. 2.7 Henry James, Portrait of a Lady", sub="Source: Andrew Piper, Enumerations:\nData and Literary Study (2018)")
legend("topright", c("Half1", "Half2"), pch=c(1,2), box.col="white")
Axis(side=2, at=seq(-0.002,0.002, by=0.002), labels=c(-0.002, 0.000, 0.002))
Axis(side=1, at=seq(-0.002,0.002, by=0.002), labels=c(-0.002, 0.000, 0.002))
box()

### 2.11 ###
## Foote Novelty ##
#this script looks for maximum moments of change in a 4-chapter window across novels
#chapters are regularized into 20 equal-sized parts

#calculate for a single novel
dict0<-read.csv("Dict_German_NovelWords_3000.csv", header=F, stringsAsFactors = F)
dict3<-read.csv("Dict_German_NamesPlus.csv", header=F, stringsAsFactors = F)
  corpus1 <- VCorpus(DirSource("Kafka_TheCastle_20"), readerControl=list(language="German")) #CHANGE LANGUAGE
  #run transformations
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  #remove problems
  problems<-c("apparat","datumsangaben","seite","page","erläuterungen", "kommentar", "kapitel", "mutter", "vater")
  #problems<-c("chapter", "mother", "father")
  #problems<-c("a", "des", "mère", "père")
  corpus1 <- tm_map(corpus1, removeWords, problems)
  #get scaling value
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  scaling1<-rowSums(corpus1.matrix)
  #remove stopwords
  corpus1 <- tm_map(corpus1, removeWords, stopwords("de")) #CHANGE LANGUAGE
  #remake DTM
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  #keep only words in general dictionary
  corpus1.sub<-corpus1.dtm[,colnames(corpus1.dtm) %in% dict0$V1]
  #keep top 1000 terms
  keep.freqs<-sort(colSums(as.matrix(corpus1.sub)), decreasing = T)[1:1000]
  corpus1.matrix<-as.matrix(corpus1.sub[, colnames(corpus1.sub) %in% names(keep.freqs)])
  #remove proper names
  corpus1.matrix<-corpus1.matrix[, !colnames(corpus1.matrix) %in% dict3$V1]
  #scale
  conversion.scaled<-corpus1.matrix/scaling1
  nword=dim(conversion.scaled)[2]
  freq.dat<-array(0,c(20,nword))
  freq.dat[1,]=conversion.scaled[1,]
  freq.dat[2,]=conversion.scaled[12,]
  for (j in 3:9){
    freq.dat[j,]=conversion.scaled[11+j,]
  }
  for (j in 10:19){
    freq.dat[j,]=conversion.scaled[j-8,]
  }
  freq.dat[20,]=conversion.scaled[13,]
  #create distance table
  #cosine
  conversion.dist<-simil(freq.dat, method="cosine")
  freq.dist<-as.matrix(conversion.dist)
  freq.dist[is.na(freq.dist)]<-0
  #Euclidean
  #conversion.dist<-dist(freq.dat, method = "Euclidean")
  #freq.dist<-as.matrix(pr_dist2simil(conversion.dist))
  cormat<-freq.dist
  cormat[cormat==0]<-1
  #cormat.scale<-scale(cormat, center=TRUE, scale=TRUE)
  cormat.scale <- apply(cormat, 2, function(x) (x-min(x))/(max(x)-min(x)))
  win<-4 #set window of parts
  a.pos<-rep(1, win/2)
  a.neg<-rep(-1, win/2)
  a<-append(a.pos, a.neg)
  b<-append(a.neg, a.pos)
  a.mat<-matrix(rep(a, win/2), ncol=win, byrow=T)
  b.mat<-matrix(rep(b, win/2), ncol=win, byrow=T)
  foote.m<-rbind(a.mat, b.mat)
  foote.win<-win-1
  #observed values
  foote.obs<-vector()
  for (i in 1:(ncol(cormat.scale)-foote.win)){
    cormat.sub<-cormat.scale[i:(i+foote.win), i:(i+foote.win)]
    comb.m<-cormat.sub*foote.m
    foote.score<-sum(comb.m)
    foote.obs<-append(foote.obs, foote.score)
  }
  #add one because moment of change = i+1
  foote.obs<-append(NA, foote.obs)
  #permute n times
  perm.vec<-vector()
  for (i in 1:200){
    perm.m <- cormat[sample(nrow(cormat)), sample(ncol(cormat))]
    replace1<-unname(sample(cormat[,1], nrow(cormat)))
    replace1<-replace1[-which(replace1 == 1)]
    perm.m<-apply(perm.m, 1:2, function (x) if (x == 1){x<-sample(replace1)[1]} else {x<-x})
    for (j in 1:ncol(perm.m)){
      perm.m[j,j]<-1
    }
    perm.scale <- apply(perm.m, 2, function(x) (x-min(x))/(max(x)-min(x)))
    perm.obs<-vector()
    for (l in 1:(ncol(perm.scale)-foote.win)){
      perm.sub<-perm.scale[l:(l+foote.win), l:(l+foote.win)]
      perm.sub.m<-perm.sub*foote.m
      foote.score<-sum(perm.sub.m)
      perm.obs<-append(perm.obs, foote.score)
    }
    perm.vec<-append(perm.vec, perm.obs)
  }
  #calc significance band
  perm.high<-quantile(perm.vec, c(.95))
  foote.df<-data.frame(foote.obs)

### Fig. 2.8 ###
## Inter-chapter novelty in Kafka's The Castle
ggplot(foote.df, aes(x=as.numeric(row.names(foote.df)), y=foote.obs)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  labs(x="Narrative Sections", y="Novelty Score", caption="\nFig. 2.8 Inter-chapter novelty in Kafka's The Castle\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)") +
  #labs(x="Narrative Sections", y="Novelty Score") +
  geom_hline(yintercept=perm.high, lty=3)
  
#calculate for all novels in a directory
#this calculates the point of maximum difference in novels
#output = TurningPoints_German_Cosine.csv
dict0<-read.csv("Dict_German_NovelWords_3000.csv", header=F, stringsAsFactors = F)
dict3<-read.csv("Dict_German_NamesPlus.csv", header=F, stringsAsFactors = F)
final.df<-NULL
for (k in 3:152) {
  print(k)
  dir <- paste("[INSERT DIRECTORY]", as.character(k), sep="")
  #load corpus
  corpus1 <- VCorpus(DirSource(dir), readerControl=list(language="German")) #CHANGE LANGUAGE
  corpus1 <- VCorpus(DirSource("Kafka_TheCastle_20"), readerControl=list(language="German")) #CHANGE LANGUAGE
  #run transformations
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  #remove problems
  problems<-c("apparat","datumsangaben","seite","page","erläuterungen", "kommentar", "kapitel", "mutter", "vater")
  #problems<-c("chapter", "mother", "father")
  #problems<-c("a", "des", "mère", "père")
  corpus1 <- tm_map(corpus1, removeWords, problems)
  #get scaling value
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  scaling1<-rowSums(corpus1.matrix)
  #remove stopwords
  corpus1 <- tm_map(corpus1, removeWords, stopwords("de")) #CHANGE LANGUAGE
  #remake DTM
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  #keep only words in general dictionary
  corpus1.sub<-corpus1.dtm[,colnames(corpus1.dtm) %in% dict0$V1]
  #keep top 1000 terms
  keep.freqs<-sort(colSums(as.matrix(corpus1.sub)), decreasing = T)[1:1000]
  corpus1.matrix<-as.matrix(corpus1.sub[, colnames(corpus1.sub) %in% names(keep.freqs)])
  #remove proper names
  corpus1.matrix<-corpus1.matrix[, !colnames(corpus1.matrix) %in% dict3$V1]
  #scale
  conversion.scaled<-corpus1.matrix/scaling1
  nword=dim(conversion.scaled)[2]
  freq.dat<-array(0,c(20,nword))
  freq.dat[1,]=conversion.scaled[1,]
  freq.dat[2,]=conversion.scaled[12,]
  for (j in 3:9){
    freq.dat[j,]=conversion.scaled[11+j,]
  }
  for (j in 10:19){
    freq.dat[j,]=conversion.scaled[j-8,]
  }
  freq.dat[20,]=conversion.scaled[13,]
  #create distance table
  #cosine
  conversion.dist<-simil(freq.dat, method="cosine")
  freq.dist<-as.matrix(conversion.dist)
  freq.dist[is.na(freq.dist)]<-0
  #Euclidean
  #conversion.dist<-dist(freq.dat, method = "Euclidean")
  #freq.dist<-as.matrix(pr_dist2simil(conversion.dist))
  cormat<-freq.dist
  cormat[cormat==0]<-1
  #cormat.scale<-scale(cormat, center=TRUE, scale=TRUE)
  cormat.scale <- apply(cormat, 2, function(x) (x-min(x))/(max(x)-min(x)))
  win<-4 #set window of parts
  a.pos<-rep(1, win/2)
  a.neg<-rep(-1, win/2)
  a<-append(a.pos, a.neg)
  b<-append(a.neg, a.pos)
  a.mat<-matrix(rep(a, win/2), ncol=win, byrow=T)
  b.mat<-matrix(rep(b, win/2), ncol=win, byrow=T)
  foote.m<-rbind(a.mat, b.mat)
  foote.win<-win-1
  #observed values
  foote.obs<-vector()
  for (i in 1:(ncol(cormat.scale)-foote.win)){
    cormat.sub<-cormat.scale[i:(i+foote.win), i:(i+foote.win)]
    comb.m<-cormat.sub*foote.m
    foote.score<-sum(comb.m)
    foote.obs<-append(foote.obs, foote.score)
  }
  #add one because moment of change = i+1
  foote.obs<-append(NA, foote.obs)
  #permute n times
  perm.vec<-vector()
  for (i in 1:200){
    perm.m <- cormat[sample(nrow(cormat)), sample(ncol(cormat))]
    replace1<-unname(sample(cormat[,1], nrow(cormat)))
    replace1<-replace1[-which(replace1 == 1)]
    perm.m<-apply(perm.m, 1:2, function (x) if (x == 1){x<-sample(replace1)[1]} else {x<-x})
    for (j in 1:ncol(perm.m)){
      perm.m[j,j]<-1
    }
    perm.scale <- apply(perm.m, 2, function(x) (x-min(x))/(max(x)-min(x)))
    perm.obs<-vector()
    for (l in 1:(ncol(perm.scale)-foote.win)){
      perm.sub<-perm.scale[l:(l+foote.win), l:(l+foote.win)]
      perm.sub.m<-perm.sub*foote.m
      foote.score<-sum(perm.sub.m)
      perm.obs<-append(perm.obs, foote.score)
    }
    perm.vec<-append(perm.vec, perm.obs)
  }
  #calc significance band
  perm.high<-quantile(perm.vec, c(.95))
  #plot
  plot(foote.obs, type="line")
  #plot(foote.obs, type="line", xlab="Sections", ylab="Novelty")
  axis(side = 1, at = c(1:20))
  #draw line to indicate significance level
  abline(h=perm.high, lty=2)
  #calculate which sections exceed the significance band
  turning.point<-which(foote.obs > perm.high)
  if (length(turning.point) == 0){turning.point<-0}
  work<-row.names(corpus1.matrix)[1]
  word.count<-sum(scaling1)
  temp.df<-data.frame(work, word.count, turning.point)
  final.df<-rbind(final.df, temp.df)
}

