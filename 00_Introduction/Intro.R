### Introduction ###

#### 0.1 ####

#what is the average repetitiveness of pages of novels?
#This script takes a selection of 1,275 novels published since 1950
#It then takes 20 random page pairs from each novel and for each pair
#it calculates the % of words that appear on the following page that
#were on the previous page.

#the following code is for demonstration purposes. See below for loading the table of values
#select random novels from the NOVEL_20C corpus
a<-read.csv("KLAB_RANDOM_META.csv")
#subset by works published from 1950 forward and take 25 per year
a<-a[a$PUBL_DATE > 1949,]
b<-NULL
for (i in 1:nlevels(factor(a$PUBL_DATE))){
  sub<-a[a$PUBL_DATE == levels(factor(a$PUBL_DATE))[i],]
  sub<-sub[sample(nrow(sub), 25),]
  b<-rbind(b, sub)
}
filenames<-as.character(b$FILENAME)
#calculate repeating words between pages
#see below to preload the vector of percentages
#this code demonstrates the steps used on the original data which cannot be shared
#establish an empty vector
page.v<-vector()
#for all files
for (i in 1:length(filenames)){
  print(i)
  #import work
  work<-scan(filenames[i], what="character", quote="", quiet = T)
  #clean
  work.clean<- gsub("\\d", "", work)
  work.clean<-gsub("\\W", "", work)
  work.clean<- tolower(work.clean)
  work.clean<-work.clean[work.clean != ""]
  #take 20 pairs per novel
  #work must be more than 20K words in length to qualify
  if (length(work.clean) > 20000){
    for (j in 1:20){
      #chose a random starting point
      start<-sample((length(work.clean)-1200), 1)
      #find first selection of 500 words (i.e. a "page")
      sub1<-work.clean[start:(start+499)]
      #take following 500 words
      sub2<-work.clean[(start+500):(start+999)]
      #what percentage of words in page 2 are in page 1?
      sub.ttr<-length(which(sub2 %in% sub1))/length(sub2)
      #create a single vector for all values
      page.v<-append(page.v, sub.ttr)
    }
  }
}

### START HERE ###

#take the mean value, i.e. the percentage of words that repeat 
#load table
page.v<-read.csv("RepeatingWords.csv")
mean(page.v)
#observe distribution for normality
plot(density(page.v))


##### 0.2 ####

### vector space model example ###
library(tm)
library(lsa)
library(proxy)
#ingest corpus
corpus1 <- VCorpus(DirSource("Werther_Sample"), readerControl=list(language="German"))
#clean
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
#make document term matrix
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
row.names(corpus1.matrix)<-c(1,2,3)
#make similarity matrix
m<-as.matrix(simil(corpus1.matrix, method="Cosine"))


#### 0.3 ####

#### Distinctive Words #####
#observing distinctive words of Kafka's Castle
#this uses both a Fisher's Exact Test and Log-Likelihood ratio to identify distinctive words
#For every word in the novel, it builds a contingency table in the following format
#         Kafka   NotKafka
#Word     a         b
#NotWord  c         d

corpus1 <- VCorpus(DirSource("2_txtalb_Novel450_DE"), readerControl=list(language="German"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1.dtm<-TermDocumentMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#remove terms that do not occur in at least 60% of documents
#and that are less than 3 characters
corpus1.sparse<-removeSparseTerms(corpus1.dtm, 0.4)
corpus1.sparse<-as.matrix(corpus1.sparse)
corpus1.sparse<-corpus1.sparse[which(nchar(row.names(corpus1.sparse)) > 2),]

#then test each word individually
#kafka's castle = column 148
#log likelihood function
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}
#run
mdw.df<-NULL
for (i in 1:nrow(corpus1.sparse)){
  #count of word in Kafka
  a<-corpus1.sparse[i,148]
  #count of word not in Kafka
  b<-sum(corpus1.sparse[i,])-a
  #count of other words in Kafka
  c<-sum(corpus1.sparse[,148])-a
  #count of other words not in Kafka
  d<-sum(corpus1.sparse)-c
  #odds ratio
  df<-data.frame(c(a, c), c(b, d))
  model<-fisher.test(df)
  word<-row.names(corpus1.sparse)[i]
  odds.ratio<-model$estimate[[1]]
  odds.p<-model$p.value
  llr = 2*sum(df)*(H(df)-H(rowSums(df))-H(colSums(df)))
  temp.df<-data.frame(word, a, b, odds.ratio, odds.p, llr)
  mdw.df<-rbind(mdw.df, temp.df)
}

#limit by p-value using Bonferroni correction (0.05 / 3000)
mdw2.df<-mdw.df[mdw.df$odds.p < 0.000016,]
#sort by Log-likelihood or odds ratio and observe differences between those scores
mdw2.df<-mdw2.df[order(-mdw2.df$odds.ratio),]
mdw2.df<-mdw2.df[order(-mdw2.df$llr),]
#write.csv(mdw2.df, file="MDW_Kafka.csv", row.names = F)


### 0.4 ###

#### collocate analysis of Weg ####
#creating a vector space model of the word Weg in the German novel
#this script goes through all novels and builds a vector representation of the collocates of the word Weg
#it creates 150 separate representations, so that each novel is withheld from the whole and can be compared to it
#it then creates a vector representation of Weg for each novel and compares it to the whole collection (not including itself)
#the similarity scores are normalized by the log of the number of times Weg appears
#more occurrences of the word means a novel will by nature approximate the general distribution better
#this normalization reduces the correlation between frequency of Weg and similarity from 0.83 to 0.26

library("tm")
library("SnowballC")

filenames<-list.files("2_txtalb_Novel450_DE", pattern="*.txt", full.names=FALSE)
#change working directory to be inside of "2_txtalb_Novel450_DE"
#input keyword
key<-c("weg")
#input problem words
remove<-c("seite", "stellenkommentar", "datumsangaben", "page", "apparat")
#input stopwords (minus weg!)
stop<-stopwords("German")
stop<-stop[!stop %in% "weg"]
#input window (how many words +/- to search around Weg)
window<-9
#run
#create data frame of collocate counts for each novel
novel.df<-data.frame(matrix(ncol=1, nrow=0))
colnames(novel.df)<-c("work.collocates")
#create vector of all collocates for all novels
all.collocates<-vector()
for (i in 1:length(filenames)) {
  print(i)
  #scan in work and clean
  work<-scan(filenames[i], what="character", quote="")
  work.clean<-gsub("\\d", "", work)
  work.clean<-gsub("[[:punct:]]","", work.clean)
  work.clean<-tolower(work.clean)
  #remove problem words
  work.clean<-work.clean[!work.clean %in% remove]
  #remove stopwords
  work.clean<-subset(work.clean, !(work.clean %in% stop))
  #remove words less than 3 characters
  work.clean<-work.clean[which(nchar(work.clean) > 2)]
  work.clean<-subset(work.clean, !(work.clean %in% stop))
  #stem
  #work.clean<-wordStem(work.clean, language="de")
  #work.clean<-work.clean[work.clean != ""]
  #for each keyword identify collocates
  for (k in 1:length(key)) {
    work.ont<-which(work.clean == as.character(key[k])) # position of keyword
    work.collocates<-vector()
    if (length(work.ont) > 0) {
      for (j in 1:length(work.ont)){
        after<-work.ont[j]+window
        before<-work.ont[j]-window
        if (before <= 0) {before <- (window-1)-(abs(before))}
        if (after > length(work.clean)) {after <- after-(after-length(work.clean))}
        shift<-work.ont[j]+1
        before.gram<-work.clean[before:(work.ont[j]-1)]
        after.gram<-work.clean[shift:after]
        total.gram<-vector()
        total.gram<-append(before.gram, after.gram)
        work.collocates<-append(work.collocates, total.gram)
      }
      all.collocates<-append(all.collocates, work.collocates)
      temp.df<-data.frame(table(work.collocates))
      novel.df<-merge(novel.df, temp.df, by="work.collocates", all=T)
      colnames(novel.df)[i+1]<-filenames[i]
    }
  }
}

#replace NAs with 0s
novel.df[is.na(novel.df)]<-0

#turn collocate list into table
all.df<-data.frame(table(all.collocates))
all.order<-all.df[order(-all.df$Freq),]
write.csv(all.order, file="Weg_Collocates.csv", row.names=F)

#create a table of all collocates for every novel
#that equals the total collocates minus the novel's collocates
#this table represents the collocate count of Weg for all novels minus a given novel
novel.df2<-novel.df
#order ascending alphabetically
novel.df2<-novel.df2[order(as.character(novel.df2$work.collocates)),]
row.names(novel.df2)<-as.character(novel.df2$work.collocates)
#order the all collocates table ascending alphabetically
all.df<-all.df[order(as.character(all.df$all.collocates)),]
#check for alignment
which(all.df$all.collocates != novel.df2$work.collocates)
#go through and for each novel create a vector that subtracts the novel's
#collocates from the all collocates
final.df<-NULL
for (i in 2:ncol(novel.df2)){
  all.v<-all.df$Freq-novel.df2[,i]
  final.df<-cbind(final.df, all.v)
}
colnames(final.df)<-colnames(novel.df2)[2:151]
row.names(final.df)<-as.character(all.df$all.collocates)

#clean
novel.df3<-novel.df2[,-1]
novel.df3<-as.matrix(novel.df3)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
row.names(novel.df3)<-trim(row.names(novel.df3))
row.names(final.df)<-trim(row.names(final.df))
novel.df3<-novel.df3[!row.names(novel.df3) %in% stopwords("de"),]
final.df<-final.df[!row.names(final.df) %in% stopwords("de"),]
novel.df3<-novel.df3[row.names(novel.df3) != "",]
final.df<-final.df[row.names(final.df) != "",]
#mirror each other
final.df<-final.df[order(row.names(final.df)),]
novel.df3<-novel.df3[order(row.names(novel.df3)),]
final.df<-final.df[row.names(final.df) %in% row.names(novel.df3),]
novel.df3<-novel.df3[row.names(novel.df3) %in% row.names(final.df),]

#measuring similarity using cosine similarity
#this creates a vector of the similarity of every novel to the all collocates vector
library(lsa)
test.v<-vector()
for (i in 1:ncol(final.df)){
  all.1<-data.frame(row.names(final.df), final.df[,i])
  novel.1<-data.frame(row.names(novel.df3), novel.df3[,i])
  test.df<-cbind(all.1, novel.1)
  #normalize by word count
  test<-cosine(test.df[,2]/sum(final.df[,i]), test.df[,4]/sum(novel.df3[,i]))
  test.v<-append(test.v, test)
}

#observe the distribution for normality
hist(test.v)

#create data frame with results
df<-data.frame(colnames(corpus1.matrix), test.v, scale(test.v, center=T, scale=T))
colnames(df)<-c("novels", "similarity", "z")
#add Weg count
weg<-corpus1.matrix[which(row.names(corpus1.matrix) == "weg"),]
df$weg.count<-unname(weg)
#test for correlation between frequency of Weg and similarity to average novel
cor.test(df$weg.count, df$similarity)
plot(df$weg.count, df$similarity)
#adjust by log of frequency of Weg
cor.test(df$weg.count, df$similarity/(log(df$weg.count)))
#scale
df$norm<-scale(df$similarity/(log(df$weg.count)), center=T, scale=T)
#observe Kafka
df[which(df$novels == "DE_1922_Kafka,Franz_DasSchloß_Novel.txt"),]
write.csv(df, file="Weg_Similarity_Table.csv", row.names=F)











