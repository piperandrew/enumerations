###### Enumerations Conclusion ########

### C.0 ###
#vector space model using Word2Vec
#library("devtools")
library(magrittr)
library(tsne)
library(wordVectors)
#install_github("bmschmidt/wordVectors")

#read existing model
model<-read.vectors("Cont_Histories.bin")
#find words most similar to keyword
model %>% nearest_to(model[["implicated"]], 20) %>% round(3)

#### C.1 ####
#this script provides a variety of self-implication measures
library("tm")
library("SnowballC")
library("proxy")
library("cluster")
library("dendextend")
library("splitstackshape")

#Self-Implication Scores

# first establish function to clean texts
cleanText<- function (x){
  work.clean<- gsub("\\d", "", x)
  work.clean<- tolower(work.clean)
  work.clean<- gsub("\\W", "", work.clean)
  work.clean<- work.clean[work.clean != ""]
}

# then establish filenames
#these have been transformed into DTMs in accompanying data to preserve privacy
filenames1<-list.files("Enum_Chaps_NoNotes")
filenames2<-list.files("Enum_Parking")

#measures
#chapter word count (chap.word.count)
#parking to chapter ratio (park.chap.ratio)
#jettisoned words (abandoned.words)
#median similarity (see below)

revision.df<-NULL
for (i in 1:length(filenames1)){
  #grab chapters
  setwd("")
  work1<-scan(filenames1[i], what="character", quote="")
  work.clean1<-cleanText(work1)
  
  #grab outtakes
  setwd("")
  work2<-scan(filenames2[i], what="character", quote="")
  work.clean2<-cleanText(work2)
  
  #chapter word count
  chap.word.count<-length(work.clean1)
  
  #outtake to chapter ratio
  park.chap.ratio<-length(work.clean2)/length(work.clean1)

  #create frequency table for each
  df.chap<-data.frame(table(work.clean1))
  df.park<-data.frame(table(work.clean2))
  #relabel columns
  colnames(df.chap)<-c("words", "freq")
  colnames(df.park)<-c("words", "freq")
  
  #remove stopwords
  remove<-gsub("\\W", "", stopwords("en"))
  df.park<-df.park[!df.park$words %in% remove,]
  df.chap<-df.chap[!df.chap$words %in% remove,]

  #how many words are in the outtakes NOT in the chapter
  #this equals percentage of jettisoned thoughts
  
  #first find words in the outtakes not in the chapter
  diff.df<-df.park[which(!df.park$words %in% df.chap$words),]
  #second sum their frequency and divide by total # of original tokens
  abandoned.words<-sum(diff.df$freq)/sum(df.park$freq)
  chapter<-filenames1[i]
  temp.df<-data.frame(chapter, chap.word.count, park.chap.ratio, abandoned.words)
  revision.df<-rbind(revision.df, temp.df)
}

##add column on average similarity to JSTOR articles
#compared to 2K JSTOR articles written over last decade that have the discipline and subject field
#as "Languages and literature"

#run TM Package on articles and chapters after removing proper names
#names are locate din dictionaries in accompanying data
remove1<-read.csv("Dict_English_NamesPlus.csv", stringsAsFactors = F, header=F)
remove2<-read.csv("Conc_NodeList.csv", stringsAsFactors = F)
remove3<-read.csv("Dict_French_NamesPlus.csv", stringsAsFactors = F, header=F)
remove4<-read.csv("Dict_German_NamesPlus.csv", stringsAsFactors = F, header=F)
test<-unlist(strsplit(remove2$x, " "))
test<-tolower(test)
test<-removePunctuation(test)
test<-test[test != ""]
remove.all<-append(remove1$V1, test)
remove.all<-append(remove.all, remove3$V1)
remove.all<-append(remove.all, remove4$V1)

#read in corpus1
corpus1 <- VCorpus(DirSource("JSTOR_plusChapters", encoding = "UTF-8"), readerControl=list(language="English"))

#clean the data
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))

#create document term matrix
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))

#remove stopwords
dtm1<-corpus1.dtm[,!colnames(corpus1.dtm) %in% gsub('[[:punct:]]','',stopwords("en"))]

#remove proper names
dtm1<-dtm1[,!colnames(dtm1) %in% remove.all]

#remove sparse words
dtm1<-removeSparseTerms(corpus1.dtm, 0.8)
dtm1<-as.matrix(dtm1)

#OR load DTM from accompanying data
dtm1<-read.csv("JSTOR_plusChapters_DTM.csv")

#create a similarity matrix
sim2<-simil(as.matrix(dtm1), method="correlation")
sim2.m<-as.matrix(sim2)

#calculate the avg similarity of each chapter to all the articles
avg.sim<-vector()
for (i in 1:6){
  test<-median(sim2.m[i,7:ncol(sim2.m)])
  avg.sim<-append(avg.sim, test)
}

revision.df$med.sim<-avg.sim

#test to see if those similarities are uniquely high or low
avg.sim.rand<-vector()
for (i in 1:1000){
  test<-sim2.m[sample(nrow(sim2.m), 1),7:ncol(sim2.m)]
  test<-test[complete.cases(test)]
  test<-median(test)
  avg.sim.rand<-append(avg.sim.rand, test)
}

#calculate confidence interval around avg similarity of random articles
model<-t.test(avg.sim.rand)
model$conf.int[[1]]
model$conf.int[[2]]

### Fig. 7.1 ###
library(ggplot2)
library(gridExtra)
revision.df$name<-gsub(".txt", "", revision.df$chapter)
revision.df$index<-seq(1:6)
tit.size<-14
p1<-ggplot(revision.df, aes(x=index, y=chap.word.count)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  scale_x_continuous(breaks = seq(1, 6, 1), limits = c(1, 6)) +
  geom_point(shape=15, size=3) +
  geom_line(linetype="dotted") +
  labs(title = "Word Count", x="", y="Total Words")

p2<-ggplot(revision.df, aes(x=index, y=park.chap.ratio)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  scale_x_continuous(breaks = seq(1, 6, 1), limits = c(1, 6)) +
  geom_point(shape=15, size=3) +
  geom_line(linetype="dotted") +
  labs(title = "Outtake Ratio", x="", y="Words cut to words kept")

p3<-ggplot(revision.df, aes(x=index, y=abandoned.words)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  scale_x_continuous(breaks = seq(1, 6, 1), limits = c(1, 6)) +
  geom_point(shape=15, size=3) +
  geom_line(linetype="dotted") +
  labs(title = "Abandoned Words", x="", y="Percentage")

p4<-ggplot(revision.df, aes(x=index, y=med.sim)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  scale_x_continuous(breaks = seq(1, 6, 1), limits = c(1, 6)) +
  geom_point(shape=15, size=3) +
  geom_line(linetype="dotted") +
  geom_hline(yintercept=model$conf.int[[1]]) +
  geom_hline(yintercept=model$conf.int[[2]]) +
  scale_y_continuous(limits = c(0.89, 0.93)) +
  labs(title = "Median Similarity", x="", y="Correlation")

#library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol=2)

#C.2
#### Distinctive Words -- Outtakes ####
#this runs on full text docs
#accompanying data has DTMs and thus code needs to be adjusted

#all chapters together
chaps.v<-vector()
park.v<-vector()
for (i in 1:length(filenames1)){
  #grab chapters
  setwd("")
  work1<-scan(filenames1[i], what="character", quote="")
  work.clean1<-cleanText(work1)
  
  #grab outtakes
  setwd("")
  work2<-scan(filenames2[i], what="character", quote="")
  work.clean2<-cleanText(work2)
  
  #place in single vector
  chaps.v<-append(chaps.v, work.clean1)
  park.v<-append(park.v, work.clean2)
}

#create frequency table for each
df.chap<-data.frame(table(chaps.v))
df.park<-data.frame(table(park.v))
#relabel columns
colnames(df.chap)<-c("words", "freq")
colnames(df.park)<-c("words", "freq")
#subset by frequency threshold
chap.sub<-df.chap[df.chap$freq > 19,]
park.sub<-df.park[df.park$freq > 19,]
#merge
all.df<-merge(chap.sub, park.sub, by="words", all=T)
all.df[is.na(all.df)]<-0
colnames(all.df)<-c("words", "chapters", "parking")
#run distinctive word test
#       chapter   parking
#word
#notword

final.df<-NULL
for (i in 1:nrow(all.df)){
  word.chap<-all.df[i,2]
  word.park<-all.df[i,3]
  notword.chap<-sum(all.df$chapters)-word.chap
  notword.park<-sum(all.df$parking)-word.park
  df<-data.frame(c(word.park, notword.park), c(word.chap, notword.chap))
  model<-fisher.test(df)
  word<-as.character(all.df$words[i])
  odds.ratio<-model$estimate[[1]]
  odds.p<-model$p.value
  temp.df<-data.frame(word, word.chap, word.park, odds.ratio, odds.p)
  final.df<-rbind(final.df, temp.df)
}
#limit by p-value
mdw.df<-final.df[final.df$odds.p < 0.01,]
mdw.df<-mdw.df[order(-mdw.df$odds.ratio),]
#limit by words that appear in both corpuses
mdw.df<-mdw.df[mdw.df$word.chap != 0,]

#plot - not shown in book
#wordcloud(mdw.df$word[1:20], (mdw.df$odds.ratio^2)[1:20], min.freq = 0, rot.per = 0, random.order = F)

### C.3 ###
#### Distinctive Words -- Chapters ###

#takes corpus1.dtm as input from above
#does not remove stopwords or proper names

#remove stopwords
#dtm1<-corpus1.dtm[,!colnames(corpus1.dtm) %in% gsub('[[:punct:]]','',stopwords("en"))]

#remove proper names
#dtm1<-dtm1[,!colnames(dtm1) %in% remove.all]

#remove sparse words
dtm1<-removeSparseTerms(corpus1.dtm, 0.8)
dtm1<-as.matrix(dtm1)

#or load from accompanying data
dtm1<-read.csv("JSTOR_plusChapters_DTM_C3.csv")

#run distinctive words test using the JSTOR corpus from above
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}
jstor.df<-NULL
for (i in 1:ncol(dtm1)){
  word.chap<-sum(dtm1[1:6,i])
  word.jstor<-(colSums(dtm1)[i]-word.chap)[[1]]
  notword.chap<-sum(dtm1[1:6,])-word.chap
  notword.jstor<-sum(dtm1)-sum(dtm1[1:6,])-word.jstor
  df<-data.frame(c(word.chap, notword.chap), c(word.jstor, notword.jstor))
  model<-fisher.test(df)
  word<-as.character(colnames(dtm1)[i])
  odds.ratio<-model$estimate[[1]]
  odds.p<-model$p.value
  llr = 2*sum(df)*(H(df)-H(rowSums(df))-H(colSums(df)))
  temp.df<-data.frame(word, word.chap, word.jstor, odds.ratio, odds.p, llr)
  jstor.df<-rbind(jstor.df, temp.df)
}
#limit by p-value
mdw2.df<-jstor.df[jstor.df$odds.p < 0.01,]
#mdw2.df<-mdw2.df[order(-mdw2.df$odds.ratio),]
mdw2.df<-mdw2.df[order(-mdw2.df$llr),]

### Fig. 7.2 ###
#plot over-used words
library(wordcloud)
df.over<-mdw2.df[mdw2.df$odds.ratio>1,]
df.under<-mdw2.df[mdw2.df$odds.ratio<1,]
wordcloud(df.over$word[1:20], (df.over$llr/2)[1:20], min.freq = 0, rot.per = 0, random.order = F)
wordcloud(df.under$word[2:21], (df.under$llr/2)[2:21], min.freq = 0, rot.per = .2, random.order = F, color=)

### C.4 ###
#cluster chapters with JSTOR articles find nearest neighbors
#not used

#takes similarity matrix sim2 from above

#find documents in JSTOR sample most similar to the chapters and extract metadata
meta.d<-read.csv("citations.csv")
meta2<-cSplit(meta.d, "doi", sep="/")
sim2.mat<-as.matrix(sim2)
sim2.mat<-sim2.mat[1:6,]
final.df<-NULL
for (i in 1:nrow(sim.mat)){
  sub<-sim.mat[i,]
  sub<-data.frame(sort(sub, decreasing = T)[1:10])
  sub$files<-row.names(sub)
  sub2<-cSplit(sub, "files", sep="_")
  sub2<-cSplit(sub2, "files_3", sep=".")
  sub2<-sub2[complete.cases(sub2),]
  meta.sub<-meta2[meta2$doi_2 %in% sub2$files_3_1,]
  meta.sub$chapter<-row.names(sim.mat)[i]
  final.df<-rbind(final.df, meta.sub)
}

### C.5 ###
#Understanding author distributions in the MLA Bibliography
## MLA Data ##

#this data represents 1,937 and 6,252 and bibliographic records in the field of literary studies from 1970 and 2015 respectively.
#the data was downloaded from the MLA database using the ProQuest interface in January 2017.
#when asked, the MLA would not share this data from their own records.
#the following filter criteria were used:
#subject headings included "English literature," "American literature," "Canadian literature", "French literature,"
#"Russian literature", "Indian literature," "Chinese literature," "Latin language literature," "Spanish literature"
#"Italian literature"
#only peer-reviewed articles or book chapters were kept (does not include dissertations or theses)
#only articles or book chapters written in English were kept

#the data was exported in .RIS format and Andrew Goldstone's mlaibr package in R was used to transform into .csv files
#authors were extracted using Goldstone's is_author function. 
#This leaves some false positives (such as Hamlet (1600-1601)) which were then manually cleaned up through
#records that appear 3 or more times in the collection. On 700+ records an estimated ~15 FPs were found.
#a second heuristic could be added to remove "authors" whose life spans are less than 20 years

### This data is not allowed to be shared due to licensing policies of the MLA ###

#to download mlaibr
#library("devtools")
#devtools::install_github("agoldst/mlaibr")
library(mlaibr)

#load final tables
all.2015<-read.csv("MLA_Article_Data_2015_All.csv")
all.1970<-read.csv("MLA_Article_Data_1970_All.csv")

#total number of articles
nlevels(as.factor(all.2015$uniqueID))
nlevels(as.factor(all.1970$uniqueID))

#extract authors
authors2015<-all.2015[is_author(all.2015$value),4]
authors1970<-all.1970[is_author(all.1970$value),4]

#sort by frequency
top2015<-data.frame(sort(table(authors2015), decreasing = T))
top2015<-top2015[top2015$Freq > 0,]
top2015$authors<-factor(top2015$authors)
top1970<-data.frame(sort(table(authors1970), decreasing = T))
top1970<-top1970[top1970$Freq > 0,]
top1970$authors<-factor(top1970$authors)

#load cleaned author table
top2015<-read.csv("MLA_2015_AllAuthors_Cleaned.csv")
top1970<-read.csv("MLA_1970_AllAuthors_Cleaned.csv")

#total estimated number of authors
nrow(top2015)
nrow(top1970)

#references to authors

#top20% of authors account for how many articles
#2015
top20per<-round(nrow(top2015)*.2)
auth.sub<-top2015[1:top20per,]
art.sub<-all.2015[all.2015$value %in% auth.sub$authors,]
nlevels(as.factor(art.sub$uniqueID))/nlevels(as.factor(all.2015$uniqueID))

#1970
top20per<-round(nrow(top1970)*.2)
auth.sub<-top1970[1:top20per,]
art.sub<-all.1970[all.1970$value %in% auth.sub$authors,]
nlevels(as.factor(art.sub$uniqueID))/nlevels(as.factor(all.1970$uniqueID))

#top1% of authors account for how many articles
#2015
top1per<-round(nrow(top2015)*.01)
auth.sub<-top2015[1:top1per,]
art.sub<-all.2015[all.2015$value %in% auth.sub$authors,]
nlevels(as.factor(art.sub$uniqueID))/nlevels(as.factor(all.2015$uniqueID))

#1970
top1per<-round(nrow(top1970)*.01)
top1per<-c(33)
auth.sub<-top1970[1:top1per,]
art.sub<-all.1970[all.1970$value %in% auth.sub$authors,]
nlevels(as.factor(art.sub$uniqueID))/nlevels(as.factor(all.1970$uniqueID))

#Gini co-efficient
library(ineq)
Gini(top2015$Freq)
Gini(top1970$Freq)

#HHI score -- measures concentratedness
Herfindahl(top2015$Freq)
Herfindahl(top1970$Freq)

#minus Shakespeare
#Gini
Gini(top2015$Freq[2:nrow(top2015)])
Gini(top1970$Freq[2:nrow(top1970)])

#HHI score -- measures concentratedness
Herfindahl(top2015$Freq[2:nrow(top2015)])
Herfindahl(top1970$Freq[2:nrow(top2015)])

#Shakespeare Effect
#% of total articles
top2015[1,2]/sum(top2015$Freq)
top1970[1,2]/sum(top1970$Freq)
#how many times more than next author
top2015[1,2]/top2015[2,2]
top1970[1,2]/top1970[2,2]

### Fig. 7.3 ###
#this figure shows Lorenz curves of the relative inequalities surrounding academic publishing
#it focuses on the fraction of articles published relative to the fraction of PhD-granting
#institutions and compares this with the fraction of articles that are accounted for by 
#authors as subjects in the MLA Bibliography in 2015.
#NOTE: the underlying articles are not the same. MLA Authors refers to articles in all journals in 2015
#while PhD Institutions refers to articles published in 4 journals since 1970.

library(ineq)
#load journal data
a<-read.csv("journals_masterlist.csv")
phd.rank<-data.frame(sort(table(a$PhD), decreasing = T))
author.rank<-data.frame(sort(table(a$Institutional_Affiliation), decreasing = T))

#calculate Gini coefficient
Gini(phd.rank$Freq)
Gini(author.rank$Freq)

#create smoothing vector for quantiles
vec<-sort(seq(0,1,by = 1/nrow(phd.rank)), decreasing = T)
vec<-vec[1:(length(vec)-1)]
vec2<-sort(seq(0,1,by = 1/nrow(author.rank)), decreasing = T)
vec2<-vec2[1:(length(vec2)-1)]
vec3<-sort(seq(0,1,by = 1/nrow(top2015)), decreasing = T)
vec3<-vec3[1:(length(vec3)-1)]

#create quantile means
phd.rank$quant <- cut(phd.rank$Freq+vec, breaks=quantile(phd.rank$Freq+vec, probs=seq(0,1, by=0.05), na.rm=TRUE), include.lowest=TRUE, labels = seq(1:20))
phd.mean<-sort(unname(tapply(phd.rank$Freq, phd.rank$quant, mean)), decreasing = F)
#author.rank$quant <- cut(author.rank$Freq+vec2, breaks=quantile(author.rank$Freq+vec2, probs=seq(0,1, by=0.05), na.rm=TRUE), include.lowest=TRUE, labels = seq(1:20))
#author.mean<-sort(unname(tapply(author.rank$Freq, author.rank$quant, mean)), decreasing = F)
top2015$quant <- cut(top2015$Freq+vec3, breaks=quantile(top2015$Freq+vec3, probs=seq(0,1, by=0.05), na.rm=TRUE), include.lowest=TRUE, labels = seq(1:20))
mla.mean<-sort(unname(tapply(top2015$Freq, top2015$quant, mean)), decreasing = F)

#create Lorenz objects
Distr1 <- Lc(phd.mean, n = rep(1,length(phd.mean)), plot =F)
#Distr2 <- Lc(author.mean, n = rep(1,length(author.mean)), plot =F)
Distr3 <- Lc(mla.mean, n = rep(1,length(author.mean)), plot =F)

lor1<-data.frame(1-Distr1$p, 1-Distr1$L, c("PhD Institutions"))
#lor2<-data.frame(1-Distr2$p, 1-Distr2$L, c("Employment"))
lor3<-data.frame(1-Distr3$p, 1-Distr3$L, c("MLA Authors"))
colnames(lor1)<-c("x", "y", "category")
#colnames(lor2)<-c("x", "y", "category")
colnames(lor3)<-c("x", "y", "category")
#lor.all<-rbind(lor1, lor2, lor3)
lor.all<-rbind(lor1, lor3)

#plot
ggplot(lor.all, aes(x=x, y=y, shape=category, linetype=category)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  #theme(legend.position="top", legend.direction="horizontal") +
  theme(legend.title=element_blank(), legend.position=c(0.8, 0.2)) +
  geom_point(size=2) +
  geom_line() +
  scale_shape_manual(values=c(17,5)) +
  geom_abline(intercept = 0, slope = 1) +
  annotate("text", x = .5, y = .5, angle=atan(.99)*180/pi, vjust=1.5, label = "Equality") +
  #xlab("Fraction of Authors / Institutions") +
  #ylab("Fraction of Articles")
  labs(x="Fraction of Authors / Institutions", y="Fraction of Articles", caption="\nFig. 7.4 Lorenz curves comparing the relative inequalities\nsurrounding how many academic articles are accounted for\nby the fraction of PhD institutions and the fraction of\nliterary authors listed as subjects in the MLA Bibliography\n\nSource: Andrew Piper, Enumerations: The Quantities of Literature (2018)")




