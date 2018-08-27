##### CHAPTER THREE: TOPICS ######

library("tm")
library("proxy")
library("SnowballC")
library("stats")
library("topicmodels")
library(slam)
library(splitstackshape)

### 3.1 ###
#this script allows you run a topic model on a given set of documents
#see 19C_NovelPlusPoetry_Words_Topic100_Alpha0.5.csv for output

#read in a dictionary of English words to remove proper names and extended stopwords
dict<-read.csv("Dict_English_NovelWords_3000.csv", header=F)
words<-as.character(dict$V1)
words<-unique(wordStem(words))

#create your DTM
corpus1 <- VCorpus(DirSource("[INSERT DICTIONARY]"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")
corpus1.dtm<-DocumentTermMatrix(corpus1)
corpus1.sub<-as.matrix(corpus1.dtm[,colnames(corpus1.dtm) %in% words])

#remove rows with all 0 values (this is caused by only keeping non sparse words)
row_zero<-rowSums(corpus1.sub)
length(which(row_zero == 0))

# if length > 0 !!!!! run these lines
# row_sub<-as.vector(which(row_zero == 0))
# corpus2<-corpus1.sub[-row_sub,]
# mode(corpus2) <- "numeric"

#if length = 0 then run this line
corpus2<-corpus1.sub

#define number of topics
k=100

#set parameters
control_LDA_Gibbs<-list(alpha=0.5, estimate.beta=TRUE, iter=1000, burnin=20, best=TRUE)

#run model
topicmodel<-LDA(corpus2, method="Gibbs", k=k, control = control_LDA_Gibbs) # k = # topics

#extract topic words - top 20
term_dis<-terms(topicmodel, 20) 

#to see all words per topic
term_dis_all<-terms(topicmodel, ncol(corpus.scaled)) 

### 3.2 ###

### Table 3.1 ###
#load workspace "Novel_German_Topic150.RData"
#this workspace contains all inputs and outputs from the model used in the chapter
#model was generated using LDA(x, k=300, method = "VEM")
#this model was produced very early in the project (as in 2012) and there are a number of things I would refine if
#I were to do it again (better stopwords list being the most important, but also not stemming to remove ambiguity between word types)
#probabilities2 = the topic to document probabilities
#corpus.sparse.dtm = sparse document term matrix
#words.topic = the top 20 words per topic

#run metrics on a particular topic
#this gives you a range of diagnostics with which to understand your
#topic before analyzing its content
#it goes through and calculates a set of scores for every topic
#so you can compare your chosen topic to the population of topics
#see p. 76f. in Enumerations for a discussion
#the measures are also commented below

final.df<-NULL
for (i in 1:ncol(probabilities2)){
  print(i)
  topic.no<-i
  #no. tokens
  #takes the top 20 words and counts their overall frequency in the corpus
  tok.sub<-corpus.sparse.dtm[,which(colnames(corpus.sparse.dtm) %in% as.character(words.topic[,topic.no]))]
  no.tokens<-sum(tok.sub)
  #no. documents
  #counts the number of documents that exhibit a given topic above some artificial threshold (here 0.2)
  doc.sub<-data.frame(row.names(probabilities2), probabilities2[,topic.no])
  doc.sub<-doc.sub[which(doc.sub$probabilities2...topic.no. > 0.2),]
  no.docs<-nrow(doc.sub)
  if (no.docs > 5){
  #no. novels
  #counts how many novels the documents belong to
  nov.sub<-cSplit(doc.sub, "row.names.probabilities2.", sep="_")
  no.novels<-nlevels(factor(nov.sub$row.names.probabilities2._3))
  #heterogeneity
  #finds the number of documents that belong to the single most dominant novel to see the extent to
  #which the topic is dominated by a single novel
  heterogeneity<-1-max(table(factor(nov.sub$row.names.probabilities2._3)))/nrow(nov.sub)
  #avg. date
  #average date of the documents
  avg.date<-round(mean(nov.sub$row.names.probabilities2._1))
  #sd.date
  #standard deviation of the document dates
  sd.date<-round(sd(nov.sub$row.names.probabilities2._1))
  #coherence
  #see Mimno's article -- measures co-document frequency relative to document frequency
  #the more words appear together in documents (co-document frequency) versus in a single document (doc frequency)
  #the more semantically coherent the topic
  tdm<-as.matrix(t(corpus.sparse.dtm))
  tdm<-tdm[,colnames(tdm) %in% as.character(doc.sub$row.names.probabilities2.)] #keep docs in the top topic list
  tdm<-tdm[row.names(tdm) %in% as.character(words.topic[,topic.no]),] #keep only top 20 topic words
  russel.dist<-as.matrix(simil(tdm, method = "Russel", convert_distances = TRUE))
  russel.final<-russel.dist*ncol(tdm)
  russel.final[is.na(russel.final)]<-0
  coherence.total<-0
  for (k in 1:nrow(tdm)) {
    doc.freq<-length(which(tdm[k,] != 0))
    vec1<-0
    for (m in 1:nrow(russel.final)) {
      if (russel.final[k,m] != 0){
        co.doc.freq<-as.integer(russel.final[k,m])
        coherence1<-log((co.doc.freq+1)/doc.freq)
        vec1<-vec1 + coherence1
      }
    }
    coherence.total<-coherence.total + vec1
  }
  coherence.total<-abs(coherence.total)
  #avg.sim
  #calculate the average pairwise similarity between documents in this topic using all their words
  #similarity is measured using correlation
  dtm<-corpus.sparse.dtm[row.names(corpus.sparse.dtm) %in% as.character(doc.sub$row.names.probabilities2.),]
  dtm<-as.matrix(dtm[,col_sums(dtm) != 0])
  dtm.sim<-as.matrix(simil(dtm, method = "correlation"))
  sim.score<-mean(dtm.sim[lower.tri(dtm.sim)])
  #store in data frame
  temp.df<-data.frame(topic.no, no.tokens, no.docs, no.novels, heterogeneity, avg.date, sd.date, coherence.total, sim.score)
  final.df<-rbind(final.df, temp.df)
  }
}
test<-final.df

#create table of percentiles for each measure
row.v<-test$topic.no
test<-test[,-1]
test<-apply(test, 2, as.numeric)
row.names(test)<-row.v
#add trivial adjustment to allow for non-duplicate percentiles
add.v<-seq(from=0.000001, to=(0.000001*277), by=0.000001)
test<-test+add.v
test2<-apply(test,2,function(x) cut(x, breaks=quantile(x, probs=seq(0,1, by=0.01)), include.lowest=TRUE, labels=c(1:100)))
colnames(test2)<-c("tokens.per", "docs.per", "novels.per", "het.per", "avg.date.per",
                   "sd.date.per", "coherence.per", "sim.per")
test2<-apply(test2, 2, as.integer)
row.names(test2)<-row.v
test3<-cbind(test, test2)

#print results
test3[which(row.names(test3) == "150"),9:16]

#create Table A.3.1
topic.no<-150
doc.sub<-data.frame(row.names(probabilities2), probabilities2[,topic.no])
doc.sub<-doc.sub[which(doc.sub$probabilities2...topic.no. > 0.2),]
passages<-as.character(doc.sub$row.names.probabilities2.)
doc.sub<-cSplit(doc.sub, "row.names.probabilities2.", sep="_")
novels<-data.frame(table(doc.sub$row.names.probabilities2._3))
#find average probability
final.df<-NULL
for (i in 1:nlevels(doc.sub$row.names.probabilities2._3)){
  sub<-doc.sub[doc.sub$row.names.probabilities2._3 == levels(doc.sub$row.names.probabilities2._3)[i],]
  mean.probability<-mean(sub$probabilities2...topic.no.)
  max.probability<-max(sub$probabilities2...topic.no.)
  author<-as.character(sub$row.names.probabilities2._2)[1]
  title<-as.character(sub$row.names.probabilities2._3)[1]
  date<-as.numeric(sub$row.names.probabilities2._1)[1]
  no.docs<-nrow(sub)
  temp.df<-data.frame(author, title, date, no.docs, mean.probability, max.probability)
  final.df<-rbind(final.df, temp.df)
}

### 3.3 ###
#Within-topic clustering
#this script uses the method of hierarchical clustering to observe sub-groups within the topical space.
topic.no<-150
doc.sub<-data.frame(row.names(probabilities2), probabilities2[,topic.no])
doc.sub<-doc.sub[which(doc.sub$probabilities2...topic.no. > 0.2),]
topic.dtm<-corpus.sparse.dtm[row.names(corpus.sparse.dtm) %in% as.character(doc.sub$row.names.probabilities2.),] #keep docs in the top topic list
topic.dtm<-topic.dtm[,colnames(topic.dtm) %in% as.character(words.topic[,topic.no])] #keep only top 20 topic words
topic.matrix<-as.matrix(topic.dtm)
topic.dist<-simil(topic.matrix, method="cosine")
topic.dist<-pr_simil2dist(topic.dist)
fit<-hclust(topic.dist, method = "ward.D2") #other options for "ward.D2" = "complete", "single", or "centroid" (there are more...)

#dendrogram of the documents in Topic 150
plot(as.dendrogram(fit))
#predict optimal number of clusters
library(dendextend)
fit_k<-find_k(fit, krange = 2:(nrow(topic.dist)-1))
#identify clusters in the dendrogram
rect.hclust(fit, k=fit_k$nc, border="red")

### 3.4 ###
#this script creates a bipartite network of topic to document relationships
#Both topics and documents are nodes where each topic is given an edge to a document
#with which it has a greater than 20% probability of being associated based on the model above

#create a document - topic list such that the probability of a topic being in a document is above 0.2
probabilities3<-probabilities2
colnames(probabilities3)<-seq(1:ncol(probabilities3))
net.df<-NULL
for (i in 1:nrow(probabilities3)){
  print(i)
  sub<-t(probabilities3[i,])
  above<-unname(which(sub[,1]>0.2))
  if (length(above) > 0){
    source<-row.names(probabilities3)[i]
    target<-above
    weight<-sub[above,]
    temp.df<-data.frame(source, target, weight)
    net.df<-rbind(net.df, temp.df)
  }
}

#subset by only those topics associated with documents associated with Topic 150
#here we're creating a subset of the network to visualize and keeping
#every topic that is one hop from the documents associated with Topic 150
#choose topic of interest
topic.no<-150
#subset documents for that topic
doc.sub<-data.frame(row.names(probabilities2), probabilities2[,topic.no])
doc.sub<-doc.sub[which(doc.sub$probabilities2...topic.no. > 0.2),]
#subset network graph by documents from that topic
topic.sub<-net.df[as.character(net.df$source) %in% as.character(doc.sub$row.names.probabilities2.),]
#subset network graph by topics associated with documents from that topic
#this is the final table to be used in the next figure
#it provides a limited network graph of all topics associated with all documents associated with Topic 150
net.sub<-net.df[as.character(net.df$target) %in% as.character(topic.sub$target),]
#remove top topic (stopword topic)
net.sub2<-net.sub[net.sub$target != as.numeric(names(which(table((factor(net.sub$target))) == max(table((factor(net.sub$target))))))),]
#remove bottom 4 topics below Topic 150
net.sub2<-net.sub2[as.character(net.sub2$target) %in% names(sort(table(as.character(net.sub2$target)), decreasing=T)[1:15]),]
write.csv(net.sub2, file="Topic150_Edge_List.csv")

### Fig. 3.1 ###
#Network diagram of a bipartite graph of topics to documents
#this figure was generated using Gephi
#Gephi file located in data folder

### 3.5 ###
#generating MDS plots of topic words based on correlations of word frequencies
#it is similar to the clustering model above but instead of clustering documents based on words
#here we are clustering words based on documents

#select topic
topic.no<-150
#subset by topic words
doc.sub<-data.frame(row.names(probabilities2), probabilities2[,topic.no])
#subset by documents
doc.sub<-doc.sub[which(doc.sub$probabilities2...topic.no. > 0.2),]
#create term document matrix
topic.dtm<-corpus.sparse.dtm[row.names(corpus.sparse.dtm) %in% as.character(doc.sub$row.names.probabilities2.),] #keep docs in the top topic list
topic.dtm<-topic.dtm[,colnames(topic.dtm) %in% as.character(words.topic[,topic.no])] #keep only top 20 topic words
topic.matrix<-as.matrix(topic.dtm)
tdm<-t(topic.matrix)
#create similarity matrix
tdm.dist<-simil(tdm, method="correlation")
#transform to distance matrix for clustering
tdm.dist<-pr_simil2dist(tdm.dist)
#establish clusters w hierarchical clustering (using k=2)
fit <- hclust(tdm.dist, method="ward.D2") 
#cut tree into k clusters
groups <- cutree(fit, k=2) # cut tree into k clusters
#get coordinates using MDS
coords <- cmdscale(tdm.dist, eig = TRUE, k = 2)
x <- coords$points[, 1]
y <- coords$points[, 2]
#build data frame
mds<-data.frame(x, y, row.names(coords$points), unname(groups))
colnames(mds)<-c("x", "y", "words", "cluster")
write.csv(mds, file="Topic150_Coordinates_MDS.csv") #add English translations to words for plotting

### Fig. 3.2 ###
library(ggplot2)
mds<-read.csv("Topic150_Coordinates_MDS.csv")
ggplot(mds, aes(x=x, y=y)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.title=element_blank()) +
  theme(legend.position="none") +
  geom_text(aes(label=translation, colour=as.factor(cluster)),hjust=0, vjust=0) +
  scale_colour_manual(values=c("black", "grey50")) +
  scale_x_continuous(limits = c(-0.8, 0.8)) +
  ggtitle("Topic 150")+
  theme(plot.title = element_text(hjust = 0.5, size=22))+
  labs(caption="\nFig. 3.2 Correlation of words associated with Topic 150 in its strong state.\nBlack/gray represents predicted clusters.\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")

### 3.6 ###
### Weak State Analysis ###

#representation of correlation in documents where the topic is weakly present
#weak = 
topic.no<-150
#subset by topic words
doc.sub<-data.frame(row.names(probabilities2), probabilities2[,topic.no])
#subset by documents
doc.sub<-doc.sub[which(doc.sub$probabilities2...topic.no. > 0.1 & doc.sub$probabilities2...topic.no. < 0.13),]
#create term document matrix
topic.dtm<-corpus.sparse.dtm[row.names(corpus.sparse.dtm) %in% as.character(doc.sub$row.names.probabilities2.),] #keep docs in the top topic list
topic.dtm<-topic.dtm[,colnames(topic.dtm) %in% as.character(words.topic[,topic.no])] #keep only top 20 topic words
topic.matrix<-as.matrix(topic.dtm)
tdm<-t(topic.matrix)
#create similarity matrix
tdm.dist<-simil(tdm, method="correlation")
#transform to distance matrix for clustering
tdm.dist<-pr_simil2dist(tdm.dist)
#use clusters from model above
#get coordinates using MDS
coords <- cmdscale(tdm.dist, eig = TRUE, k = 2)
x <- coords$points[, 1]
y <- coords$points[, 2]
#build data frame
mds<-data.frame(x, y, row.names(coords$points), unname(groups))
colnames(mds)<-c("x", "y", "words", "cluster")
#write.csv(mds, file="Topic150_Coordinates_MDS_Weak.csv") #add English translations to words for plotting

### Fig. 3.3 ###
library(ggplot2)
mds<-read.csv("Topic150_Coordinates_MDS_Weak.csv")
ggplot(mds, aes(x=x, y=y)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.title=element_blank()) +
  theme(legend.position="none") +
  geom_text(aes(label=translation, colour=as.factor(cluster)),hjust=0, vjust=0) +
  scale_colour_manual(values=c("black", "grey50")) +
  scale_x_continuous(limits = c(-0.6, 0.55)) +
  ggtitle("Topic 150 - Weak State")+
  theme(plot.title = element_text(hjust = 0.5, size=22))+
  labs(caption="\nFig. 3.3 Correlation of words in Topic 150 in its weak state (posterior probability of 10-12%).\nBlack/gray represents predicted clusters.\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")

#which documents are present in the weak state?
doc.sub

#of these what are their strong topics?
other.df<-net.df[as.character(net.df$source) %in% as.character(doc.sub$row.names.probabilities2.),]

#choose a secondary topic of interest and observe its presence with Topic 150 -- both in their weak state
topic.no1<-150
topic.no2<-73
#which words overlap (all)?
intersect(as.character(words.topic[,topic.no1]), as.character(words.topic[,topic.no2]))
#subset topic words by top 10 for clarity
topic.words.set.sub<-append(as.character(words.topic[,topic.no1][1:10]), as.character(words.topic[,topic.no2][1:10]))
topic.words.set<-append(as.character(words.topic[,topic.no1]), as.character(words.topic[,topic.no2]))
#subset by topic words
doc.sub1<-data.frame(row.names(probabilities2), probabilities2[,topic.no1])
doc.sub2<-data.frame(row.names(probabilities2), probabilities2[,topic.no2])
colnames(doc.sub1)<-c("x", "y")
colnames(doc.sub2)<-c("x", "y")
doc.sub1$group<-150
doc.sub2$group<-73
doc.sub3<-rbind(doc.sub1, doc.sub2)
#subset by documents
doc.sub3<-doc.sub3[which(doc.sub3$y > 0.1 & doc.sub3$y < 0.13),]
#create term document matrix
topic.dtm<-corpus.sparse.dtm[row.names(corpus.sparse.dtm) %in% as.character(doc.sub3$x),] #keep docs in the top topic list
topic.dtm<-topic.dtm[,colnames(topic.dtm) %in% topic.words.set] #keep only top 20 topic words
topic.matrix<-as.matrix(topic.dtm)
tdm<-t(topic.matrix)
#create similarity matrix
tdm.dist<-simil(tdm, method="correlation")
#transform to distance matrix for clustering
tdm.dist<-pr_simil2dist(tdm.dist)
#get coordinates using MDS
coords <- cmdscale(tdm.dist, eig = TRUE, k = 2)
x <- coords$points[, 1]
y <- coords$points[, 2]
#assign clusters based on topic #
group.v<-vector()
for (i in 1:length(row.names(coords$points))){
  if (row.names(coords$points)[i] %in% as.character(words.topic[,topic.no1])){group<-150
  } else {group<-73}
  group.v<-append(group.v, group)
}
#build data frame
mds<-data.frame(x, y, row.names(coords$points), group.v)
colnames(mds)<-c("x", "y", "words", "cluster")
#subset by top 10 words for visual clarity
mds<-mds[as.character(mds$words) %in% topic.words.set.sub,]
#write.csv(mds, file="Topic150_Topic73_Coordinates_MDS_Weak.csv") #add English translations to words for plotting

### Fig. 3.4 ###
library(ggplot2)
mds<-read.csv("Topic150_Topic73_Coordinates_MDS_Weak.csv")
ggplot(mds, aes(x=x, y=y)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.title=element_blank()) +
  theme(legend.position="none") +
  geom_text(aes(label=translation, colour=as.factor(cluster)),hjust=0, vjust=0) +
  scale_colour_manual(values=c("black", "grey50")) +
  scale_x_continuous(limits = c(-0.45, 0.7)) +
  ggtitle("Topic 150 and Topic 73 - Weak States")+
  theme(plot.title = element_text(hjust = 0.5, size=22))+
  labs(caption="\nFig. 3.4 Correlation of words associated with Topic 150 (gray) and Topic 73 (black)\nin documents where the topics are only weakly present.\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")

#observing Topic 73 in its strong state
#select topic
topic.no<-73
#subset by topic words
doc.sub<-data.frame(row.names(probabilities2), probabilities2[,topic.no])
#subset by documents
doc.sub<-doc.sub[which(doc.sub$probabilities2...topic.no. > 0.2),]
#create term document matrix
topic.dtm<-corpus.sparse.dtm[row.names(corpus.sparse.dtm) %in% as.character(doc.sub$row.names.probabilities2.),] #keep docs in the top topic list
topic.dtm<-topic.dtm[,colnames(topic.dtm) %in% as.character(words.topic[,topic.no])] #keep only top 20 topic words
topic.matrix<-as.matrix(topic.dtm)
tdm<-t(topic.matrix)
#create similarity matrix
tdm.dist<-simil(tdm, method="correlation")
#transform to distance matrix for clustering
tdm.dist<-pr_simil2dist(tdm.dist)
#establish clusters w hierarchical clustering
fit <- hclust(tdm.dist, method="ward.D2") 
#find optimal # clusters
dend_k<-find_k(fit, krange = 2:(nrow(tdm.dist)-1))
#cut tree into k clusters
groups <- cutree(fit, k=dend_k$nc)
#get coordinates using MDS
coords <- cmdscale(tdm.dist, eig = TRUE, k = 2)
x <- coords$points[, 1]
y <- coords$points[, 2]
#build data frame
mds<-data.frame(x, y, row.names(coords$points), unname(groups))
colnames(mds)<-c("x", "y", "words", "cluster")
write.csv(mds, file="Topic73_Coordinates_MDS.csv") #add English translations to words for plotting

### Fig. 3.5 ###
library(ggplot2)
mds<-read.csv("Topic73_Coordinates_MDS.csv")
ggplot(mds, aes(x=x, y=y)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.title=element_blank()) +
  theme(legend.position="none") +
  geom_text(aes(label=translation, colour=as.factor(cluster)),hjust=0, vjust=0) +
  scale_colour_manual(values=c("black", "grey50")) +
  scale_x_continuous(limits = c(-0.65, 0.5)) +
  ggtitle("Topic 73 - Strong State")+
  theme(plot.title = element_text(hjust = 0.5, size=22))+
  labs(caption="\nFig. 3.4 Correlation of words associated with Topic 73 in documents\nwhere the topic is strongly present. Colors represent predicted clusters.\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")

#which documents are associated with Mereau's Amanda und Eduard and also weakly so with topic 150?

#first establish all topics associated with Amanda und Eduard above weak threshold of 0.1
weak.150<-probabilities2[grep("Amanda", row.names(probabilities2)),]
colnames(weak.150)<-seq(1:ncol(weak.150))
weak.df<-NULL
for (i in 1:nrow(weak.150)){
  sub<-t(weak.150[i,])
  above<-unname(which(sub[,1]>0.1))
  if (length(above) > 0){
    source<-row.names(weak.150)[i]
    target<-above
    weight<-sub[above,]
    temp.df<-data.frame(source, target, weight)
    weak.df<-rbind(weak.df, temp.df)
  }
}

#then find which documents also have topic 150 weakly in them?
weak.sub<-weak.df[weak.df$target == 150,]
weak.sub2<-weak.df[weak.df$weight > 0.2,]
weak.sub3<-weak.sub2[as.character(weak.sub2$source) %in% as.character(weak.sub$source),]

#compare these values to Topc 73 overall to find which docs have it the strongest
topic.no<-73
#subset by topic words
doc.sub<-data.frame(row.names(probabilities2), probabilities2[,topic.no])
doc.sub<-doc.sub[which(doc.sub$probabilities2...topic.no. > 0.2),]

#what are the most important topics for this novel?
table(weak.df$target)
