#### Chap 5: Characterization ####

#This chapter is about understanding fictional characters.
#It consists of two parts. In the first part I explore the homogeneity of 
#of character word distributions. In the second, I explore a particular set of 
#dependent words associated with cognition and sense perception to think about
#"introverted" characters and their role in fiction.

##### Part 1: Character Homogeneity Tests #####

### 5.1 ###
## Fig. 5.1 ##
#distribution of character mentions (these include proper names, aliases, and pronouns
#that refer to a character)
#Input is a table of character occurrences per novel
#these occurrences were derived using David Bamman's BookNLP
#This script then combines those counts by position and averages across the collection
library("ggplot2")
a<-read.csv("Char_Counts_NovelEnglish700.csv")

#what is the mean number of characters in 19C novels?
char.df<-NULL
for (i in 1:nlevels(a$file)){
  sub<-a[which(a$file == levels(a$file)[i]),]
  char.count<-nrow(sub)
  filename<-as.character(sub$file)[1]
  temp.df<-data.frame(filename, char.count)
  char.df<-rbind(char.df, temp.df)
}
mean.char<-round(mean(char.df$char.count))

#get mean character counts for the top 85 characters for every novel
final.df<-NULL
for (i in 1:nlevels(a$file)){
  sub<-a[which(a$file == levels(a$file)[i]),]
  char.count<-sub$mentions_per_100K_words
  if (length(char.count) < mean.char){
    extra<-rep(0, mean.char-length(char.count))
    char.count<-append(char.count, extra)
  }
  if (length(char.count) > mean.char){
    char.count[1:mean.char]
  }
  final.df<-cbind(final.df, char.count)
}
avg.char<-rowMeans(final.df)
index<-c(1:85)

#plot
plot.df<-data.frame(index, avg.char)
ggplot(plot.df, aes(x=index, y=avg.char)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point() +
  #geom_line() +
  labs(x="Character Rank", y="Mentions (per 100K)", caption="\nFig. 5.1 Frequency of character occurrences by character rank\nin nineteenth-century novels.\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")
  #labs(x="Character Rank", y="Mentions (per 100K)")

## Calculating the avg. number of POS ##
all<-read.csv("LIWC_Contemporary_Novels.csv")
mean(all$ppron)
mean(all$verb)
mean(all$conj)
mean(all$Period)

### Table 5.2 ###
#this takes as input a directory called "Char_Vectors_Sample" which contains 
#tables that list the words associated with characters from a given novel or genre
temp = list.files(pattern="*.csv")
final.df<-NULL
for (i in 1:length(temp)){
  group<-temp[i]
  a<-read.csv(temp[i], sep="\t")
  a<-a[order(-a$PROB),]
  temp.df<-a[1:20,]
  temp.df$GROUP<-group
  final.df<-rbind(final.df, temp.df)
}

### Fig. 5.2 ###
#this tests how similar characters are to each other across novels versus how similar
#they are to the novels from which they come.
a<-read.csv("KL_Test_WithinNovelsBetweenNovels_AllTrials.csv")
a$x<-a$A.CHAR..vs..B.CHAR...1000.w.o.stop...comm..
a$y<-a$A.CHAR..vs..A.NON.CHAR...1000.w.o.stop...comm..
#observe for normality
hist(a$x, prob=T)
curve(dnorm(x, mean=mean(a$x), sd=sd(a$x)), add=TRUE)
hist(a$y, prob=T)
curve(dnorm(x, mean=mean(a$y), sd=sd(a$y)), add=TRUE)

#test for significance
t.test(a$A.CHAR..vs..B.CHAR...1000.w.o.stop...comm.., a$A.CHAR..vs..A.NON.CHAR...1000.w.o.stop...comm..)
wilcox.test(a$A.CHAR..vs..B.CHAR...1000.w.o.stop...comm.., a$A.CHAR..vs..A.NON.CHAR...1000.w.o.stop...comm..)

#plot
a.char<-a$A.CHAR..vs..B.CHAR...1000.w.o.stop...comm..
a.nonchar<-a$A.CHAR..vs..A.NON.CHAR...1000.w.o.stop...comm..
a.char<-data.frame(a.char, c("Between Novels"))
a.nonchar<-data.frame(a.nonchar, c("Within Novels"))
colnames(a.char)<-c("value", "category")
colnames(a.nonchar)<-c("value", "category")
b<-rbind(a.char, a.nonchar)
ggplot(b, aes(value, linetype = category)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.position="top") +
  theme(legend.title=element_blank()) +
  geom_density() +
  labs(x="Divergence", y="Density", caption="\nFig. 5.2 Information lost when comparing characters between novels versus\ncomparing characters to their own novels\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")
  #labs(x="Divergence", y="Density")

### Table 5.4 ###
#Lexical Homogeneity of Characters Within Novels
#this takes as input a table of the average similarities between the top-5 characters
#compared to each other and the top-5 nouns compared to each other for every novel
a<-read.csv("Lexical_Diversity_Cosine_All.csv", sep="\t")
final.df<-NULL
for (i in 2:nlevels(a$CLASS)){
  sub<-a[a$CLASS == levels(a$CLASS)[i],]
  if (nrow(sub) != 0){
    model<-t.test(sub$CHAR., sub$NOUN)
    class<-as.character(levels(a$CLASS)[i])
    mean.char<-model$estimate[[1]]
    mean.noun<-model$estimate[[2]]
    model.p<-model$p.value
    sd.char<-sd(sub$CHAR.)
    sd.noun<-sd(sub$NOUN)
    temp.df<-data.frame(class, mean.char, sd.char, mean.noun, sd.noun, model.p)
    final.df<-rbind(final.df, temp.df)
  }
}

### 5.2 ###
#### Lexical Diversity Over Narrative Time #####
#this takes as input a table that has calculated the cosine similarity between
#character vectors of the first half of a novel with those of the second for the top character of a novel;
#and then also the avg. similarity for noun vectors for the first half and second halves
#for the top 5 nouns
a<-read.csv("Lexical_Diversity_OverNarrativeTime.csv", sep="\t")
#observe for normality
hist(a$CHAR...TOP.1000.)
hist(a$NOUN..TOP.1000.)
#because of the non-normality of the data use a wilcoxon rank sum test
wilcox.test(a$CHAR...TOP.1000., a$NOUN..TOP.1000.)
median(a$CHAR...TOP.1000.)
median(a$NOUN..TOP.1000.)
#observe the different distributions
plot(density(a$CHAR...TOP.1000.))
lines(density(a$NOUN..TOP.1000.), lty="dotted")

###### Part Two: Perception / Cogitation Tests #####

### 5.3a ###

#Create normalized table for the introversion test
a<-read.csv("FeatureTable_ROM_VIC_Labeled.csv")
#first subtract the introvert scores from the extrovert scores for each axis
a$COMM<-a$COMMUNICATIVENESS-a$PERCEPTION
a$SOCIO<-a$SOCIABILITY-a$COGITATION
#then normalize by scaling the values
a.norm<-a[,13:ncol(a)]
a.norm<-apply(a.norm, 2, function(x) scale(x, center = T, scale=T))
a.new<-cbind(a[,1:12], a.norm)
#write.csv(a.new, file="FeatureTable_ROM_VIC_Normalized.csv")

### Fig. 5.4 ####
#Four Part Grid
library("ggplot2")
library("ggrepel")
grid<-read.csv("FeatureTable_ROM_VIC_Normalized.csv")
grid2<-grid[which(grid$Combination_Class2 == "Male_Male" | grid$Combination_Class2 == "Female_Female"), ]
grid.f<-grid[which(grid$Combination_Class == "Female_Female"),]
ggplot(grid.f, aes(x=SOCIO, y=COMM)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(size=1) +
  stat_ellipse(linetype="dotted") +
  theme(legend.position="none") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_text(aes(label=Austen_Title2), size=3, hjust=1.0, vjust=-0.5) +
  scale_x_continuous(limits=c(-3, 3)) +  
  scale_y_continuous(limits=c(-3, 3)) +
  labs(x="Cogitation <-> Sociability", y="Perception <-> Communication", caption="\nFig. 5.4 Ratio of introversion to extroversion in main characters in nineteenth-century\nnovels. Only novels with female authors and female protagonists are shown.\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")
  #labs(x="Cogitation <-> Sociability", y="Perception <-> Communication")

### 5.3 ###
#significance tests on Cogitative and Perceptual Vocabulary
a<-read.csv("FeatureTable_ROM_VIC_Labeled.csv")
#test for normality for all features
shapiro.test(a$COGITATION[a$Author_Gender=="m"])
shapiro.test(a$COGITATION[a$Author_Gender == "f"])
shapiro.test(a$PERCEPTION[a$Author_Gender=="m"]) #this one appears to be slightly not-normally distributed
shapiro.test(a$PERCEPTION[a$Author_Gender=="f"])
shapiro.test(a$PERCEPTION[a$Combination=="m_F"])
shapiro.test(a$PERCEPTION[a$Combination == "f_F"])
shapiro.test(a$COGITATION[a$Combination=="m_F"])
shapiro.test(a$COGITATION[a$Combination == "f_F"])

#test introversion in male and female writers
t.test(a$COG_PER[a$Author_Gender == "f"], a$COG_PER[a$Author_Gender == "m"])
sd(a$COG_PER[a$Author_Gender == "f"])
sd(a$COG_PER[a$Author_Gender == "m"])

#anova test for introversion across all 4 gender categories
summary(aov(a$COG_PER ~ a$Combination))

#visualize
boxplot(a$COG_PER ~ a$Combination, main="Author_Protagonist Relationship to Introversion\nMeasured by Gender")

#Tukey's test to identify which pair(s) have significantly elevated (depressed) levels of introversion
library(DTK)
results = DTK.test(a$COG_PER,a$Combination,0.05)
DTK.plot(results)

### 5.4 ###

#### Collocate Analysis #### 
#this script derives collocates of cogitative perceptual words
#it subsets collocates by words distinctive of a particular population
#and also by words that are more likely to be near a target word
#using mutual information scores

#it takes as input directories of texts where only sentences with main
#characters appear

library("tm")
library("SnowballC")

#get file list from directory
filenames<-list.files("Protag_Texts_Women_19C", pattern="*.txt", full.names=FALSE)
#input the keywords
dict<-read.csv("Cog_Per_Dict.csv", stringsAsFactors = F, header=F)
#this decides the window of words for collocates
window<-9 
#run
collocates.df<-NULL
for (i in 1:length(filenames)) {
  print(i)
  #scan in work and clean
  work<-scan(filenames[i], what="character", quote="")
  work.clean<-gsub("\\d", "", work)
  work.clean<-gsub("[[:punct:]]","", work.clean)
  work.clean<-tolower(work.clean)
  work.clean<-gsub("page", "", work.clean)
  not.blanks<-which(work.clean!="")
  work.clean<-work.clean[not.blanks]
  word.count<-length(work.clean)
  work.clean<-subset(work.clean, !(work.clean %in% stopwords("English"))) #optional: removes stopwords
  work.clean<-wordStem(work.clean, language="en")
  #for each keyword identify collocates
  for (k in 1:length(dict$V1)) {
    work.ont<-which(work.clean == as.character(dict[k,1])) # position of keyword
    work.collocates<-NULL
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
        emotion.gram<-as.character(dict[k,1])
        emotion.stem<-as.character(dict[k,2])
        temp.df<-data.frame(total.gram, emotion.gram, emotion.stem)
        work.collocates<-rbind(work.collocates, temp.df)
      }
      collocates.df<-rbind(collocates.df, work.collocates)
    }
  }
}
collocates.final<-data.frame(collocates.df$emotion.stem, collocates.df$total.gram)
collocates.weighted<-data.frame(table(collocates.final))
colnames(collocates.weighted)<-c("source", "target", "weight")
collocates.weighted$type<-c("undirected")
collocates.weighted<-collocates.weighted[collocates.weighted$weight > 0,]
#remove words less than 3 letters
collocates.weighted<-collocates.weighted[which(nchar(as.character(collocates.weighted$target)) > 2),]
#remove words that appear less than 11 times
collocates.weighted<-collocates.weighted[which(collocates.weighted$weight > 10),]
#write.csv(collocates.weighted, file="Character_Collocates_Women.csv", row.names=F)

#### subset collocates by distinctive words ####

#Step 1: Find Distinctive Words
#this process uses a Wilcoxon rank sum test to identify words that are distinctive 
#of one population versus another

#load corpus 1
corpus1 <- VCorpus(DirSource("Protag_Texts_Women_19C"), readerControl=list(language="English")) 
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, removeWords, stopwords("English"))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
scaling1<-rowSums(corpus1.matrix)

#load corpus2
corpus2 <- VCorpus(DirSource("Protag_Texts_Men_19C"), readerControl=list(language="English")) 
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, content_transformer(removePunctuation))
corpus2 <- tm_map(corpus2, content_transformer(removeNumbers))
corpus2 <- tm_map(corpus2, removeWords, stopwords("English"))
corpus2 <- tm_map(corpus2, stemDocument, language = "english")
corpus2.dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F) 
scaling2<-rowSums(corpus2.matrix)

#make joint dictionary
count1<-colSums(corpus1.matrix)
count1<-count1[which(count1>50)] #adjust integer here depending on your data
count2<-colSums(corpus2.matrix)
count2<-count2[which(count2>50)] #adjust integer here depending on your data
count1.frame<-as.data.frame(count1)
count2.frame<-as.data.frame(count2)
count.merged1<-subset(count1.frame, rownames(count1.frame) %in% rownames(count2.frame))
count.merged2<-subset(count2.frame, rownames(count2.frame) %in% rownames(count1.frame))
dict<-rownames(count.merged1)
length(dict)
#subset dtms by words in common
mann.matrix1<-corpus1.matrix[, colnames(corpus1.matrix) %in% dict]
mann.matrix2<-corpus2.matrix[, colnames(corpus2.matrix) %in% dict]
#scale
corpus1.scaled<-mann.matrix1/scaling1
corpus2.scaled<-mann.matrix2/scaling2
rho.matrix1<-corpus1.scaled
rho.matrix2<-corpus2.scaled
#produce statistic
final.df<-NULL
for (j in 1:ncol(rho.matrix1)){
  rnk<-wilcox.test(rho.matrix1[,j], rho.matrix2[,j])
  count.a<-sum(mann.matrix1[,j])
  med.a<-median(rho.matrix1[,j])
  count.b<-sum(mann.matrix2[,j])
  med.b<-median(rho.matrix2[,j])
  p<-rnk$p.value
  word<-colnames(rho.matrix1)[j]
  diff<-med.a-med.b
  temp.df<-data.frame(word, count.a, count.b, med.a, med.b, p, diff)
  final.df<-rbind(final.df, temp.df)
}
final.sort<-final.df[order(-final.df$diff),] #sort by difference in median value of word
final.sort<-final.sort[final.sort$p < 0.01,] #cut by p-value
final.sort<-final.sort[final.sort$diff > 0,] #cut by positive association w group a
#write.csv(final.sort, file="MDW_Women.csv", row.names=F)

#Step 2: subset collocates by distinctive words
all<-read.csv("Character_Collocates_SciFi.csv", stringsAsFactors = F)
dict<-read.csv("MDW_SciFi.csv", stringsAsFactors = F)
dict<-dict$word
sub<-all[all$target %in% dict,]
#write.csv(sub, file="Character_Collocates_Women_Distinctive.csv", row.names=F)

#Step 3: sort words by Pointwise Mutual Information score
#PMI = joint probability divided by marginal probability
#joint probability = probability of word pair occurring together in the collocate list divided by overall word count in corpus
#marginal probability = probability of each word in pair occurring in overall data set times each other
joint.df<-read.csv("Character_Collocates_Women_Distinctive.csv", stringsAsFactors = F)
dict<-append(unique(joint.df$source), unique(joint.df$target))
corpus1 <- VCorpus(DirSource("Protag_Texts_Women_19C"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")
#run first w all words to get overall word count
corpus1.dtm<-TermDocumentMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
total.words<-sum(corpus1.matrix)
#get probabilities of individual words
#sum each word's occurrence in all docs and divide by total words
all.raw<-as.data.frame(rowSums(corpus1.matrix))
all.matrix<-rowSums(corpus1.matrix)/total.words
all.df<-as.data.frame(all.matrix)
joint.final<-NULL
#for each keyword derive a ranked list of words that are most likely to be near that keyword using PMI score
for (i in 1:nlevels(as.factor(joint.df$source))){
  joint.sub<-joint.df[joint.df$source == levels(as.factor(joint.df$source))[i],]
  pmi.v<-vector()
  for (j in 1:nrow(joint.sub)){
    joint.prob<-joint.sub$weight[j]/total.words #joint probability = frequency of co-occurrence divided by total words
    prob.x<-all.df[row.names(all.df) == joint.sub$target[j],] #probability of source word in corpus
    prob.y<-all.df[row.names(all.df) == joint.sub$source[j],] #probability of target word in corpus
    pmi<-log(joint.prob/(prob.x*prob.y))
    pmi.v<-append(pmi.v, pmi)
  }
  joint.sub$pmi<-pmi.v
  joint.final<-rbind(joint.final, joint.sub)
}
#write.csv(joint.final, "Character_Collocates_Women_Distinctive_PMI.csv", row.names=F)

#sort by words that occur at least N times and collapse into single edge list
a<-read.csv("Character_Collocates_Women_Distinctive_PMI.csv")
a.sub<-a[a$weight > 50,]
a.sub<-a.sub[a.sub$target != "re",]
final.df<-NULL
for (i in 1:nlevels(as.factor(a.sub$source))){
  sub.s<-a.sub[a.sub$source == levels(as.factor(a.sub$source))[i],]
  sub.s<-sub.s[order(-sub.s$pmi),]
  sub.s<-sub.s[1:10,]
  final.df<-rbind(final.df, sub.s)
}
#write.csv(final.df, file="Character_Collocates_Women_Distinctive_PMI_EdgeList_Top10.csv")

### Fig. 5.5 ###
library(igraph)
edge<-read.csv("Character_Collocates_Women_Distinctive_PMI_EdgeList_Top10.csv", header=T)
edge.sub<-edge[,2:3]
g<-graph.data.frame(edge.sub, directed=FALSE, vertices=NULL)
E(g)$weight<-1
plot.igraph(g, layout=layout_with_gem, edge.width=E(g)$weight,
            edge.color="black", vertex.color="white", vertex.shape="none", vertex.label.color="black",
            vertex.label.cex=0.8)

### 5.5 ###

#### Futurity Tests ####

### 1. Look Forward, Wait, Await ###
#directories - run twice
#Protag_Texts_Men_19C
#Protag_Texts_Women_19C
filenames<-list.files("Protag_Texts_Women_19C", pattern="*.txt", full.names=FALSE)
wait.v<-vector()
await.v<-vector()
word.count.v<-vector()
look.forward.v<-vector()
for (i in 1:length(filenames)){
  work<-scan(filenames[i], what="character", quote="")
  work.clean<-gsub("\\d", "", work)
  work.clean<-gsub("[[:punct:]]","", work.clean)
  work.clean<-tolower(work.clean)
  work.clean<-gsub("page", "", work.clean)
  work.clean<-wordStem(work.clean, language="en")
  work.clean<-work.clean[work.clean != ""]
  word.count<-length(work.clean)
  wait<-length(grep("wait", work.clean))
  await<-length(grep("await", work.clean))
  look.forward<-0
  for (j in 1:(length(work.clean)-1)){
    if (work.clean[j] == "look" & work.clean[j+1] == "forward"){
      look.forward<-look.forward+1
    }
  }
  wait.v<-append(wait.v, wait)
  await.v<-append(await.v, await)
  look.forward.v<-append(look.forward.v, look.forward)
  word.count.v<-append(word.count.v, word.count)
}
df<-data.frame(filenames, wait.v, await.v, look.forward.v, word.count.v)
#write.csv(df, file="LookForward_Women.csv")

#compare men versus women
men<-read.csv("LookForward_Men.csv")
women<-read.csv("LookForward_Women.csv")
#create contingency table for each word

#             F   M
#Feature
#NotFeature

#extract total word counts
all1<-sum(women$word.count.v)
all2<-sum(men$word.count.v)

#extract feature counts
#change variables for men and women

#wait
# word1<-sum(women$wait.v) 
# word2<-sum(men$wait.v) 
# #await
# word1<-sum(women$await.v)
# word2<-sum(men$await.v)
# #look forward
# word1<-sum(women$look.forward.v)
# word2<-sum(men$look.forward.v)

##run fisher's exact test
not.word1<-all1-word1
not.word2<-all2-word2
cont.table<-data.frame(array(0, c(2,2)))
cont.table[1,1]<-word1
cont.table[1,2]<-word2
cont.table[2,1]<-not.word1
cont.table[2,2]<-not.word2
fisher.test(cont.table)

### 2. POS and Dictionary ###
#this script uses POS analysis and custom dictionaries to better understand the relationship between past, present and future tenses
#in novels by men and women with male and female protagonists respectively
library("NLP")
library("openNLP")
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en")
filenames<-list.files("Women_Novel_Protag_Texts", pattern="*.txt", full.names=FALSE)
final.df<-NULL
for (i in 1:length(filenames)) {
  work<-scan(filenames[i], what="character", quote="") # load novel, separate chunks by line breaks
  if (length(work) < 290000){
    work.clean<- gsub("\\d", "", work)
    work<- tolower(work)
    work<-strsplit(work, "\\W")
    work<-unlist(work)
    not.blanks<-which(work!="") #which parts of the vector are not blanks (punctuation became blanks)
    work<-work[not.blanks]
    text.whole<-paste(work.clean, collapse=" ") # collapse into single chunk
    text.char<-as.String(text.whole)
    a2 <- annotate(text.char, list(sent_token_annotator, word_token_annotator))
    a3 <- annotate(text.char, pos_tag_annotator, a2)
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    #define past tense
    tags.past1 <- which(tags == "VBD")
    tags.past2 <- which(tags == "VBN")
    #present tense
    tags.present1<-which(tags == "VBG")
    tags.present2<-which(tags == "VBP")
    tags.present3<-which(tags == "VBZ")
    past<-length(tags.past1)+length(tags.past2)
    present<-length(tags.present1)+length(tags.present2)+length(tags.present3)
    #future tense
    future1<-which(work == "will")
    future2<-which(work == "forward")
    future3<-which(work == "future")
    future4<-which(work == "shall")
    future5<-which(work == "ought")
    future6<-which(work == "might")
    future7<-which(work == "must")
    past.per<-past/length(work)
    present.per<-present/length(work)
    future.per<-(length(future1)+length(future2)+length(future3)+length(future4)+length(future5)+length(future6)+length(future7))/length(work)
    future<-length(future1)+length(future2)+length(future3)+length(future4)+length(future5)+length(future6)+length(future7)
    past.present.ratio<-past/present
    title<-filenames[i]
    word.count<-length(work)
    temp.df<-data.frame(title, word.count, past, present, past.per, present.per, future, future.per, past.present.ratio)
    final.df<-rbind(final.df, temp.df)
  }
}
#write.csv(final.df, file="Futurity_Women_Protag.csv")

#compare men and women related to futurity
a<-read.csv("Futurity_Women_Protag.csv")
b<-read.csv("Futurity_Men_Protag.csv")

#fisher's test to compare levels of futurity and pastness in texts male and female protagonists
#example of the contingency table
#         F   M
#Fut
#notFut
m.fut<-sum(b$future.per*b$word.count)
f.fut<-sum(a$future.per*a$word.count)
m.not<-sum(b$word.count)-m.fut
f.not<-sum(a$word.count)-f.fut
df<-data.frame(c(f.fut, f.not), c(m.fut, m.not))
colnames(df)<-c("F", "M")
row.names(df)<-c("futurity", "notFuturity")
fisher.test(df)

#to compare past tense
m.past<-sum(b$past)
f.past<-sum(a$past)
m.not<-sum(b$word.count)-m.past
f.not<-sum(a$word.count)-f.past
df<-data.frame(c(f.past, f.not), c(m.past, m.not))
colnames(df)<-c("F", "M")
row.names(df)<-c("past", "notPast")
fisher.test(df)

### 5.6 ###
## Testing whether G Eliot's use of strong is markedly different
#first, ingest a table of sentences with female characters in which one of the five keywords appears
a<-read.csv("Cog_Per_Sentences_Women_SubTargetWords.csv")
#keep only those sentences in which "strong" appears
a.sub<-a[a$target == "strong",]
a.non<-a[a$target != "strong",]
#divide by George Eliot and the rest
eliot<-a.sub[grep("Eliot", a.sub$work),]
not.eliot<-a.sub[-grep("Eliot", a.sub$work),]
eliot.split<-unlist(strsplit(as.character(eliot$sent.all), " "))
eliot.split<-gsub("[[:punct:]]","", eliot.split)
eliot.split<-tolower(eliot.split)
not.blanks<-which(eliot.split!="") #which parts of the vector are not blanks (punctuation became blanks)
eliot.split<-eliot.split[not.blanks]
#not eliot
not.split<-unlist(strsplit(as.character(not.eliot$sent.all), " "))
not.split<-gsub("[[:punct:]]","", not.split)
not.split<-tolower(not.split)
not.blanks<-which(not.split!="") #which parts of the vector are not blanks (punctuation became blanks)
not.split<-not.split[not.blanks]
#
all.eliot<-data.frame(table(eliot.split))
all.not<-data.frame(table(not.split))
colnames(all.eliot)<-c("word", "freq")
colnames(all.not)<-c("word", "freq")
all<-merge(all.eliot, all.not, by="word", all = T)
all[is.na(all)]<-0
all1<-sum(all$freq.x)
all2<-sum(all$freq.y)
#find words that are distinctive of George Eliot's use of strong
#i.e. which words appear in sentences w strong for George Eliot versus other women writers
fisher.df<-NULL
for (i in 1:nrow(all)){
  word<-as.character(all$word)[i]
  word1<-all[i,2]
  word2<-all[i,3]
  not.word1<-all1-word1
  not.word2<-all2-word2
  cont.table<-data.frame(array(0, c(2,2)))
  cont.table[1,1]<-word1
  cont.table[1,2]<-word2
  cont.table[2,1]<-not.word1
  cont.table[2,2]<-not.word2
  fish<-fisher.test(cont.table)
  fish.p<-fish$p.value
  fish.odds<-fish$estimate[[1]]
  temp.df<-data.frame(word, word1, word2, fish.odds, fish.p)
  fisher.df<-rbind(fisher.df, temp.df)
}
fisher.final<-fisher.df[fisher.df$fish.odds > 1, ]
fisher.final<-fisher.final[order(-fisher.final$fish.odds),]
fisher.final<-fisher.final[fisher.final$fish.p < 0.05,]
#write.csv(fisher.final, file="MDW_Eliot_Strong.csv")

#test pos/neg sentiment associated with  Eliot's sentences w 'strong'
pos<-read.csv("Bing_Sentiment_Positive.csv", header=F)
neg<-read.csv("Bing_Sentiment_Negative.csv", header=F)
fisher.pos<-fisher.final[as.character(fisher.final$word) %in% as.character(pos$V1),]
fisher.neg<-fisher.final[as.character(fisher.final$word) %in% as.character(neg$V1),]
eliot.ratio<-sum(fisher.neg$word1)/sum(fisher.pos$word1)
#run for words distinctive of other novelists
fisher2<-fisher.df[fisher.df$fish.odds < 1, ]
all.pos<-fisher2[as.character(fisher2$word) %in% as.character(pos$V1),]
all.neg<-fisher2[as.character(fisher2$word) %in% as.character(neg$V1),]
all.ratio<-sum(all.neg$word1)/sum(all.pos$word1)
all.ratio<-sum(all.pos$word1)/sum(all.neg$word1)

### 5.7 ###
### Windows ###
#this examines the place of windows in women's novels
a<-read.csv("Cog_Per_Sentences_Women_SubTargetWords.csv")
#subset sentences by containing the word window
a.sub<-a[a$target == "window",]
a.non<-a[a$target != "window",]
#turn into word vector -- these are all words in sentences w the target word
asub.split<-unlist(strsplit(as.character(a.sub$sent.all), " "))
asub.split<-gsub("[[:punct:]]","", asub.split)
asub.split<-tolower(asub.split)
asub.split<-asub.split[asub.split != ""]
#ditto
not.split<-unlist(strsplit(as.character(a.non$sent.all), " "))
not.split<-gsub("[[:punct:]]","", not.split)
not.split<-tolower(not.split)
not.split<-not.split[not.split != ""]
#merge
all.target<-data.frame(table(asub.split))
all.not<-data.frame(table(not.split))
colnames(all.target)<-c("word", "freq")
colnames(all.not)<-c("word", "freq")
all<-merge(all.target, all.not, by="word", all = T)
all[is.na(all)]<-0
#subset by words occurring more than X times
all<-all[all$freq.x > 5,]
all1<-sum(all$freq.x)
all2<-sum(all$freq.y)
#find words that are distinctive of windows
#i.e. which words appear in sentences w "window" versus those without window
fisher.df<-NULL
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}
for (i in 1:nrow(all)){
  word<-as.character(all$word)[i]
  word1<-all[i,2]
  word2<-all[i,3]
  not.word1<-all1-word1
  not.word2<-all2-word2
  cont.table<-data.frame(array(0, c(2,2)))
  cont.table[1,1]<-word1
  cont.table[1,2]<-word2
  cont.table[2,1]<-not.word1
  cont.table[2,2]<-not.word2
  fish<-fisher.test(cont.table)
  fish.p<-fish$p.value
  fish.odds<-fish$estimate[[1]]
  G2 = 2*sum(cont.table)*(H(cont.table)-H(rowSums(cont.table))-H(colSums(cont.table)))
  temp.df<-data.frame(word, word1, word2, fish.odds, fish.p, G2)
  fisher.df<-rbind(fisher.df, temp.df)
}
fisher.final<-fisher.df[fisher.df$fish.odds > 1, ]
fisher.final<-fisher.final[order(-fisher.final$fish.odds),]
fisher.final<-fisher.final[fisher.final$fish.p < 0.05,]
fisher.final<-fisher.final[fisher.final$word1 > 20,] #only keep words that appear more than 20x
#write.csv(fisher.final, file="MDW_Window.csv")

#test for whether sentences w windows have considerably more positive words and/or considerably more negative words
#takes table "all" from above
#runs Fisher's Exact test to aggregate sentence-level behavior
#sentiment analysis works better at broader scale than single sentences
pos<-read.csv("Bing_Sentiment_Positive.csv", header=F)
neg<-read.csv("Bing_Sentiment_Negative.csv", header=F)
all.pos<-all[as.character(all$word) %in% as.character(pos$V1),]
all.neg<-all[as.character(all$word) %in% as.character(neg$V1),]
#run twice, once for positive and once for negative
#positive
word1<-sum(all.pos$freq.x)
word2<-sum(all.pos$freq.y)
not.word1<-sum(all$freq.x)-word1
not.word2<-sum(all$freq.y)-word2
cont.table<-data.frame(array(0, c(2,2)))
cont.table[1,1]<-word1
cont.table[1,2]<-word2
cont.table[2,1]<-not.word1
cont.table[2,2]<-not.word2
fisher.test(cont.table)
#negative
word1<-sum(all.neg$freq.x)
word2<-sum(all.neg$freq.y)
not.word1<-sum(all$freq.x)-word1
not.word2<-sum(all$freq.y)-word2
cont.table<-data.frame(array(0, c(2,2)))
cont.table[1,1]<-word1
cont.table[1,2]<-word2
cont.table[2,1]<-not.word1
cont.table[2,2]<-not.word2
fisher.test(cont.table)

### 5.8 ###
### The Male Gaze ###
#do men appear more or less often when women are by windows?
#takes "all" from above
a<-read.csv("Cog_Per_Sentences_Women_SubTargetWords.csv")
#subset sentences by containing the word window
win<-a[a$target == "window",]
non.win<-a[a$target != "window",]
#find those sentences with windows and men
win.men<-NULL
for (m in 1:nrow(win)){
  sentence.split<-unlist(strsplit(as.character(win$sent.all[m]), " "))
  sentence.split<-gsub("[[:punct:]]","", sentence.split)
  sentence.split<-tolower(sentence.split)
  not.blanks<-which(sentence.split!="") #which parts of the vector are not blanks (punctuation became blanks)
  sentence.split<-sentence.split[not.blanks]
  if (length(which(sentence.split == "he" | sentence.split == "his" | sentence.split == "him")) != 0) {
    win.men<-rbind(win.men, win[m,])
  }
}
#sentences without windows but with men
non.win.men<-NULL
for (m in 1:nrow(non.win)){
  sentence.split<-unlist(strsplit(as.character(non.win$sent.all[m]), " "))
  sentence.split<-gsub("[[:punct:]]","", sentence.split)
  sentence.split<-tolower(sentence.split)
  not.blanks<-which(sentence.split!="") #which parts of the vector are not blanks (punctuation became blanks)
  sentence.split<-sentence.split[not.blanks]
  if (length(which(sentence.split == "he" | sentence.split == "his" | sentence.split == "him")) != 0) {
    non.win.men<-rbind(non.win.men, non.win[m,])
  }
}
#sentences with windows but without men
win.notmen<-nrow(win)-nrow(win.men)
#sentences with neither windows nor men
notwin.notmen<-nrow(non.win)-nrow(non.win.men)
cont.table<-data.frame(array(0, c(2,2)))
cont.table[1,1]<-win.notmen
cont.table[1,2]<-nrow(win.men)
cont.table[2,1]<-notwin.notmen
cont.table[2,2]<-nrow(non.win.men)
fisher.test(cont.table)

### 5.9 ###
#### Comparing 19C interiority w twentieth century novels ###
a<-read.csv("FeatureTable_All_Protagonist.csv")
C19<-a[a$COLLECTION == "stanford",]
C20<-a[a$COLLECTION == "klab",]
C19$COG_PER<-((C19$COGITATION)/100)+C19$PERCEPTION
C20$COG_PER<-((C20$COGITATION)/100)+C20$PERCEPTION
C20.samp<-C20[sample(nrow(C20), nrow(C19)),]
wilcox.test(C19$COG_PER, C20.samp$COG_PER)
median(C19$COG_PER)
median(C20.samp$COG_PER)
var.test(C19$COG_PER, C20.samp$COG_PER)

#compare features of high COG_PER texts
a<-read.csv("FeatureTable_20C.csv")
#keep statistically significant characters in the cog-per feature
#a.sub<-a[a$cog_per_norm > 1.959,]
a.sub<-a[order(-a$cog_per),]
a.sub<-a[1:round(0.05*nrow(a)),]
#what percentage of these characters/authors are male?
length(which(as.character(a.sub$AUTH_GENDER) == "M"))/nrow(a.sub)
length(which(as.character(a.sub$PROTAG..GENDER) == "M"))/nrow(a.sub)
#compare to 19C
b<-read.csv("FeatureTable_ROM_VIC_Normalized.csv")
b<-b[order(-b$COG_PER),]
b<-b[1:round(0.05*nrow(b)),]
length(which(b$Author_Gender == "m"))/nrow(b)
length(which(b$PROTAG.GENDER == "M"))/nrow(b)

#odds ratio of female-female to male-male across 19C-20C

#1. odds ratio of having female-female in the high-introversion group 19C v. 20C

#       19C    20C
#FF
#Not

#remove empty cells
a.sub<-a.sub[1:202,]
ff.19<-length(which(b$Combination == "f_F"))
not.19<-nrow(b)-ff.19
ff.20<-nrow(a.sub[which(as.character(a.sub$AUTH_GENDER) == "F" & as.character(a.sub$PROTAG..GENDER) == "F"),])
not.20<-nrow(a.sub)-ff.20
df<-data.frame(c(ff.19, not.19), c(ff.20, not.20))
fisher.test(df)

#2. odds ratio of having male-male in high introversion group in 19C v. 20C
mm.19<-length(which(b$Combination == "m_M"))
not.19<-nrow(b)-mm.19
mm.20<-nrow(a.sub[which(as.character(a.sub$AUTH_GENDER) == "M" & as.character(a.sub$PROTAG..GENDER) == "M"),])
not.20<-nrow(a.sub)-mm.20
df<-data.frame(c(mm.20, not.20), c(mm.19, not.19))
fisher.test(df)

### Table 5.6 ###
sort(table(a.sub$GENRE.1)/nrow(a.sub), decreasing = T)

### 5.10 ###
### Contemporary Novels ###
a<-read.csv("FeatureTable_Contemporary.csv")

#analysis of variance test on all six genres 
summary(aov(cog_per ~ GENRE, data=a))
hist(a$cog_per)
levels(a$GENRE)

#increase in scifi versus all genres
median(a[a$GENRE == "scifi",]$cog_per)-median(a[a$GENRE != "scifi",]$cog_per)

#increase in scifi minus YA and Romance
b<-a[which(a$GENRE != "romance" & a$GENRE != "young-adult"),]
b$GENRE<-factor(b$GENRE)
median(b[b$GENRE == "scifi",]$cog_per)-median(b[b$GENRE != "scifi",]$cog_per)

library(DTK)
results = DTK.test(a$cog_per,a$GENRE,0.05)
DTK.plot(results)

### Fig. 5.6 ###
#Collocate network for science fiction
library(igraph)
edge<-read.csv("Character_Collocates_SciFi_Distinctive_PMI_EdgeList_Top10.csv", header=T)
edge.sub<-edge[,2:3]
g<-graph.data.frame(edge.sub, directed=FALSE, vertices=NULL)
E(g)$weight<-1
plot.igraph(g, layout=layout_with_gem, edge.width=E(g)$weight,
            edge.color="black", vertex.color="white", vertex.shape="none", vertex.label.color="black",
            vertex.label.cex=0.8)

