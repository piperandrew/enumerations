### Chap 1 Punctuation ###
library(stringr)
library(tm)
library(SnowballC)
library(zoo)
library(splitstackshape)
library(ggplot2)
library(scales)
library(gridExtra)

### 1.0 ###
### Extracting punctuation ###
##Demonstration only##

#this section counts punctuation marks by word and line (if poems have line breaks)
#for either a group of works or a single work that has been divided into smaller chunks
#it does not record all punctuation marks, but limits itself to a small set of primary marks:
#these include: ?, !, ., , ;, : ()
#it takes as input a directory of works and outputs a table of marks per word/line
#This is the code used to extract punctuation from the following data sets:
#txtLAB450, Novel_19C, Novel_20C, Poetry_19C, Poetry_20C

#Only the derived data is shared. The code is presented for use on other data sets.
#Turn off lines related to "line breaks" for non-poetry
filenames<-list.files("20CPoetryAll", pattern="*.txt", full.names=FALSE)
punctuation.dtm<-NULL
for (i in 1:length(filenames)) {
  #load poem
  work<-scan(filenames[i], what="character", quote="")
  #load second version separating by line breaks
  work2<-scan(filenames[i], what="character", quote="", sep = "\n")
  if (length(work) > 0){
    no.lines<-(length(work2)/2)-1
    punct<-grep("\\?|\\!|\\.|\\,|\\;|\\:|\\(|\\)", work)
    work.punct<-str_extract(work, "\\?|\\!|\\.|\\,|\\;|\\:|\\(|\\)")
    punct2<-work.punct[!is.na(work.punct)]
    #calculate total words
    work.lower<-tolower(work) # all lower case
    work.words<-strsplit(work.lower, "\\W") # turn into a list of words
    work.word.vector<-unlist(work.words) #turn into a vector
    work.word.vector<-gsub("\\d", "", work.word.vector) #remove numbers
    work.word.vector<-work.word.vector[which(work.word.vector!="")]#only keeps parts of vector with words
    total.words<-length(work.word.vector) #total words in the novel
    #frequency of a given punctuation mark
    quest<-length(grep("\\?", punct2))/total.words
    quest.line<-length(grep("\\?", punct2))/no.lines
    exclam<-length(grep("\\!", punct2))/total.words
    exclam.line<-length(grep("\\!", punct2))/no.lines
    period<-length(grep("\\.", punct2))/total.words
    period.line<-length(grep("\\.", punct2))/no.lines
    comma<-length(grep("\\,", punct2))/total.words
    comma.line<-length(grep("\\,", punct2))/no.lines
    total<-length(punct)/total.words
    total.line<-length(punct)/no.lines
    #novel.dtm<-data.frame(filenames[i], total.words,quest, exclam, period, comma, total)
    novel.dtm<-data.frame(filenames[i], total.words, no.lines, quest, quest.line, exclam, exclam.line, period, period.line, comma, comma.line, total, total.line)
    punctuation.dtm<-rbind(punctuation.dtm, novel.dtm)
  }
}

### 1.1 ###
#compute the ratio of commas to periods
a<-read.csv("Punctuation_Novels_450.csv")
#subset by language
a<-a[grep("DE", a$filenames, ignore.case=FALSE),]
#or subset by work
#a<-a[grep("Lesabendio", a$filenames.i.),]
#calculate ratio
ratio<-a$comma/a$period
mean(ratio)

### 1.2 ###
#Periods in Paul Scheerbart's Lesabendio
work<-scan("DE_1913_Scheerbart,Paul_LesabendioEinAsteroiderRoman_Novel.txt", what="character", quote="")
punctuation.df<-NULL
for(i in seq(from=1, to=(length(work)-1000), by=100)){
  #establish 1,000 word window
  work.sub<-work[i:(i+1000)]
  #extract periods
  work.punct<-str_extract(work.sub, "\\.")
  punct<-which(!is.na(work.punct))
  #calculate percentage of periods relative to number of words
  period<-(length(punct)/length(work.sub))*100
  temp.df<-data.frame(i, period)
  punctuation.df<-rbind(punctuation.df, temp.df)
}
punctuation.df$i<-punctuation.df$i+999
#create rolling mean
punctuation.df$roll.mean<-rollmean(punctuation.df$period, k=10, na.pad=TRUE)

### Fig. 1.1 ###
#if you get an error unload your packages and reload these
library(ggplot2)
library(scales)
t<-ggplot(punctuation.df, aes(x=i, y=roll.mean)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  scale_x_continuous(labels=comma) +
  labs(x="Words in the Novel", y="Percentage of Periods", caption="\nFig. 1.1 Rolling mean of the percentage of periods\nwithin 1,000-word windows of the novel Lesabéndio\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")
  #labs(x="Words in the Novel", y="Percentage of Periods")

### Fig. 1.2 ###
#plot 1
novel<-read.csv("Punctuation_Novel_19C_20C_ByYear.csv")
novel.period<-data.frame(novel[,1], novel[,4])
novel.period$label<-c("period")
colnames(novel.period)<-c("year", "frequency", "label")
novel.comma<-data.frame(novel[,1], novel[,5])
novel.comma$label<-c("comma")
colnames(novel.comma)<-c("year","frequency", "label")
novel.df<-rbind(novel.period, novel.comma)
novel.df<-novel.df[-which(novel.df$frequency == min(novel.df$frequency)),]

p1<-ggplot(novel.df, aes(x=year, y=frequency*100, shape=label)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.title=element_blank(), legend.position = c(.85, .20), legend.text=element_text(size=10), legend.background = element_rect(fill="transparent")) +
  theme(axis.title.x=element_blank())+
  geom_point(size=1) +
  scale_shape_manual(values=c(1,17)) + #17=solid triangles, 1=empty circles
  xlim(1795, 1995) +
  labs(title = "Novels", y="Percentage") +
  stat_smooth(method=loess, colour="black")

#plot 2
C19<-read.csv("Punctuation_Poetry_19C_ByYear.csv")
C20<-read.csv("Punctuation_Poetry_20C_ByYear.csv")
all<-rbind(C19, C20)
all.sub<-all[all$variable == "period" | all$variable == "comma",]

p2<-ggplot(all.sub, aes(x=date.adj, y=means*100, shape=variable)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank()) +
  geom_point(size=1) +
  scale_shape_manual(values=c(1,17)) + #17=solid triangles, 1=empty circles
  #ylim(0, 15) +
  xlim(1795, 1995) +
  labs(title = "Poetry",y="Percentage") +
  stat_smooth(method=loess, colour="black")

#plot3
all2<-all[all$variable == "total",]
all2$label<-c("All Punctuation")

p3<-ggplot(all2, aes(x=date.adj, y=means*100, shape=label)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.position="none") +
  geom_point(size=1) +
  #coord_fixed() +
  scale_shape_manual(values=c(18)) + 
  xlim(1795, 1995) +
  #labs(title = "Poetry (All Punctuation)",x="Year", y="Percentage", caption="\nFig. 1.2 Percentage of punctuation in\nEnglish poetry and the novel, 1790-1990\n\nSource: Andrew Piper, Enumerations:\nThe Quantities of Literature (2018)") +
  labs(title = "Poetry (All Punctuation)",x="Year", y="Percentage") +
  stat_smooth(method=loess, colour="black")

grid.arrange(p1, p2, p3, ncol=1)

### 1.3 ###
#do periods increase over the course of novels?
#this test uses the NOVEL750 data set and the NOVEL_20C
#it calculates the percentage of periods in sliding 1000-word windows and then runs a linear model and calculates the slope
#output located in Punctuation_PeriodIncrease_Novels.csv

#run1 w 20C Data
#run2 w 19C Data

final.df<-NULL
for (j in 1:length(filenames$FILENAME)){
  print(j)
  work<-scan(as.character(filenames$FILENAME)[j], what="character", quote="")
  if (length(work) > 25000){
  work.df<-NULL
  for(i in seq(from=1, to=(length(work)-1000), by=100)){
    #establish 1,000 word window
    work.sub<-work[i:(i+1000)]
    #extract periods
    work.punct<-str_extract(work.sub, "\\.")
    punct<-which(!is.na(work.punct))
    #calculate percentage of periods relative to number of words
    period<-(length(punct)/length(work.sub))*100
    temp.df<-data.frame(i, period)
    work.df<-rbind(work.df, temp.df)
  }
  work.df$i<-work.df$i+999
  #create linear model1
  lm.model<-lm(period ~ i, data=work.df)
  if (lm.model$coefficients[[2]] < 0){id<-c("neg")} else {id<-c("pos")}
  #store results
  title<-as.character(filenames$FILENAME)[j]
  temp.df<-data.frame(title, id)
  final.df<-rbind(final.df, temp.df)
  }
}

#on first run add C20
#on second run add C19
#all.df<-NULL
#final.df$collection<-c("C20")
final.df$collection<-c("C19")
all.df<-rbind(all.df, final.df)
#ratio of positive to negative slopes
table(final.df$id)[[1]]/table(final.df$id)[[2]]

#load table
all.df<-read.csv("Punctuation_PeriodIncrease_Novels.csv")
pos.19<-data.frame(nrow(all.df[which(all.df$collection == "C19" & all.df$id == "pos"),]), c("19C"), c("Increase"))
neg.19<-data.frame(nrow(all.df[which(all.df$collection == "C19" & all.df$id == "neg"),]), c("19C"), c("Decrease"))
pos.20<-data.frame(nrow(all.df[which(all.df$collection == "C20" & all.df$id == "pos"),]), c("20C"), c("Increase"))
neg.20<-data.frame(nrow(all.df[which(all.df$collection == "C20" & all.df$id == "neg"),]), c("20C"), c("Decrease"))
colnames(pos.19)<-c("value", "collection", "direction")
colnames(neg.19)<-c("value", "collection", "direction")
colnames(pos.20)<-c("value", "collection", "direction")
colnames(neg.20)<-c("value", "collection", "direction")
m<-rbind(pos.19, neg.19, pos.20, neg.20)

### Fig. 1.3 ###

ggplot(m, aes(x=collection, y=value, fill = direction)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.position="bottom", legend.title=element_blank()) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  #theme(axis.title=element_blank()) +
  scale_fill_manual(values=c("black", "white")) +
  labs(x="Century", y="Novels", caption="\nFig. 1.3 Number of novels that show an increase or decrease\nof periods over the course of their narratives\n\nSource: Andrew Piper, Enumerations:\nData and Literary Study (2018)")
  #labs(x="Century", y="Novels")

### 1.4 ###
#observe if there is a racial bias in the high period group
#are high period poems indicative of African-American writing?
#meaning are Af-Am writers over-represented in the HP group relative to their overall
#frequency in the corpus?
#this uses a Fisher's exact test to calculate

###Step 1: Establish High Period Poems (HP)
#For a whole population observe the distribution of a given punctuation mark
a<-read.csv("Punctuation_Poetry_20C_All.csv")
summary(a$period)

#how many poets are there in the collection?
nlevels(a$author)

#how many poems have 0 periods?
length(which(a$period == 0))

#how many poems have periods 3 standard deviations above the mean? This is defined as our High Period group (HP)
#first remove poems above 50% periods (removes 8 poems)
a.sub<-a[a$period < 0.5,]
#establish cut-off
cut<-mean(a.sub$period)+(3*(sd(a.sub$period)))
#how many poems are in this group?
length(which(a.sub$period > cut))
#create group
hp<-a.sub[a.sub$period > cut,]

### Step2: load author data
authors<-read.csv("20CPoetryAll_Authors.csv")
#refactorize the authors in HP group
hp$author<-factor(as.character(hp$author))
hp.merge<-merge(hp, authors, by="author")
a.merge<-merge(a.sub, authors, by="author")
#to run we create a 2x2 that compares the rate of African Americans in HP v. their rate in the overall population
all1<-nrow(hp.merge) #size of population of HP group
all2<-nrow(a.merge) #size of population of whole group
group1<-length(which(hp.merge$ethnicity == "African-American")) #Af-Am poems in HP group
group2<-length(which(a.merge$ethnicity == "African-American")) #Af-am poems in whole group
not.group1<-all1-group1 #non-af-am poems in HP group
not.group2<-all2-group2 #non-afam poems in whole group
cont.table<-data.frame(array(0, c(2,2))) #construct the contingency table
cont.table[1,1]<-group1
cont.table[1,2]<-group2
cont.table[2,1]<-not.group1
cont.table[2,2]<-not.group2
fisher.test(cont.table) #run the test
#results = 
#odds ratio = 2.823
#pvalue < 2.2e-16

### Fig. 1.4 ###
#histogram of periods per words in the twentieth century
#this figure takes the table "a.sub" from above
#this is the cleaned punctuation table for Poetry_20C
#transform fractions into percentages
a.sub$period.per<-a.sub$period*100
#create density plot
ggplot(a.sub, aes(period.per)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(legend.position="none") +
  #geom_freqpoly()+
  geom_histogram(bins=50, color = "white", fill="black") +
  #geom_density() +
  labs(x="Percentage", y="Number of poems", caption="\nFig. 1.4 Percentage of periods per poem in twentieth-century poetry\n\nSource: Andrew Piper, Enumerations:\nData and Literary Study (2018)")
  #labs(x="Percentage of Periods", y="Number of poems") 

#between what two values is 50% of the data accounted for?
quantile(a.sub$period.per, probs = c(.25, .75))
quantile(a.sub$period.per, probs = c(.05, .95))
quantile(a.sub$period.per, probs = c(.01, .99))

### 1.5 ###

##### WORDS ASSOCIATED W PERIODS #####
#this script finds words that are associated with periods, i.e. that are more likely to precede a period
#uses POETRY_20C
filenames<-list.files("20CPoetryAll", pattern="*.txt", full.names=FALSE)
endings<-vector()
for (i in 1:length(filenames)) {
  print(i)
  work<-scan(filenames[i], what="character", quote="")
  if (length(work) > 0){
    #extract words next to periods
    work.sub<-work[grep("\\.", work)]
    #clean
    work.sub<-tolower(work.sub) #lowercase
    work.sub<-unlist(strsplit(work.sub, "\\W")) #remove the punctuation
    work.sub<-gsub("\\d", "", work.sub) #remove numbers
    work.sub<-work.sub[which(work.sub!="")]
    endings<-append(endings, work.sub)
  }
}
endings.df<-as.data.frame(sort(table(endings), decreasing = T))
endings.df<-endings.df[,-1]
colnames(endings.df)<-c("words", "freq")
#normalize by total number of words
endings.df$normal<-endings.df$freq/length(endings)
#then compare to these words frequency within the collection as a whole
#limit to words that appear in more than 2% of all poems (1500x)
endings.top<-endings.df[endings.df$freq > 1499,]
words<-as.character(endings.top$words)
corpus1 <- VCorpus(DirSource("20CPoetryAll"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1.dtm<-TermDocumentMatrix(corpus1, control=list(wordLengths=c(1,Inf), dictionary=words))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
word.counts<-data.frame(rowSums(corpus1.matrix))
word.counts<-cbind(row.names(word.counts), word.counts)
colnames(word.counts)<-c("words", "freq")
word.counts$all.normal<-word.counts$freq/sum(word.counts$freq)
final.df<-merge(endings.top, word.counts, by = "words")
colnames(final.df)<-c("words", "freq.period", "period.normal", "freq.all", "all.normal")

#run a Fisher's Exact Test to find increased likelihood of a word occurring before
#a period than in the corpus as a whole

#this takes the form

#         afterPeriod   notAfterPeriod
# word
# notWord

all1<-sum(final.df$freq.period)
all2<-sum(final.df$freq.all)
fisher.df<-NULL
for (i in 1:nrow(final.df)){
  word<-as.character(final.df$words)[i]
  word1<-final.df$freq.period[[i]]
  word2<-final.df$freq.all[[i]]
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
fisher.final<-fisher.final[fisher.final$fish.p < 0.05/(nrow(fisher.df)),]
fisher.final<-fisher.final[order(-fisher.final$fish.odds),]
#write.csv(fisher.final, file="Punctuation_Words_Before_Periods_20C.csv")

### Fig. 1.5 ###
#create a wordcloud of words that are more likely to come at the end of a sentence
library("wordcloud")
results<-read.csv("Punctuation_Words_Before_Periods_20C.csv")
#size of words based on squared odds-ratio
wordcloud(results$word, results$fish.odds*results$fish.odds, min.freq = 0, random.order=F, rot.per=0)

### 1.6 ###

#### DISTINCTIVE FEATURES ####
#these steps look for statistically distinctive features of the high period corpus using a Fisher's Exact Test

#divide tables into two groups - High Period Poems and the rest
a<-read.csv("Punctuation_Poetry_20C_All.csv")
#remove poems with more than 50% periods as outliers (removes 8 poems)
a.sub<-a[a$period < 0.5,]
#keep only those poems that are 3sd above the mean of all poems
cut<-mean(a.sub$period)+(3*(sd(a.sub$period)))
a.sub<-a.sub[a.sub$period > cut,]
#sort by word count
a.sub<-a.sub[order(-a.sub$total.words),]
#remove three long poems as this will skew word counts
a.sub<-a.sub[4:nrow(a.sub),]
#create HP group
filenames1<-as.character(a.sub$filenames)
#create not HP group
a.sub2<-a[a$period < cut,]
filenames2<-as.character(a.sub2$filenames)[!as.character(a.sub2$filenames) %in% filenames1] 

#ingest .RData "20CPoetry.RData"
#these tables were made using the following functions
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.dtm.sparse<-removeSparseTerms(corpus1.dtm, .99) #only used for words

#run for each DTM (words, POS, hypernyms)
#corpus1.matrix<-as.matrix(poetry_20C.words.dtm)
corpus1.matrix<-as.matrix(poetry_20C.pos.dtm)
#corpus1.matrix<-as.matrix(poetry_20C.hypernyms.dtm)

#distinctive feature test
test.df<-corpus1.matrix[row.names(corpus1.matrix) %in% filenames1,]
control.df<-corpus1.matrix[row.names(corpus1.matrix) %in% filenames2,]
all1<-sum(test.df) #sum dtm1
all2<-sum(control.df) #sum dtm2
fisher.df<-NULL
for (i in 1:ncol(test.df)){
  word<-colnames(test.df)[i]
  word1<-sum(test.df[,i])
  word2<-sum(control.df[,i])
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
fisher.final<-fisher.df
#keep only features that appear in approximately 10% of documents
fisher.final<-fisher.final[fisher.final$word1 > 92,] 
#keep only features that pass a significance test, adjusted for number of tests
fisher.final<-fisher.final[fisher.final$fish.p < .05/(nrow(fisher.final)),] 
#change for positive and negative distinctive features
fisher.final<-fisher.final[fisher.final$fish.odds < 1,] 
fisher.final<-fisher.final[order(-fisher.final$word1),]
#write.csv(fisher.final, file="Distinctive_POS_HighPeriod_Low.csv")

#1.7
#### LEXICAL SIMPLICITY TEST ####
#this test counts the number of characters per word to assess whether poems
#with high periods use a significantly greater number of shorter words
#word length is a frequently used measure to assess vocabulary difficulty (along with sentence length)
#since we know sentence length decreases in poems w more periods we only want to capture word length.

#divide tables into two groups - High Period Poems and the rest
a<-read.csv("Punctuation_Poetry_20C_All.csv")
#remove poems with more than 50% periods as outliers (removes 8 poems)
a.sub<-a[a$period < 0.5,]
#keep only those poems that are 3sd above the mean of all poems
cut<-mean(a.sub$period)+(3*(sd(a.sub$period)))
a.sub<-a.sub[a.sub$period > cut,]
#sort by word count
a.sub<-a.sub[order(-a.sub$total.words),]
#require poems have a maximum of 500 words
#and minimum of 10 words
a.sub<-a.sub[a.sub$total.words < 501 & a.sub$total.words > 9,]
#create HP group
filenames1<-as.character(a.sub$filenames)
#create not HP group
a.sub2<-a[a$period < cut,]
a.sub2<-a.sub2[a.sub2$total.words < 501 & a.sub2$total.words > 9,]
filenames2<-as.character(a.sub2$filenames)[!as.character(a.sub2$filenames) %in% filenames1] 
#take 10K sample
filenames3<-filenames2[sample(length(filenames2), 10000)]

#run twice
filenames<-filenames3 #define group as either HP or not-HP
#filenames<-filenames1 #define group as either HP or not-HP
final.df<-NULL
for (i in 1:length(filenames)){
  file.id<-as.character(filenames[i])
  work<-scan(filenames[i], what="character", quote="")
  work.words<-tolower(work) # all lower case
  work.words<-gsub("[^[:alnum:][:space:]]", "", work.words) #this removes punctuation but does not split on apostrophes (it's becomes its not it s)
  work.words<-gsub("\\d", "", work.words) #remove numbers
  work.words<-str_trim(work.words) #remove whitespace
  work.words<-work.words[work.words != ""] #remove blanks
  char.count<-unname(sapply(work.words, function (x) nchar(x)))
  mean.char<-mean(char.count)
  sd.char<-sd(char.count)
  temp.df<-data.frame(file.id, mean.char, sd.char)
  final.df<-rbind(final.df, temp.df)
}
#hp<-final.df
#all.p<-final.df
#hp$collection<-c("hp")
#all.p$collection<-c("all")
#final.df<-rbind(hp, all.p)
#write.csv(final.df, file="Difficulty_WordLength.csv")

#compare populations
a<-read.csv("Difficulty_WordLength.csv")
a1<-a[a$collection == "hp",]
a2<-a[a$collection == "all",]
wilcox.test(a1$mean.char, a2$mean.char)
wilcox.test(a1$sd.char, a2$sd.char)
median(a1$mean.char)
median(a2$mean.char)

### 1.8 ###
#### SEMANTIC SIMILARITY AROUND PERIODS ####
#This test examines the semantic similarity of sentence units around periods.
#It creates a vector representation of sentence i and compares its similarity to sentence i+1
#It only takes the final clause of the first sentence and the first clause of the second sentence
#where there are clauses, otherwise it takes the whole sentence.

#Step 1: Build a semantic model
#this uses the word2vec tool to build a vector space model of all 20C poetry in English
library("devtools")
library(magrittr)
#library(tsne)
library(wordVectors)
#Prep text
#prep_word2vec("20CPoetryAll","20CPoetry.txt",lowercase=T) #first is directory, second is output file.
## train model
#model = train_word2vec("20CPoetry.txt",output="20CPoetry.bin",threads = 3,vectors = 100,window=12, force=T)
#or read existing model
model<-read.vectors("20CPOetryAll.bin")
#some operations
model %>% nearest_to(model[["stone"]], 20) %>% round(3)

#Step 2
#this divides each poem into sentences and sequentially compares the semantic
#similarity of each sentence to the following one
#it stores this in a table for inspection
require("NLP")
library("openNLP")
library("openNLPdata")
#establish functions
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en")
#these two functions prepare sentences for comparison
#the first turns a given sentence into a vector of words and only keeps the *final* clause where there is one
#the second turns a given sentence into a vector of words and only keeps the *first* clause where there is one
cleanTxt1<-function (x){
  sent1.whole<-tolower(x) # all lower case
  sent1.token<-unlist(strsplit(sent1.whole, " "))
  sent1.punct<-str_extract(sent1.token, "[[:punct:]]")
  sent1.punct[is.na(sent1.punct)]<-0
  #check to see if there is punctuation
  #check if sentence ends with a period
  if (sent1.punct[length(sent1.punct)] == "."){
    #find final section of sentence after last punctuation mark
    punct1<-which(sent1.punct != 0)
    if (length(punct1) > 1){
      start1<-punct1[(length(punct1)-1)]
      sent1.sub<-sent1.token[(start1+1):length(sent1.token)]
    } else {
      sent1.sub<-sent1.token
    }
    sent1.sub<-str_trim(sent1.sub) #remove whitespace
    sent1.sub<-gsub("[[:punct:]]", "", sent1.sub) #strip punctuation
    sent1.sub<-gsub("\\d", "", sent1.sub) #remove numbers
    sent1.sub<-sent1.sub[!sent1.sub %in% stopwords("English")] #remove stopwords
    sent1.sub<-sent1.sub[sent1.sub != ""]
  } else {
    sent1.sub<-NULL
  }
  return(sent1.sub)
}
cleanTxt2<-function (x){
  sent1.whole<-tolower(x) # all lower case
  sent1.token<-unlist(strsplit(sent1.whole, " "))
  sent1.punct<-str_extract(sent1.token, "[[:punct:]]")
  sent1.punct[is.na(sent1.punct)]<-0
  #check to see if there is punctuation
  #check if sentence ends with a period
  if (sent1.punct[length(sent1.punct)] == "."){
    #find first section of sentence before first punctuation mark
    punct1<-which(sent1.punct != 0)
    if (length(punct1) > 1){
      sent1.sub<-sent1.token[1:punct1[1]]
    } else {
      sent1.sub<-sent1.token
    }
    sent1.sub<-str_trim(sent1.sub) #remove whitespace
    sent1.sub<-gsub("[[:punct:]]", "", sent1.sub) #strip punctuation
    sent1.sub<-gsub("\\d", "", sent1.sub) #remove numbers
    sent1.sub<-sent1.sub[!sent1.sub %in% stopwords("English")] #remove stopwords
    sent1.sub<-sent1.sub[sent1.sub != ""]
  } else {
    sent1.sub<-NULL
  }
  return(sent1.sub)
}
# load sets
a<-read.csv("Punctuation_Poetry_20C_All.csv")
a.sub<-a[a$period < 0.5,]
cut<-mean(a.sub$period)+(3*(sd(a.sub$period)))
a.sub<-a.sub[a.sub$period > cut,]
a.sub<-a.sub[order(-a.sub$total.words),]
a.sub<-a.sub[a.sub$total.words < 501,]
filenames1<-as.character(a.sub$filenames)
a.sub2<-a[a$period < cut,]
filenames2<-as.character(a.sub2$filenames)[!as.character(a.sub2$filenames) %in% filenames1] 
#create random subset of whole
filenames3<-filenames2[sample(length(filenames2), 5000)]
#run
#this takes as input directories of poems
#it subsets by high period and non-high period groups
#it outputs a table of similarity scores between sentences for every poem
#this is then averaged for a given poem
#filenames<-filenames1 #define group
filenames<-filenames3#define group
final.df<-NULL
for (i in 1:length(filenames)) {
  print(i)
  #for (i in 1:5) { #test
  filename<-filenames[i]
  work<-scan(filenames[i], what="character", quote="")
  #run only on poems of less than 2000 words
  if (length(work) < 2000){
    work.clean<- gsub("\\d", "", work)
    text.whole<-paste(work.clean, collapse=" ") # collapse into single chunk
    text.char<-as.String(text.whole)
    a1 <- annotate(text.char, sent_token_annotator)
    sentences<-text.char[a1]
    if (length(sentences) > 1){
      for (j in 1:(length(sentences)-1)){
        sent.no<-j
        sent1<-cleanTxt1(sentences[j])
        sent2<-cleanTxt2(sentences[j+1])
        if (length(sent1) != 0 & length(sent2) != 0){
          sentA<-paste(sent1, collapse=",")
          sentB<-paste(sent2, collapse=",")
          sim.score<-cosineSimilarity(model[[sent1]], model[[sent2]])
          temp.df<-data.frame(filename, sent.no, sim.score, sentA, sentB)
          final.df<-rbind(final.df, temp.df)
        }
      }
    }
  }
}
final.df<-final.df[complete.cases(final.df$sim.score), ]
#compute average semantic sim score per poem
avg.df<-NULL
for (i in 1:nlevels(final.df$filename)){
  sub<-final.df[which(final.df$filename == levels(final.df$filename)[i]),]
  avg.score<-mean(sub$sim.score)
  sd.score<-sd(sub$sim.score)
  file.id<-as.character(sub$filename[1])
  temp.df<-data.frame(file.id, avg.score, sd.score)
  avg.df<-rbind(avg.df, temp.df)
}
write.csv(avg.df, "Sentence_Test_Results_Sample.csv")
write.csv(final.df, "Sentence_Test_Output_Sample.csv")

#test results
a<-read.csv("Sentence_Test_Results_HighPeriod.csv")
b<-read.csv("Sentence_Test_Results_Sample.csv")
a<-a[complete.cases(a),]
b<-b[complete.cases(b),]
#variance is significantly different between populations
var.test(a$avg.score, b$avg.score)
#data not normally distributed so run Wilcoxon Test
#run on similar sized samples
wilcox.test(a$avg.score, b$avg.score)
for (i in 1:5){
  sub<-sample(b$sd.score, nrow(a))
  print(wilcox.test(a$sd.score, sub))
  print(cohen.d(a$sd.score, sub))
}
#on all
wilcox.test(a$sd.score, b$sd.score)
#observe medians
median(a$sd.score)
median(b$sd.score)
#calculate effect size
library(effsize)
cohen.d(a$sd.score, b$sd.score)
#plot to observe strength of differences
plot(sort(a$avg.score))
lines(sort(b$avg.score))
plot(density(b$avg.score))
lines(density(a$avg.score), lty="dashed")
plot(density(b$sd.score), xlab = "Similarity", main = "Standard Deviation in Semantic Similarity\nBetween Sentences in 20C Poetry")
lines(density(a$sd.score), lty="dashed")
