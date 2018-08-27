### Chap 6 Corpus ###
#These scripts can be used to understand the shape of a poet's corpus and career
#Section 1 provides transformations necessary for the models used
#Section 2 concerns measures to understand "vulnerability".
#Section 3 concerns understanding "late style"

library("tm")
library("SnowballC")
library("proxy")
library("corpcor")
library("reshape2")
library("lsa")
library("zoo")
library("koRpus")
library("ggplot2")
library("scales")
library("ggrepel")
library("igraph")
library("splitstackshape")
library("dendextend")

#### Section One: Transformations ####

#in order build the models described in this chapter, you will need to transform 
#poems into 4 different representations:
#1. lexical
#2. semantic
#3. part of speech
#4. phonetic

#in the accompanying data all transformations are available for download
#you can also use code below and accompanying python scripts to transform your own data

#1 and #2 are performed during each model.
#3 is contained below
#4 is contained in the python script library under "Phoneme Translator"

#3
### POS Transformation ###
#this script transforms a directory of text files into their part of speech representation
#it only works on German and English, not French
### Step 0: Prepare POS Directories ###
#unload all previous packages as they will conflict with NLP packages.
require("NLP")
library("openNLP")
library("openNLPdata")
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "de")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "de")
pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "de")
setwd("~/Sites/Topologies - Data/Topologies - Data (Poetry)/PoetryCorpusesAuthor/")
filenames<-list.files("PoetryAuthors_German2", full.names=FALSE)
for (i in 1:length(filenames)) {
  setwd("~/Sites/Topologies - Data/Topologies - Data (Poetry)/PoetryCorpusesAuthor/PoetryAuthors_German2")
  filenames2<-list.files(filenames[i], pattern="*.txt", full.names=FALSE)
  dir<-paste("~/Sites/Topologies - Data/Topologies - Data (Poetry)/PoetryCorpusesAuthor/PoetryAuthors_German2/", filenames[i], sep="")
  setwd(dir)
  for (j in 1:length(filenames2)){
    setwd(dir)
    work<-scan(filenames2[j], what="character", quote="")
    work.clean<- gsub("\\d", "", work)
    text.whole<-paste(work.clean, collapse=" ") # collapse into single chunk
    text.char<-as.String(text.whole)
    a2 <- annotate(text.char, list(sent_token_annotator, word_token_annotator))
    a3 <- annotate(text.char, pos_tag_annotator, a2)
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    tags.final<-paste(tags, collapse = " ")
    dir2<-paste("~/Sites/Topologies - Data/Topologies - Data (Poetry)/PoetryCorpusesAuthor/PoetryAuthors_German2_POS", "/", filenames[i], "_POS", sep="")
    setwd(dir2)
    write(tags.final, file = filenames2[j])
  }
}

##### Section Two: Vulnerabilities #####

#6.1
#This script produces a hierarchical tree based on the similarities between the different
#editions of Leaves of Grass. It uses four primary features: lexical, semantic, part of speech and phonemes.
#it takes as input four separate representations of a poet's work:
#1. lexical representation of a poet's work using tfidf
#2. syntactic representation of a poet's work (POS) using tfidf
#3. phonetic representation of a poet's work (PHON) using tfdidf
#4. semantic representation of a poet's work using Latent Semantic Analysis (LSA)
#it then combines similarity matrices based on these features into a single representation
#and then runs hierarchical clustering on that matrix

#master dir
dir0<-paste("[INPUT DIRECTORY]")
setwd(dir0)
#ingest poems
corpus1 <- VCorpus(DirSource("Whitman_LeavesOfGrass_Editions_Full"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
scaling1<-rowSums(corpus1.matrix)
wordcount.df<-data.frame(scaling1)
#remove stopwords
corpus1 <- tm_map(corpus1, removeWords, stopwords("English"))
#remake dtm
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#tfidf
corpus.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)
corpus.tfidf.mat<-as.matrix(corpus.tfidf, stringsAsFactors=F)

#LSA
data(stopwords_en)
nums<-c(1:5000)
myMatrix<-textmatrix("Whitman_LeavesOfGrass_Editions_Full", stemming=TRUE, language="english", minWordLength=2, maxWordLength=FALSE, minDocFreq=1, maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE, stopwords=stopwords_de, vocabulary=NULL, phrases=NULL, removeXML=FALSE, removeNumbers=FALSE)
myMatrix<-myMatrix[!row.names(myMatrix) %in% nums,]
myMatrix<-lw_logtf(myMatrix) * gw_idf(myMatrix)
myLSAspace<-lsa(myMatrix, dims=dimcalc_share())
cosine.dist2<-simil(t(as.textmatrix(myLSAspace)), method="cosine")
cosine.matrix2<-as.matrix(cosine.dist2, stringsAsFactors=F)*100

#POS
corpus2 <- VCorpus(DirSource("Whitman_LeavesOfGrass_Editions_Full_POS"), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
#take 1-2 POSgrams
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
corpus2.tfidf<-weightTfIdf(corpus2.dtm, normalize = TRUE)
corpus2.tfidf.mat<-as.matrix(corpus2.tfidf, stringsAsFactors=F)

#PHONEMES
corpus3 <- VCorpus(DirSource("Whitman_LeavesOfGrass_Editions_Full_PHON"))
corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf), removePunctuation = FALSE, stopwords = FALSE, tolower=FALSE))
corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F)
#tfidf
corpus3.tfidf<-weightTfIdf(corpus3.dtm, normalize = TRUE)
corpus3.tfidf.mat<-as.matrix(corpus3.tfidf, stringsAsFactors=F)

#add POS & PHONEMES to LEXICAL DTM
#this creates a single large feature space of POS, Phonemes, and words
all.tfidf<-cbind(corpus.tfidf.mat, corpus2.tfidf.mat, corpus3.tfidf.mat)
#then create similarity matrix for all poems based on these features
cosine.dist1<-simil(all.tfidf, method = "cosine")
cosine.matrix1<-as.matrix(cosine.dist1, stringsAsFactors=F)*100

#combine LSA with LEX/POS/PHON similarity matrices
#this effectively weights the semantic representation the same as all other 3 combined
#see the appendix in the book for a full discussion and chap 6 for a validation
#### this is only one way to build the model which I tested on numerous cases ####
#### there are many other ways one could do this -- more testing needed!! ####
cosine.matrix1[is.na(cosine.matrix1)]<-0 #Lex/POS Matrix #change 0 to 1 for heat map
cosine.matrix2[is.na(cosine.matrix2)]<-0 #LSA Matrix #change 0 to 1 for heat map
cosine.matrix<-(cosine.matrix1+cosine.matrix2)/2

#prepare for clustering exercise
d<-cosine.matrix
d[d==0]<-NA
#turn into distance matrix
d<-100-d
d[is.na(d)]<-0
d<-d/100
row.names(d)<-c("1855", "1856", "1860", "1867", "1871", "1881", "1891")
#run hierarchical clustering
d.dist<-as.dist(d)
fit <- hclust(d.dist, method="ward.D")
plot(fit, horiz=T)
#predict optimal number of clusters
dend_k<-find_k(fit, krange = 2:(nrow(d.dist)-1))

### Fig. 6.1 ###
dend<-as.dendrogram(fit)
plot(dend, xlab="", ylab="", horiz=T)
title(main="The Editions of Leaves of Grass", sub="Fig. 6.1 Stylistic affinities of the different editions of Walt Whitman's Leaves of Grass\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")

#6.2
#Network of the relationship of pages to each other in the different editions of Leaves of Grass
#the figure shows the 1891 edition based on the four features above
#this script creates a network of relationships between the pages of a given volume of poetry
#it is assumed that pages have been normalized to be roughly the same length (i.e. chunked)
#each page is considered a node
#an edge is drawn between a page and its "most similar pages", where similarity is determined using the
#four features described above
#it does so in an evolutionary fashion meaning that a page can only be connected to a page that has come
#before it. This will make more sense when considering poets' careers (where a poem can only be similar
#to poems that the poet has already written).

#the script takes as input three directories:
#1. lexical representation of a poet's works
#2. syntactic representation of a poet's works (POS)
#3. phonetic representation of a poet's works (PHON)
#it outputs a directory of edge lists for every work, where nodes = pages of that work and edges = similarity
#it uses a cut-off to establish whether two pages are "similar" (see script for details)
#here "works" are considered volumes of poetry ("Leaves of Grass"). Later in the chapter,
#a poet's career is considered the "work" and the individual poems are equivalent to "pages" in this model

#create directories
#master dir
dir0<-paste("[INSERT DIRECTORY]")
#lexical directories
dir1<-paste(dir0, "/Whitman_LeavesOfGrass_Chunked_300/", sep="")
#POS directories
dir2<-paste(dir0,"/Whitman_LeavesOfGrass_Chunked_300_POS/", sep="")
#PHONEME directories
dir3<-paste(dir0,"/Whitman_LeavesOfGrass_Chunked_300_PHON/",sep="")
#start
setwd(dir0)
filenames0<-list.files("Whitman_LeavesOfGrass_Chunked_300", full.names=FALSE)
filenames1<-list.files("Whitman_LeavesOfGrass_Chunked_300_POS", full.names=FALSE)
filenames2<-list.files("Whitman_LeavesOfGrass_Chunked_300_PHON", full.names=FALSE)
final.df<-NULL
for (m in 7:length(filenames0)) {
  print(m)
  #LEXICAL
  setwd(dir1)
  corpus1 <- VCorpus(DirSource(filenames0[m]), readerControl=list(language="English"))
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  corpus1 <- tm_map(corpus1, stemDocument, language = "english")
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  scaling1<-rowSums(corpus1.matrix)
  wordcount.df<-data.frame(scaling1)
  #remove stopwords
  corpus1 <- tm_map(corpus1, removeWords, stopwords("English"))
  #remake dtm
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  #tfidf
  corpus.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)
  corpus.tfidf.mat<-as.matrix(corpus.tfidf, stringsAsFactors=F)

  #LSA
  data(stopwords_en)
  dir4<-paste(dir1, filenames0[m], sep="")
  nums<-c(1:5000)
  myMatrix<-textmatrix(dir4, stemming=TRUE, language="english", minWordLength=2, maxWordLength=FALSE, minDocFreq=1, maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE, stopwords=stopwords_de, vocabulary=NULL, phrases=NULL, removeXML=FALSE, removeNumbers=FALSE)
  myMatrix<-myMatrix[!row.names(myMatrix) %in% nums,]
  myMatrix<-lw_logtf(myMatrix) * gw_idf(myMatrix)
  myLSAspace<-lsa(myMatrix, dims=dimcalc_share())
  cosine.dist2<-simil(t(as.textmatrix(myLSAspace)), method="cosine")
  cosine.matrix2<-as.matrix(cosine.dist2, stringsAsFactors=F)*100
  
  #POS
  setwd(dir2)
  corpus2 <- VCorpus(DirSource(filenames1[m]), readerControl=list(language="English"))
  corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
  corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
  #take 1-2 POSgrams
  BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
  corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
  corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
  corpus2.tfidf<-weightTfIdf(corpus2.dtm, normalize = TRUE)
  corpus2.tfidf.mat<-as.matrix(corpus2.tfidf, stringsAsFactors=F)
  
  #PHONEMES
  setwd(dir3)
  corpus3 <- VCorpus(DirSource(filenames2[m]))
  corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
  corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf), removePunctuation = FALSE, stopwords = FALSE, tolower=FALSE))
  corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F)
  #tfidf
  corpus3.tfidf<-weightTfIdf(corpus3.dtm, normalize = TRUE)
  corpus3.tfidf.mat<-as.matrix(corpus3.tfidf, stringsAsFactors=F)
  
  #add POS & PHONEMES to LEXICAL DTM
  all.tfidf<-cbind(corpus.tfidf.mat, corpus2.tfidf.mat, corpus3.tfidf.mat)
  #similarity matrix
  cosine.dist1<-simil(all.tfidf, method = "cosine")
  cosine.matrix1<-as.matrix(cosine.dist1, stringsAsFactors=F)*100

  #combine LSA with LEX/POS
  cosine.matrix1[is.na(cosine.matrix1)]<-0 #Lex/POS Matrix #change 0 to 1 for heat map
  cosine.matrix2[is.na(cosine.matrix2)]<-0 #LSA Matrix #change 0 to 1 for heat map
  cosine.matrix<-(cosine.matrix1+cosine.matrix2)/2
  #setwd("[SET DIRECTORY]")  
  cosine.file<-paste(filenames0[m], "_SIM.csv", sep = "")
  #write.csv(cosine.matrix, file=cosine.file)
  
  #generate statistical significance threshold
  test<-sm2vec(cosine.matrix)
  global.cut<-mean(test)+(1.96*sd(test))
  sim.v<-vector()
  for (z in 1:nrow(cosine.matrix)){
    sub<-sort(cosine.matrix[z,], decreasing=T)[1:100]
    sim.v<-append(sim.v, sub)
  }
  sim.mean<-mean(sim.v)
  sim.sd<-sd(sim.v)

  ### Step 2: Make Edge List ###
  edge.list<-data.frame()
  for (i in 3:nrow(cosine.matrix)) {
    #get word count of page
    count.source<-wordcount.df[row.names(wordcount.df) == row.names(cosine.matrix)[i],]
    #subset matrix by all poems up to target poem
    corp.select<-cosine.matrix[1:i,1:i]
    #OR subset matrix by most recent 100 pages (to control for differing lengths of editions)
    # if (i > 100){
    #   corp.select<-cosine.matrix[(i-99):i,(i-99):i]
    # }else{
    #   corp.select<-cosine.matrix[1:i,1:i]
    # }
    #sort poems in decreasing order
    corp.test<-corp.select
    corp.all<-sort(corp.test[(max(nrow(corp.test))),], decreasing=T)
    corp.all.df<-data.frame(corp.all)
    corp.all.df$names<-row.names(corp.all.df)
    #take the top closest poems based on significance
    #COMMENT NEXT LINES OUT until ALTERNATIVE if using alternative
    #       #calc threshold based on statistical significance
    #       #this takes 2sd above the poem's overall mean similarity to other poems
    #       cut<-mean(corp.all)+(1.96*sd(corp.all))
    #       #if that amount is below the global mean + sd of similarity, then it uses this global mean
    #       if (cut < global.cut){
    #         cut<-global.cut
    #       }
    #       corp.top<-subset(corp.all.df, corp.all.df[,1] > cut)
    #       if (nrow(corp.top) == 0) {
    #        corp.top<-data.frame(corp.all.df[1,1], row.names(corp.all.df)[1])
    #        row.names(corp.top)<-as.character(corp.top[,2])
    #       }
    #### ALTERNATIVE: take differential and cut when distance between 2 obs is greater than the next one, i.e. looking for a significant drop
    #COMMENT lines above if you use this alternative
    if (length(corp.all) > 2){
      if (diff(corp.all)[[1]] < diff(corp.all)[[2]]){
        corp.top<-corp.all.df[1,]
      } else {
        for (n in 1:(length(corp.all)-2)){
          if (diff(corp.all)[[n+1]] < diff(corp.all)[[n]]){
            corp.top<-corp.all.df[1:(n+1),]
            break
          } else {
            corp.top<-corp.all.df
          }
        }
      }
    } else {
      corp.top<-corp.all.df
    }
    ###COMMENT TO HERE###
    corp.df<-corp.top
    #create edge list
    for (j in 1:nrow(corp.df)) {
      source<-row.names(cosine.matrix)[i]
      target<-row.names(corp.df)[j]
      weight<-corp.top[j,1]
      count.target<-wordcount.df[row.names(wordcount.df) == row.names(corp.df)[j],]
      count.diff<-(count.source-count.target)/min(count.source, count.target)
      abs.diff<-abs(count.diff)
      page.diff<-which(row.names(cosine.matrix) == as.character(source))-which(row.names(cosine.matrix) == as.character(target))
      newRow<-data.frame(source, target, weight, count.source, count.target, count.diff, abs.diff, page.diff, stringsAsFactors = F)
      edge.list<-rbind(edge.list, newRow)
    }
  }
  #setwd("[SET DIRECTORY]")  
  #file.final<-paste(filenames0[m], "_EdgeList.csv", sep="")
  #write.csv(edge.list, file=file.final)
  
  #### Step 3: Network Analysis ####
  author.edge<-edge.list[,1:3]
  author.nodes<-append(edge.list$source, edge.list$target)
  author.nodes<-data.frame(author.nodes[!duplicated(author.nodes)])
  colnames(author.nodes)<-c("name")
  author.split<-cSplit(author.nodes, "name", sep="_", type.convert=FALSE)
  author.split<-cbind(author.nodes, author.split)
  g<-graph.data.frame(author.edge, directed=TRUE, vertices=NULL)
  #to measure diameter distance use inverse of similarity score
  author.edge2<-data.frame(source=author.edge[,1], target=author.edge[,2], weight=((1/author.edge[,3])*100))
  g.dist<-graph.data.frame(author.edge2, directed=TRUE, vertices=NULL)
  #create undirected graph as well
  g3<-graph.data.frame(author.edge, directed=FALSE, vertices=NULL)
  #summary calculations
  book.length<-nrow(cosine.matrix)
  #number nodes
  no.nodes<-length(V(g))
  #number edges
  no.edges<-length(E(g))
  mean.degree<-mean(degree(g))
  sd.degree<-sd(degree(g))
  #percent of nodes w 1 connection
  per.1.degree<-degree_distribution(g)[2] 
  #percent 1-3 connection
  per.1.to.3.degree<-sum(degree_distribution(g)[2:4]) 
  #percent 1-5 connection
  per.1.to.5.degree<-sum(degree_distribution(g)[2:6]) 
  #+5 connections
  per.plus.five.degree<-sum(degree_distribution(g)[7:length(degree_distribution(g))]) 
  trans.score<-transitivity(g, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))
  #calculate avg. page distance between nodes for every edge
  avg.page.diff<-mean(edge.list$page.diff)
  sd.page.diff<-sd(edge.list$page.diff)
  #normalized page difference
  norm.avg.page.diff<-avg.page.diff/book.length 
  per.page.diff.less6<-length(which(edge.list$page.diff < 6))/length(edge.list$page.diff) #percent of poems spanning more than a decade
  per.page.diff.greater20<-length(which(edge.list$page.diff > 20))/length(edge.list$page.diff) #percent of poems spanning more than a decade
  avg.poem.similarity<-mean(E(g)$weight)
  sd.poem.similarity<-sd(E(g)$weight)
  #clean community detection and just scrape modularity + no. communities for all comm methods
  comm.louvain<-cluster_louvain(g3, weights = E(g)$weight)
  groups.louvain<-max(comm.louvain$membership)
  mod.louvain<-modularity(comm.louvain)
  ####walktrap
  #based on the random walk theory that you will get trapped more in a community than not.
  comm.walk<-walktrap.community(g, weights = E(g)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
  groups.walk<-max(comm.walk$membership)
  mod.walk<-modularity(comm.walk)
  ###fastgreedy
  comm.fast<-cluster_fast_greedy(g3, weights=E(g)$weight)
  groups.fast<-max(comm.fast$membership)
  mod.fast<-modularity(comm.fast)
  #robustness = random deletion of nodes
  robustness_vector<-vector()
  for (j in 1:1000) {
    #random.nodes<-sample(V(g), length(V(g)), replace = FALSE, prob = NULL)
    g.robust<-as.undirected(g)
    for (i in 1:length(V(g))) {
      random.node<-sample(1:length(V(g.robust)), 1, replace = FALSE)
      g.robust<-delete.vertices(g.robust, v=random.node)
      if (is.connected(g.robust)==FALSE)
        break
    }
    robustness_vector<-append(robustness_vector, i)
  }
  robust.mean<-mean(robustness_vector)
  robust.sd<-sd(robustness_vector)
  #vulnerability = targeted deletion of nodes via descending order of degree
  #controlling for top 100 nodes
  
  #vulnerability1 = % nodes deleted when the remaining graph is less than half the size of the original graph
  deg<-names(sort(degree(g),TRUE))
  g.vulnerable<-as.undirected(g)
  g.vulnerable<-delete.vertices(g.vulnerable, v=as.character(deg[101:length(V(g.vulnerable))]))
  for (i in 1:length(V(g.vulnerable))) {
    g.vulnerable<-delete.vertices(g.vulnerable, v=as.character(deg[[i]]))
    #components(g.vulnerable)$csize
    #components(g.vulnerable)$no
    if (max(components(g.vulnerable)$csize) < (length(V(g.vulnerable))/2))
      break
  }
  vulnerability1<-i/length(V(g.vulnerable))
  
  #vulnerability2 = when the largest remaining component is no longer 50% bigger than the next largest
  deg<-names(sort(degree(g),TRUE))
  g.vulnerable<-as.undirected(g)
  g.vulnerable<-delete.vertices(g.vulnerable, v=as.character(deg[101:length(V(g.vulnerable))]))
  for (i in 1:length(V(g.vulnerable))) {
    g.vulnerable<-delete.vertices(g.vulnerable, v=as.character(deg[[i]]))
    #components(g.vulnerable)$csize
    #components(g.vulnerable)$no
    if (components(g.vulnerable)$no > 1){
      if ((.5*max(components(g.vulnerable)$csize)) < sort(components(g.vulnerable)$csize, decreasing=T)[2])
        break
    }
  }
  vulnerability2<-i/length(V(g.vulnerable))
  diam<-diameter(g.dist)
  
  #combine all network scores
  author.name<-as.character(filenames0[m])
  temp.df<-data.frame(author.name,book.length, no.nodes, no.edges, mean.degree, sd.degree,+
                        per.1.degree, per.1.to.3.degree, per.1.to.5.degree, per.plus.five.degree,+
                        trans.score, avg.page.diff, sd.page.diff, norm.avg.page.diff, per.page.diff.less6, per.page.diff.greater20,+
                        avg.poem.similarity, sd.poem.similarity, groups.louvain,+
                        mod.louvain,groups.walk,mod.walk,groups.fast,mod.fast, robust.mean, robust.sd,+
                        diam, vulnerability1, vulnerability2, sim.mean, sim.sd)
  final.df<-rbind(final.df, temp.df)
}
#write.csv(final.df, file="Whitman_Results.csv")

#### Fig. 6.2 ####
#The network plot used was generated using the edge lists in the directory "Whitman_LeavesOfGrass_EdgeLists_Diff"
#It was generated in Gephi using the gephi file "Whitman.gephi"

#### Fig. 6.3 ####
#Figure three takes as input the output from Script 6.2 data called "Whitman_Results.csv"
a<-read.csv("Whitman_Results.csv")

tit.size<-14
p1<-ggplot(a, aes(x=author.name, y=norm.avg.page.diff*100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(shape=15, size=2) +
  geom_line() +
  labs(title = "(a) Page Distance", x="Date", y="% of Work")

p2<-ggplot(a, aes(x=author.name, y=per.page.diff.less6*100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(shape=15, size=2) +
  geom_line() +
  labs(title = "(b) Pages less than 5 away", x="Date", y="% of Pages")

p3<-ggplot(a, aes(x=author.name, y=per.page.diff.greater20*100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(shape=15, size=2) +
  geom_line() +
  labs(title = "(c) Pages more than 20 away", x="Date", y="% of Pages")

p4<-ggplot(a, aes(x=author.name, y=sd.poem.similarity/100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(shape=15, size=2) +
  geom_line() +
  labs(title = "(d) SD of Page Similarity", x="Date", y="Similarity")

p5<-ggplot(a, aes(x=author.name, y=(1-a$vulnerability2)*100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(shape=15, size=2) +
  geom_line() +
  labs(title = "(e) Vulnerability", x="Date", y="% of Pages")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol=2)

#6.3
### Model Comparison ###
#this script allows you to observe the effects of using different features and feature weights
#it takes one poet at a time as input
#here I use the example of Muriel Rukeyser

#input directory names
#requires 3 representations of every poet's work (lexical, syntactic, phonetic)
filenames0<-c("MurielRukeyser")
filenames1<-c("MurielRukeyser_POS")
filenames2<-c("MurielRukeyser_PHON")

#what follows are the different features. these can be turned off/on depending on how you want the model to work
#currently all features are included.

#LEXICAL
corpus1 <- VCorpus(DirSource(filenames0), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
scaling1<-rowSums(corpus1.matrix)
wordcount.df<-data.frame(scaling1)
#remove stopwords
corpus1 <- tm_map(corpus1, removeWords, stopwords("English"))
#remake dtm
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#tfidf
corpus.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)
corpus.tfidf.mat<-as.matrix(corpus.tfidf, stringsAsFactors=F)
  
#LSA
data(stopwords_en)
nums<-c(1:5000)
myMatrix<-textmatrix(filenames0, stemming=TRUE, language="english", minWordLength=2, maxWordLength=FALSE, minDocFreq=1, maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE, stopwords=stopwords_de, vocabulary=NULL, phrases=NULL, removeXML=FALSE, removeNumbers=FALSE)
myMatrix<-myMatrix[!row.names(myMatrix) %in% nums,]
myMatrix<-lw_logtf(myMatrix) * gw_idf(myMatrix)
myLSAspace<-lsa(myMatrix, dims=dimcalc_share())
#make first similarity matrix based on semantic representation
cosine.dist2<-simil(t(as.textmatrix(myLSAspace)), method="cosine")
cosine.matrix2<-as.matrix(cosine.dist2, stringsAsFactors=F)*100
  
#POS
corpus2 <- VCorpus(DirSource(filenames1), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
#take 1-2 POSgrams
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
corpus2.tfidf<-weightTfIdf(corpus2.dtm, normalize = TRUE)
corpus2.tfidf.mat<-as.matrix(corpus2.tfidf, stringsAsFactors=F)
  
#PHONEMES
corpus3 <- VCorpus(DirSource(filenames2))
corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf), removePunctuation = FALSE, stopwords = FALSE, tolower=FALSE))
corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F)
#tfidf
corpus3.tfidf<-weightTfIdf(corpus3.dtm, normalize = TRUE)
corpus3.tfidf.mat<-as.matrix(corpus3.tfidf, stringsAsFactors=F)
  
#combine POS & PHONEMES & LEXICAL Tables
all.tfidf<-cbind(corpus.tfidf.mat, corpus2.tfidf.mat, corpus3.tfidf.mat)
#make similarity matrix based on combined feature space
cosine.dist1<-simil(all.tfidf, method = "cosine")
cosine.matrix1<-as.matrix(cosine.dist1, stringsAsFactors=F)*100
#adjust for length
cosine.matrix1<-((1/log(scaling1))*cosine.matrix1)*10

#clean
cosine.matrix1[is.na(cosine.matrix1)]<-0
cosine.matrix2[is.na(cosine.matrix2)]<-0

#Option 1: Combine all 4 features
#combine LSA similarity matrix with LEX/POS/PHON and take *average* similarity
cosine.matrix<-(cosine.matrix1+cosine.matrix2)/2

#Option 2: only LSA
#cosine.matrix<-cosine.matrix2

#Option 3: only LEX/POS/PHON
#cosine.matrix<-cosine.matrix1

#Option 4: only LEX
#cosine.dist1<-simil(corpus.tfidf.mat, method = "cosine")
#cosine.matrix.lex<-as.matrix(cosine.dist1, stringsAsFactors=F)*100
#cosine.matrix.lex[is.na(cosine.matrix.lex)]<-0
#cosine.matrix<-cosine.matrix.lex

### Step 2: Make Edge List ###
#this outputs a table of most similar poems for a given poem based on the model used above
#it takes as input the table "cosine.matrix" and finds the most similar poems to a given poem
#it outputs an edge list where source is the original poem and target is the poem that is most similar to it
#poems can only be similar to poems that were written *before* them
#data needs tobe in chronological order in directory for this script to work
edge.list<-data.frame()
for (i in 3:nrow(cosine.matrix)) {
  #first truncate the similarity table by poems that precede the i'th poem
  count.source<-wordcount.df[row.names(wordcount.df) == row.names(cosine.matrix)[i],]
  corp.select<-cosine.matrix[1:i,1:i]
  #criteria 1: remove poems beyond 2x above/below its word length
  #defaults to all poems if the selection criteria can't be met
  #if poem is greater than 1000 words, then it can be similar to any poem > 500 words
  if (count.source < 999){
    wordcount.sub<-wordcount.df[row.names(wordcount.df) %in% row.names(corp.select),]
    over<-which(wordcount.sub > 2*count.source)
    under<-which(wordcount.sub < count.source/2)
    comb<-append(over, under)
    if (length(comb) != 0){
      corp.test<-corp.select[-comb,-comb]
    } else {corp.test<-corp.select
    }
  } else {
    wordcount.sub<-wordcount.df[row.names(wordcount.df) %in% row.names(corp.select),]
    over<-which(wordcount.sub > 500)
    corp.test<-corp.select[over,over]
  }
  #revert to original if criteria aren't met
  if (is.null(nrow(corp.test))){
    corp.test<-corp.select
  }
  if (nrow(corp.test) == 2){
    corp.test<-corp.select
  }
  zero.test<-corp.test[max(nrow(corp.test)),][1:(nrow(corp.test)-1)]
  if(sum(zero.test) == 0){
    corp.test<-corp.select
  }
  #sort the poems in decreasing order of similarity to the source poem
  corp.all<-sort(corp.test[(max(nrow(corp.test))),], decreasing=T)
  corp.all.df<-data.frame(corp.all)
  corp.all.df$names<-row.names(corp.all.df)
    #take the top closest poems based on statistical significance
    #COMMENT NEXT LINES OUT until ALTERNATIVE if using alternative
    #       #calc threshold based on statistical significance
    #       #this takes 2sd above the poem's overall mean similarity to other poems
    #       cut<-mean(corp.all)+(1.96*sd(corp.all))
    #       #if that amount is below the global mean + sd of similarity, then it uses this global mean
    #       if (cut < global.cut){
    #         cut<-global.cut
    #       }
    #       corp.top<-subset(corp.all.df, corp.all.df[,1] > cut)
    #       if (nrow(corp.top) == 0) {
    #        corp.top<-data.frame(corp.all.df[1,1], row.names(corp.all.df)[1])
    #        row.names(corp.top)<-as.character(corp.top[,2])
    #       }
    #### ALTERNATIVE: take differential and cut when distance between 2 obs is greater than the next one, i.e. looking for a significant drop
    #COMMENT lines above if you use this alternative
  #keep only those poems for which the distance between them is not greater than the one above it
  #in other words, this cuts at the point at which the difference between two poems' similarity is greater than the previous poem's difference
  #it thus keeps the most similar poem and any others whose similarity is closer to the one kept than the next one not kept
  #it favors keeping fewer poems with the exception of when the dissimilarity of the next poem is not considerably greater than the one before it
  if (length(corp.all) > 2){
    if (diff(corp.all)[[1]] < diff(corp.all)[[2]]){
      corp.top<-corp.all.df[1,]
    } else {
      for (n in 1:(length(corp.all)-2)){
        if (diff(corp.all)[[n+1]] < diff(corp.all)[[n]]){
          corp.top<-corp.all.df[1:(n+1),]
          break
        } else {
          corp.top<-corp.all.df
        }
      }
    }
  } else {
    corp.top<-corp.all.df
  }
  ###COMMENT TO HERE###
  #this is the top most similar poem(s)
  corp.df<-corp.top
  #create edge list
  #this conjoins the source poem w its most similar poem(s)
  #if there is more than one most similar then it creates a new row
  #it records the word counts of each poem to assess how different they are in length
  for (j in 1:nrow(corp.df)) {
    source<-row.names(cosine.matrix)[i]
    target<-row.names(corp.df)[j]
    weight<-corp.top[j,1]
    #word count of source and target poems to assess their similarity in terms of size
    count.target<-wordcount.df[row.names(wordcount.df) == row.names(corp.df)[j],]
    count.diff<-(count.source-count.target)/min(count.source, count.target)
    abs.diff<-abs(count.diff)
    #how far apart are the poems in the corpus (simple representation of poem#)
    #a better representation would take year difference encoded in filenames
    poem.diff<-which(row.names(cosine.matrix) == as.character(source))-which(row.names(cosine.matrix) == as.character(target))
    newRow<-data.frame(source, target, weight, count.source, count.target, count.diff, abs.diff, poem.diff, stringsAsFactors = F)
    edge.list<-rbind(edge.list, newRow)
  }
}
#"edge.list" is the output to be inspected
#source and target are the poems being compared, i.e. it searches for the most
#similar poem to the source poem
#weight = cosine similarity *100
#count.source = word count of the source poem
#count.target = word count of the target poem [choices are limited based on word count similarity]
#count.diff = ratio of the two word counts with positive being how much longer
#the source is than the target
#abs.diff = same using absolute values
#poem.diff = how far apart they are in the corpus
#remember, you can only be similar backwards so those numbers grow as the poet ages
#more could be done to secure ordering of poems for this measure
#with each edge.list table for each model you can then compare the choices that the model makes

### Assess which words overlap between any two poems in a corpus ###
#this takes corpus.tfidf.mat from above as an input

#find intersect of two poems based on their tfidf words
poem1<-c("1948_RukeyserMuriel_018_SUMMERTHESACRAMENTO_0000001.txt")
poem2<-c("1976_RukeyserMuriel_008_BLUESPRUCE_0000001.txt")
poem2<-c("1957_RukeyserMuriel_062_Aredbridgefasteningthiscitytotheforest_0000001.txt")
df1<-corpus.tfidf.mat[which(row.names(corpus.tfidf.mat) == poem1), ]
df2<-corpus.tfidf.mat[which(row.names(corpus.tfidf.mat) == poem2), ]
df1<-df1[which(df1 != 0)]
df2<-df2[which(df2 != 0)]
intersect(names(df1), names(df2))

############# Section Two: Vulnerability and Periodization ###############

#6.4
###### Global Vulnerability ######
#this script creates a network of relationships between the works of a given poet
#each work is considered a node
#an edge is drawn between a work and its "most similar works", where similarity is determined using the
#four features described above
#it does so in an evolutionary fashion meaning that a work can only be connected to a work that has temporally come
#before it.

#the script takes as input three directories that each contain directories of:
#1. lexical representation of a poet's works
#2. syntactic representation of a poet's works (POS)
#3. phonetic representation of a poet's works (PHON)
#it requires that files are named in the following way:
#DATE_AUTHOR_TITLE.txt
#this allows the script to calculate the length of the poet's career and the yearly distances between similar poems
#it outputs a table of network measures related to the poet's career
#"Global Vulnerability" corresponds to "Vulnerability 2" below
#it can also output edge lists for every poet so these can be further studied or visualized
#they are formatted for use in Gephi

#load stopword lists for the LSA section
data(stopwords_de)
data(stopwords_en)
data(stopwords_fr)

#establish language of your corpus
#make sure to change the LSA stopword manually (this is a bug in the function)
language1<-c("German")
language2<-c("german")
#language1<-c("English")
#language2<-c("english")
#language1<-c("French")
#language2<-c("french")
#remove bad words
#for German
problems<-c("drum", "habt", "hast", "ichs", "ists", "sei", "wär", "weimar", "zwei", "seite", "apparat", "datumsangaben")
#or set as empty
#problems<-vector()

#set directories
#there should be one home directory which contains the 3 different representations of each poet
#each representation directory should contain directories of each poet (so one directory per poet)
homedir<-paste("~/Documents/2. Books/Enumerations/Enumerations - Data and Code/Data/txtlab_POETRY_CW")
#lexdir<-paste("PoetryAuthors_German")
#posdir<-paste("PoetryAuthors_German_POS")
#phondir<-paste("PoetryAuthors_German_PHON")
lexdir<-paste("PoetryAuthors_English")
posdir<-paste("PoetryAuthors_English_POS")
phondir<-paste("PoetryAuthors_English_PHON")
#lexdir<-paste("PoetryAuthors_French")
#posdir<-paste("PoetryAuthors_French_POS")
#phondir<-paste("PoetryAuthors_French_PHON")

#set working directory
setwd(homedir)

#get metadata
#meta<-read.csv("PoetryAuthors_German_Meta.csv")
meta<-read.csv("PoetryAuthors_English_Meta.csv")
#meta<-read.csv("PoetryAuthors_French_Meta.csv")

#get names of poets
#each poet's works should be a separate directory
filenames0<-list.files(lexdir, full.names=FALSE)
filenames1<-list.files(posdir, full.names=FALSE)
filenames2<-list.files(phondir, full.names=FALSE)

#run
final.df<-NULL
for (m in 1:length(filenames0)) {
  print(m)
  #LEXICAL
  dir<-paste(homedir, lexdir, sep = "/")
  setwd(dir)
  corpus1 <- VCorpus(DirSource(filenames0[m]), readerControl=list(language=language1))
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  corpus1 <- tm_map(corpus1, removeWords, problems)
  corpus1 <- tm_map(corpus1, stemDocument, language = language2)
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  scaling1<-rowSums(corpus1.matrix)
  wordcount.df<-data.frame(scaling1)
  #remove stopwords
  corpus1 <- tm_map(corpus1, removeWords, stopwords(language1))
  #remake dtm
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  #tfidf
  corpus.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)
  corpus.tfidf.mat<-as.matrix(corpus.tfidf, stringsAsFactors=F)
  
  #LSA
  dir<-paste(homedir, lexdir, filenames0[m], sep = "/")
  nums<-c(1:5000)
  #change language manually!!!
  myMatrix<-textmatrix(dir, stemming=TRUE, language=language2, minWordLength=2, maxWordLength=FALSE, minDocFreq=1, maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE, stopwords=stopwords_en, vocabulary=NULL, phrases=NULL, removeXML=FALSE, removeNumbers=FALSE)
  myMatrix<-myMatrix[!row.names(myMatrix) %in% nums,]
  myMatrix<-myMatrix[!row.names(myMatrix) %in% problems,]
  myMatrix<-lw_logtf(myMatrix) * gw_idf(myMatrix)
  myLSAspace<-lsa(myMatrix, dims=dimcalc_share())
  cosine.dist2<-simil(t(as.textmatrix(myLSAspace)), method="cosine")
  cosine.matrix2<-as.matrix(cosine.dist2, stringsAsFactors=F)*100
  
  #POS
  dir<-paste(homedir, posdir, sep = "/")
  setwd(dir)
  corpus2 <- VCorpus(DirSource(filenames1[m]), readerControl=list(language=language1))
  corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
  corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
  #take 1-2 POSgrams
  BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
  corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
  corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
  corpus2.tfidf<-weightTfIdf(corpus2.dtm, normalize = TRUE)
  corpus2.tfidf.mat<-as.matrix(corpus2.tfidf, stringsAsFactors=F)
  
  #PHONEMES
  dir<-paste(homedir, phondir, sep = "/")
  setwd(dir)
  corpus3 <- VCorpus(DirSource(filenames2[m]))
  corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
  corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf), removePunctuation = FALSE, stopwords = FALSE, tolower=FALSE))
  corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F)
  #tfidf
  corpus3.tfidf<-weightTfIdf(corpus3.dtm, normalize = TRUE)
  corpus3.tfidf.mat<-as.matrix(corpus3.tfidf, stringsAsFactors=F)
  
  #add POS & PHONEMES to LEXICAL DTM
  all.tfidf<-cbind(corpus.tfidf.mat, corpus2.tfidf.mat, corpus3.tfidf.mat)
  #similarity matrix
  cosine.dist1<-simil(all.tfidf, method = "cosine")
  cosine.matrix1<-as.matrix(cosine.dist1, stringsAsFactors=F)*100
  #adjust for length
  cosine.matrix1<-((1/log(scaling1))*cosine.matrix1)*10
  
  #combine LSA with LEX/POS
  cosine.matrix1[is.na(cosine.matrix1)]<-0
  cosine.matrix2[is.na(cosine.matrix2)]<-0
  cosine.matrix<-(cosine.matrix1+cosine.matrix2)/2

  #generate statistical significance threshold
  test<-sm2vec(cosine.matrix)
  global.cut<-mean(test)+(1.96*sd(test))
  sim.v<-vector()
  for (z in 1:nrow(cosine.matrix)){
    sub<-sort(cosine.matrix[z,], decreasing=T)[1:100]
    sim.v<-append(sim.v, sub)
  }
  sim.mean<-mean(sim.v)
  sim.sd<-sd(sim.v)
  
  ### Step 2: Make Edge List ###
  edge.list<-data.frame()
  for (i in 3:nrow(cosine.matrix)) {
    count.source<-wordcount.df[row.names(wordcount.df) == row.names(cosine.matrix)[i],]
    corp.select<-cosine.matrix[1:i,1:i]
    #remove poems beyond 2x above/below its word length
    #if conditions default to all poems if the selection criteria can't be met
    if (count.source < 999){
      wordcount.sub<-wordcount.df[row.names(wordcount.df) %in% row.names(corp.select),]
      over<-which(wordcount.sub > 2*count.source)
      under<-which(wordcount.sub < count.source/2)
      comb<-append(over, under)
      if (length(comb) != 0){
        corp.test<-corp.select[-comb,-comb]
      } else {corp.test<-corp.select
      }
      #if poem is greater than 1000 words, then it can be similar to any poem > 500 words
    } else {
      wordcount.sub<-wordcount.df[row.names(wordcount.df) %in% row.names(corp.select),]
      over<-which(wordcount.sub > 500)
      corp.test<-corp.select[over,over]
    }
    if (is.null(nrow(corp.test))){
      corp.test<-corp.select
    }
    if (nrow(corp.test) == 2){
      corp.test<-corp.select
    }
    zero.test<-corp.test[max(nrow(corp.test)),][1:(nrow(corp.test)-1)]
    if(sum(zero.test) == 0){
      corp.test<-corp.select
    }
    corp.all<-sort(corp.test[(max(nrow(corp.test))),], decreasing=T)
    corp.all.df<-data.frame(corp.all)
    corp.all.df$names<-row.names(corp.all.df)
    #take the top closest poems based on significance
    #COMMENT NEXT LINES OUT until ALTERNATIVE if using alternative
    #       #calc threshold based on statistical significance
    #       #this takes 2sd above the poem's overall mean similarity to other poems
    #       cut<-mean(corp.all)+(1.96*sd(corp.all))
    #       #if that amount is below the global mean + sd of similarity, then it uses this global mean
    #       if (cut < global.cut){
    #         cut<-global.cut
    #       }
    #       corp.top<-subset(corp.all.df, corp.all.df[,1] > cut)
    #       if (nrow(corp.top) == 0) {
    #        corp.top<-data.frame(corp.all.df[1,1], row.names(corp.all.df)[1])
    #        row.names(corp.top)<-as.character(corp.top[,2])
    #       }
    #### ALTERNATIVE: take differential and cut when distance between 2 obs is greater than the next one, i.e. looking for a significant drop
    #COMMENT lines above if you use this alternative
    if (length(corp.all) > 2){
      if (diff(corp.all)[[1]] < diff(corp.all)[[2]]){
        corp.top<-corp.all.df[1,]
      } else {
        for (n in 1:(length(corp.all)-2)){
          if (diff(corp.all)[[n+1]] < diff(corp.all)[[n]]){
            corp.top<-corp.all.df[1:(n+1),]
            break
          } else {
            corp.top<-corp.all.df
          }
        }
      }
    } else {
      corp.top<-corp.all.df
    }
    ###COMMENT TO HERE###
    corp.df<-corp.top
    #create edge list
    for (j in 1:nrow(corp.df)) {
      source<-row.names(cosine.matrix)[i]
      target<-row.names(corp.df)[j]
      weight<-corp.top[j,1]
      count.target<-wordcount.df[row.names(wordcount.df) == row.names(corp.df)[j],]
      count.diff<-(count.source-count.target)/min(count.source, count.target)
      abs.diff<-abs(count.diff)
      work.diff<-which(row.names(cosine.matrix) == as.character(source))-which(row.names(cosine.matrix) == as.character(target))
      newRow<-data.frame(source, target, weight, count.source, count.target, count.diff, abs.diff, work.diff, stringsAsFactors = F)
      edge.list<-rbind(edge.list, newRow)
    }
  }
  #to write the edge lists requires directory of directories for every author in your corpus
  #setwd(homedir)  
  #file.final<-paste(filenames0[m], "_EdgeList.csv", sep="")
  #write.csv(edge.list, file=file.final)
  
  #### Step 3: Network Analysis ####
  author.edge<-edge.list[,1:3]
  #adjust 0 values and negative values
  author.edge$weight<-unlist(lapply(author.edge$weight, function(x) if (x<=0){x<-0.0001} else {x}))
  #make node list
  author.nodes<-append(edge.list$source, edge.list$target)
  author.nodes<-data.frame(author.nodes[!duplicated(author.nodes)])
  colnames(author.nodes)<-c("name")
  author.split<-cSplit(author.nodes, "name", sep="_", type.convert=FALSE)
  author.split<-cbind(author.nodes, author.split)
  g<-graph.data.frame(author.edge, directed=TRUE, vertices=NULL)
  #to measure diameter distance use inverse of similarity score
  author.edge2<-data.frame(source=author.edge[,1], target=author.edge[,2], weight=((1/author.edge[,3])*100))
  g.dist<-graph.data.frame(author.edge2, directed=TRUE, vertices=NULL)
  #create undirected graph as well
  g3<-graph.data.frame(author.edge, directed=FALSE, vertices=NULL)
  #give attributes
  #establish date for every poem drawn from filename
  V(g)$date<-as.numeric(author.split$name_1)
  #turn this into a decade
  V(g)$decade<-as.numeric(substr(author.split$name_1, 1,3))
  #summary calculations
  career.length<-max(as.numeric(author.split$name_1))-min(as.numeric(author.split$name_1))
  #number poems
  no.nodes<-length(V(g))
  #number connections
  no.edges<-length(E(g))
  #average connections per node
  mean.degree<-mean(degree(g))
  #standard deviation of connections per node
  sd.degree<-sd(degree(g))
  #percent of nodes w 1 connection
  per.1.degree<-degree_distribution(g)[2]
  #percent 1-3 connection
  per.1.to.3.degree<-sum(degree_distribution(g)[2:4]) 
  #percent 1-5 connection
  per.1.to.5.degree<-sum(degree_distribution(g)[2:6]) 
  #+5 connections
  per.plus.five.degree<-sum(degree_distribution(g)[7:length(degree_distribution(g))]) 
  #transitivity = # closed loops (triangles)
  trans.score<-transitivity(g, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))
  #calculate avg. temporal distance between nodes for every edge
  date.v<-vector()
  for(i in 1:nrow(author.edge)){
    date.dist<-as.numeric(author.split[as.character(author.edge[i,1]) == as.character(author.split$name),2])-as.numeric(author.split[as.character(author.edge[i,2]) == as.character(author.split$name),2])
    date.v<-append(date.v, date.dist)
  }
  #average distance between connected poems
  avg.date.diff<-mean(date.v)
  #standard deviation of distance between connected poems
  sd.date.diff<-sd(date.v)
  #percent of career avg. poems span
  norm.avg.date.diff<-mean(date.v)/career.length 
  #percent of poems spanning more than a decade
  per.non.decade<-length(which(date.v > 9))/length(date.v) 
  #average poem similarity
  avg.poem.similarity<-mean(E(g)$weight)
  sd.poem.similarity<-sd(E(g)$weight)
  #assortativity by date measures the extent to which poems are connected to poems closer to them in time
  assort.date<-assortativity(g, V(g)$date, directed=TRUE)
  #number communities
  #louvain method
  comm.louvain<-cluster_louvain(g3, weights = E(g)$weight)
  groups.louvain<-max(comm.louvain$membership)
  mod.louvain<-modularity(comm.louvain)
  #walktrap method
  #based on the random walk theory that you will get trapped more in a community than not.
  #comm.walk<-walktrap.community(g, weights = E(g)$weight, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
  #groups.walk<-max(comm.walk$membership)
  #mod.walk<-modularity(comm.walk)
  ###fastgreedy
  #comm.fast<-cluster_fast_greedy(g3, weights=E(g)$weight)
  #groups.fast<-max(comm.fast$membership)
  #mod.fast<-modularity(comm.fast)
  #robustness = random deletion of nodes
  #this currently runs 100x and takes the average -- could change to 1,000 for more reliable result
  robustness_vector<-vector()
  for (j in 1:100) {
    g.robust<-as.undirected(g)
    for (i in 1:length(V(g))) {
      random.node<-sample(1:length(V(g.robust)), 1, replace = FALSE)
      g.robust<-delete.vertices(g.robust, v=random.node)
      if (is.connected(g.robust)==FALSE)
        break
    }
    robustness_vector<-append(robustness_vector, i)
  }
  #average robustness across all runs
  robust.mean<-mean(robustness_vector)
  robust.sd<-sd(robustness_vector)
  #vulnerability = targeted deletion of nodes via descending order of degree
  #vulnerability1 = when the remaining graph is less than half the size of the original graph
  #deg<-names(sort(degree(g),TRUE))
  #g.vulnerable<-as.undirected(g)
  #for (i in 1:length(V(g))) {
  #  g.vulnerable<-delete.vertices(g.vulnerable, v=as.character(deg[[i]]))
  #  if (max(components(g.vulnerable)$csize) < (length(V(g))/2))
  #    break
  #}
  #vulnerability1<-i/length(V(g))
  #vulnerability2 = when the largest remaining component is no longer 50% bigger than the next largest
  deg<-names(sort(degree(g),TRUE))
  g.vulnerable<-as.undirected(g)
  for (i in 1:length(V(g))) {
    g.vulnerable<-delete.vertices(g.vulnerable, v=as.character(deg[[i]]))
    #components(g.vulnerable)$csize
    #components(g.vulnerable)$no
    if (components(g.vulnerable)$no > 1){
      if ((.5*max(components(g.vulnerable)$csize)) < sort(components(g.vulnerable)$csize, decreasing=T)[2])
        break
    }
  }
  vulnerability<-i/length(V(g))
  #add date differential to edge list and write
  edge.list$date.diff<-date.v
  setwd(homedir)
  file.final<-paste(filenames0[m], "_EdgeList.csv", sep="")
  write.csv(edge.list, file=file.final)
  #combine all network scores
  author.name<-as.character(filenames0[m])
  avg.length<-mean(scaling1)
  gender<-as.character(meta$gender[m])
  century<-meta$century[m]
  vuln.nodes<-i
  temp.df<-data.frame(author.name, gender, century, avg.length,career.length, no.nodes, no.edges, mean.degree, sd.degree,+
                        per.1.degree, per.1.to.3.degree, per.1.to.5.degree, per.plus.five.degree,+
                        trans.score, avg.date.diff, sd.date.diff, norm.avg.date.diff, per.non.decade,+
                        avg.poem.similarity, sd.poem.similarity, assort.date, groups.louvain,+
                        mod.louvain, robust.mean, robust.sd, vulnerability, vuln.nodes)
  final.df<-rbind(final.df, temp.df)
}
setwd(homedir)
write.csv(final.df, file="Global_Vulnerability_English.csv")

#### Fig. 6.4  ####
#the data for the figures was generated using gephi and can be found
#in the two gephi files for reproducing the illustrations in Fig6.4_Data
#Droste_Reduced.gephi = Fig. 6.4
##Droste_All.gephi = the full network for comparison (not shown in book)
#inherit edge list from above
a<-read.csv("DrosteHuelshoffPoetry_EdgeList.csv")

#make graph object
author.edge<-a[,2:4]
g<-graph.data.frame(author.edge, directed=TRUE, vertices=NULL)
#calculate vulnerability
deg<-names(sort(degree(g),TRUE))
g.vulnerable<-as.undirected(g)
for (i in 1:length(V(g))) {
  g.vulnerable<-delete.vertices(g.vulnerable, v=as.character(deg[[i]]))
  #components(g.vulnerable)$csize
  #components(g.vulnerable)$no
  if (components(g.vulnerable)$no > 1){
    if ((.5*max(components(g.vulnerable)$csize)) < sort(components(g.vulnerable)$csize, decreasing=T)[2])
      break
  }
}

#find breakpoint
i

#create node list
author.nodes<-append(author.edge$source, author.edge$target)
author.nodes<-data.frame(author.nodes[!duplicated(author.nodes)])

#label by pre-break point
#identify top connected nodes prior to the breakpoint
deg.sub<-deg[1:17]
author.nodes$label<-0
#label nodes if they occur in this subset
author.nodes$label<-unlist(lapply(author.nodes$name, function(x) if (as.character(x) %in% as.character(deg.sub)){author.nodes$label<-1} else {author.nodes$label<-0}))

#observe nodes in the two main components
#list all components and their sizes
components(g.vulnerable)$csize
#get membership of 2 largest components
comp.a<-data.frame(which(components(g.vulnerable)$membership == 3))
comp.b<-data.frame(which(components(g.vulnerable)$membership == 5))
comp.a$poems<-row.names(comp.a)
comp.b$poems<-row.names(comp.b)
write.csv(comp.a, file="Droste_A.csv")
write.csv(comp.b, file="Droste_B.csv")

#write edge list minus pre-break nodes (top degree nodes)
node.sub<-author.nodes[author.nodes$label != 1,]
node.remove<-author.nodes[author.nodes$label == 1,]
edge.sub<-author.edge[!as.character(author.edge$source) %in% as.character(node.remove$name), ]
edge.sub<-edge.sub[!as.character(edge.sub$target) %in% as.character(node.remove$name), ]
write.csv(node.sub, file="Droste_Nodes_Reduced.csv")
write.csv(edge.sub, file="Droste_Edges_Reduced.csv")
write.csv(author.nodes, file="Droste_Nodes_All.csv")
write.csv(node.remove, file="Droste_Nodes_Removed.csv")

#observe vocabulary differences between the two large components
corpus1 <- VCorpus(DirSource("DrosteHuelshoffPoetry"), readerControl=list(language="German"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, removeWords, problems)
corpus1 <- tm_map(corpus1, stemDocument, language = language2)
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#divide into two components
a<-corpus1.matrix[row.names(corpus1.matrix) %in% row.names(comp.a),]
b<-corpus1.matrix[row.names(corpus1.matrix) %in% row.names(comp.b),]
#run distinctive words test
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}
word1<-colSums(a)
word2<-colSums(b)
all1<-sum(word1)
all2<-sum(word2)
results <- data.frame(word = colnames(a), 
                      group1=word1,
                      group2=word2,
                      G2 = 0,
                      fisher.OR = 0,
                      fisher.p = 0)
for (j in 1:ncol(a)){
  cont.table<-data.frame(c(word1[j], all1-word1[j]), c(word2[j], all2-word2[j]))
  fish<-fisher.test(cont.table)
  LLR = 2*sum(cont.table)*(H(cont.table)-H(rowSums(cont.table))-H(colSums(cont.table)))
  results$G2[j] = LLR
  results$fisher.OR[j] = fish$estimate
  results$fisher.p[j] = fish$p.value
}
dunning.df<-results[order(-results$G2),] #sort by G2
dunning.df<-dunning.df[dunning.df$fisher.p < 0.05,] #remove non-significant words
dunning.sort<-dunning.df
dunning.sort$diff<-dunning.sort$group1-dunning.sort$group2
G2_Sort.v<-vector()
for (i in 1:nrow(dunning.sort)){
  if (dunning.sort$diff[i] <= 0){
    G2_Sort<--dunning.sort$G2[i]
  } else {
    G2_Sort<-dunning.sort$G2[i]
  }
  G2_Sort.v<-append(G2_Sort.v, G2_Sort)
}
#this gives you the distinctive words for group A (negative values represent words distinctive of group B)
dunning.sort<-cbind(dunning.sort, G2_Sort.v)
dunning.sort<-dunning.sort[order(-dunning.sort$G2_Sort.v),]

#6.5
#### Local Vulnerability ####

#this script takes as input a collection of poets' corpuses represented in 3 forms: lexical, POS, Phonetic 
#currently it can work in 3 languages (German, French, English)
#it expects filenames in the following format:
#DATE_AnyOtherInformationAboutPoem.txt #it will use the date prior to underscore to calculate period dates
#it generates a similarity matrix for all poems across four domains: the 3 above and a semantic dimension using LSA
#it then identifies: moments (i.e. poems) in the poet's career that diverge significantly from the expected 
#stylistic range up to that point in his or her career -- these are called moments of vulnerability
#where significance = values that occur less than 1% of the time in N random permutations of the data.
#this script outputs:
#1. a directory of vulnerability tables
#these are tables which list which poems fall below the expected stylistic range of the poet's career up to that point
#2. a directory of vulnerability graphs
#these allow you to observe the moments (poems) where that vulnerability is happening 
#3. an output table called vulnerability.local that stores two different vulnerability measures
#described in the script and whether the final quarter of the career has the highest amount of vulnerability

data(stopwords_de)
data(stopwords_en)
data(stopwords_fr)
language1<-c("French")
language2<-c("french")
#remove bad words
#problems<-c("drum", "habt", "hast", "ichs", "ists", "sei", "wär", "weimar", "zwei", "seite", "apparat", "datumsangaben")
problems<-vector()
#set directories
homedir<-paste("INSERT DIRECTORY")
lexdir<-paste("PoetryAuthors_French")
phondir<-paste("PoetryAuthors_French_PHON")
posdir<-paste("PoetryAuthors_French_POS")
vulndir<-paste("PoetryAuthors_French_Vulnerability")
vuln.graph.dir<-paste("PoetryAuthors_French_Vulnerability_Graphs")
setwd(homedir)
filenames0<-list.files(lexdir, full.names=FALSE)
filenames1<-list.files(posdir, full.names=FALSE)
filenames2<-list.files(phondir, full.names=FALSE)
#ingest metadata about poets birth and death dates
meta<-read.csv("PoetryAuthors_French_Meta.csv")

#run
results.df<-NULL
vulnerability.local<-NULL
periods.df<-NULL
for (m in 1:length(filenames0)) {
  print(m)
  author<-as.character(meta$author[m])
  #LEXICAL
  dir<-paste(homedir, lexdir, sep = "/")
  setwd(dir)
  corpus1 <- VCorpus(DirSource(filenames0[m]), readerControl=list(language=language1))
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  corpus1 <- tm_map(corpus1, removeWords, problems)
  corpus1 <- tm_map(corpus1, stemDocument, language = language2)
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  scaling1<-rowSums(corpus1.matrix)
  wordcount.df<-data.frame(scaling1)
  #remove stopwords
  corpus1 <- tm_map(corpus1, removeWords, stopwords(language1))
  #remake dtm
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  #tfidf
  corpus.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)
  corpus.tfidf.mat<-as.matrix(corpus.tfidf, stringsAsFactors=F)

  #LSA
  #SET STOPWORDS MANUALLY
  dir<-paste(homedir, lexdir, filenames0[m], sep = "/")
  nums<-c(1:5000)
  myMatrix<-textmatrix(dir, stemming=TRUE, language=language2, minWordLength=2, maxWordLength=FALSE, minDocFreq=1, maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE, stopwords=stopwords_fr, vocabulary=NULL, phrases=NULL, removeXML=FALSE, removeNumbers=FALSE)
  myMatrix<-myMatrix[!row.names(myMatrix) %in% nums,]
  myMatrix<-myMatrix[!row.names(myMatrix) %in% problems,]
  myMatrix<-lw_logtf(myMatrix) * gw_idf(myMatrix)
  myLSAspace<-lsa(myMatrix, dims=dimcalc_share())
  cosine.dist2<-simil(t(as.textmatrix(myLSAspace)), method="cosine")
  cosine.matrix2<-as.matrix(cosine.dist2, stringsAsFactors=F)*100
  
  #POS
  dir<-paste(homedir, posdir, sep = "/")
  setwd(dir)
  corpus2 <- VCorpus(DirSource(filenames1[m]), readerControl=list(language="English"))
  corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
  corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
  #take 1-2 POSgrams
  BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
  corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
  corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
  corpus2.tfidf<-weightTfIdf(corpus2.dtm, normalize = TRUE)
  corpus2.tfidf.mat<-as.matrix(corpus2.tfidf, stringsAsFactors=F)
  
  #PHONEMES
  dir<-paste(homedir, phondir, sep = "/")
  setwd(dir)
  corpus3 <- VCorpus(DirSource(filenames2[m]))
  corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
  corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf), removePunctuation = FALSE, stopwords = FALSE, tolower=FALSE))
  corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F)
  corpus3.tfidf<-weightTfIdf(corpus3.dtm, normalize = TRUE)
  corpus3.tfidf.mat<-as.matrix(corpus3.tfidf, stringsAsFactors=F)
  
  #add POS & PHONEMES to LEXICAL DTM
  all.tfidf<-cbind(corpus.tfidf.mat, corpus2.tfidf.mat, corpus3.tfidf.mat)
  #similarity matrix
  cosine.dist1<-simil(all.tfidf, method = "cosine")
  cosine.matrix1<-as.matrix(cosine.dist1, stringsAsFactors=F)*100
  #adjust for length
  cosine.matrix1<-((1/log(scaling1))*cosine.matrix1)*10
  
  #combine LSA with LEX/POS/PHON
  cosine.matrix1[is.na(cosine.matrix1)]<-0 #Lex/POS/PHON Matrix
  cosine.matrix2[is.na(cosine.matrix2)]<-0 #LSA Matrix
  cosine.matrix<-(cosine.matrix1+cosine.matrix2)/2
  
  #get lower triangle
  cormat<-cosine.matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  lower_tri <- get_lower_tri(cormat)
  lower_tri[lower_tri == 0] <- NA 
  #these are the observed values
  #this takes the average similarity between a poem and every other poem prior to it
  obs.v<-vector()
  for (i in 2:nrow(lower_tri)){
    sub<-lower_tri[i,]
    sub<-sub[!is.na(sub)]
    mean.work<-mean(sub)
    obs.v<-append(obs.v, mean.work)
  }
  #smooth using rolling mean, window of ten poems
  obs.roll<-rollmean(obs.v, k=10, na.pad = T) #set k for window, here 10
  #then permute matrix n times
  #this allows you to test for significance, i.e. when is a change in similarity significant
  #by permuting the actual matrix we see the extent to which an observed value exceeds the expected range of values up to that point
  #here I use a bootstrapping approach that samples with replacement
  #I sample from the entire matrix because it makes it harder for a poem to be identified as vulnerable
  #the idea is that given the overall variability throughout the poet's entire career, which moments look more vulnerable
  final.df<-NULL
  perm.df<-NULL
  for (k in 2:nrow(lower_tri)){
    sub<-lower_tri
    #alternative = subset original matrix until poem k
    #sub<-lower_tri[1:k, 1:k]
    #turn all values into a single vector
    sub.v<-unlist(as.data.frame(sub), use.names=FALSE)
    sub.v<-sub.v[!is.na(sub.v)]
    #sample from those values with replacement N times
    mean.v<-vector()
    for (j in 1:200){
      #sample values with replacement and keep as many as there are poems prior to poem at time k
      sample.v<-sample(sub.v, (k-1), replace = T)
      #take the mean similarity for this sample (i.e. an imaginary poem)
      mean.sample<-mean(sample.v)
      #store in a vector
      mean.v<-append(mean.v, mean.sample)
    }
    #append this vector to table
    #this table represents the permuted similarity scores of every poem
    #i.e. it represents the range of possible values that a poet has available at that time in his/her career
    perm.df<-cbind(perm.df, mean.v)
  }
  #find the values in the 99th and 1st percentile for each poem at time k
  perm.high<-apply(perm.df, 2, function(x) quantile(x, c(.99)))
  perm.low<-apply(perm.df, 2, function(x) quantile(x, c(.01)))
  #smooth
  high.roll<-rollmean(perm.high, k=10, na.pad = T)
  low.roll<-rollmean(perm.low, k=10, na.pad = T)
  #create data frame
  #add one more NA cell to each 
  poem.ids<-row.names(cosine.matrix)[2:nrow(cosine.matrix)]
  final.df<-data.frame(high.roll, low.roll, obs.roll, poem.ids)
  
  #export graph as separate file
  #requires pre-existing directory
  file.name<-paste(filenames0[m], "_Vulnerability_Graph.pdf", sep="")
  dir<-paste(homedir, vuln.graph.dir, sep="/")
  setwd(dir)
  #graph
  pdf(file.name,width=8,height=6)
  plot(final.df$obs.roll, type="line", main = author, xlab = "Poems", ylab="Similarity")
  lines(final.df$high.roll, type = "line", lty=2)
  lines(final.df$low.roll, type = "line", lty=2)
  dev.off()
  
  #export table of values
  #this looks for those moments that are above the expected ranges
  #it outputs a table of values above and below the significance bands
  #this allows you to observe where the moments of vulnerability are
  #all positive values are above/below the sig bands
  final.df$diff.high<-final.df$obs.roll-final.df$high.roll
  final.df$diff.low<-final.df$low.roll-final.df$obs.roll
  #this requires a new directory; see above under "vulndir"
  file.name<-paste(filenames0[m], "_Vulnerability_Table.csv", sep="")
  dir<-paste(homedir, vulndir, sep="/")
  setwd(dir)
  write.csv(final.df, file=file.name)
  
  #vuln.score1 = % of poems that have less than 1% chance of being w/in normal range of similarity to rest of poems
  #in other words = % poems that are significantly dissimilar from the rest of the corpus up to that point
  vuln.score1<-length(which(final.df$diff.low > 0))/nrow(final.df)
  #vuln.score2 = ratio of similar to dissimilar moments (X has 1.4x as many overly similar moments to overly dissimilar ones)
  vuln.score2<-length(which(final.df$diff.high > 0))/length(which(final.df$diff.low > 0))
  #vuln.final.quart = percentage of vulnerable poems that fall in final quarter of all poems
  #this allows you to observe extent to which vulnerability is a late phenomenon
  all.vuln1<-which(final.df$diff.low > 0)
  vuln.final.quart<-length(all.vuln1[all.vuln1>nrow(final.df)-(nrow(final.df)/4)])/length(all.vuln1)
  gender<-meta$gender[m]
  century<-meta$century[m]
  birth<-as.numeric(meta$birth.date[m])
  death<-as.numeric(meta$death.date[m])
  temp.df<-data.frame(author, birth, death, gender, century, vuln.score1, vuln.score2, vuln.final.quart)
  vulnerability.local<-rbind(vulnerability.local, temp.df)
}
setwd(homedir)
write.csv(vulnerability.local, file="Vulnerability_Local_French.csv", row.names = F)

### Fig. 6.5 ####
#Local vulnerability in four poets
#input are local vulnerability tables produced by Script 3.2
library(gridExtra)
library(ggplot2)
a<-read.csv("Vulnerability_Table_Hugo.csv")
b<-read.csv("Vulnerability_Table_Droste.csv")
c<-read.csv("Vulnerability_Table_Rich.csv")
d<-read.csv("Vulnerability_Table_Auden.csv")

tit.size<-14
p1<-ggplot(a, aes(x=X, y=obs.roll/100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  geom_line(aes(x=X, y=a$high.roll/100), linetype="dotted") +
  geom_line(aes(x=X, y=a$low.roll/100), linetype="dotted") +
  labs(title = "Victor Hugo", x="Poems over time", y="Similarity")

p2<-ggplot(b, aes(x=X, y=obs.roll/100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  geom_line(aes(x=X, y=b$high.roll/100), linetype="dotted") +
  geom_line(aes(x=X, y=b$low.roll/100), linetype="dotted") +
  labs(title = "Droste-Hülshoff", x="Poems over time", y="Similarity")

p3<-ggplot(c, aes(x=X, y=obs.roll/100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  geom_line(aes(x=X, y=c$high.roll/100), linetype="dotted") +
  geom_line(aes(x=X, y=c$low.roll/100), linetype="dotted") +
  labs(title = "Adrienne Rich", x="Poems over time", y="Similarity")

p4<-ggplot(d, aes(x=X, y=obs.roll/100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  geom_line(aes(x=X, y=d$high.roll/100), linetype="dotted") +
  geom_line(aes(x=X, y=d$low.roll/100), linetype="dotted") +
  #ylim(c(0, 0.22)) +
  labs(title = "W.H. Auden", x="Poems over time", y="Similarity")

#library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol=2)

#ratio of local to global vulnerability in all poets
a<-read.csv("Vulnerability_Local_All.csv")
b<-read.csv("Vulnerability_Global_All.csv")
a<-a[order(as.character(a$language), as.character(a$author)),]
b<-b[order(as.character(b$language), as.character(b$author.name)),]
#take inverse of global vulnerability
#in original measure higher = less vulnerable, i.e. more of the network needs to be removed before splitting in two
glob<-1-b$vulnerability
glob<-scale(glob, center = T, scale = T)
loc<-scale(a$vuln.score1, center = T, scale = T)
author<-a$author
label<-b$label2
#label<-b$author.name

#gender<-b$gender
all.df<-data.frame(author, label, glob, loc)
all.df$combined<-all.df$glob+all.df$loc
all.df$low.high<-all.df$loc-all.df$glob
all.df$high.low<--all.df$loc+all.df$glob
#
low.low<-all.df[order(all.df$combined)[1:5],]
low.high<-all.df[order(all.df$low.high)[1:5],]
high.low<-all.df[order(all.df$high.low)[1:5],]
high.high<-all.df[order(-all.df$combined)[1:5],]

#### Fig. 6.6 ####
### Career Types by two types of vulnerability ###
#library("ggplot2")
ggplot(all.df, aes(x=loc, y=glob)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_point(size=1) +
  #stat_ellipse(linetype="dotted") +
  #theme(legend.position="none") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_text(aes(label=label), size=3, hjust=0.5, vjust=-0.5) +
  scale_x_continuous(limits=c(-3, 3)) +  
  scale_y_continuous(limits=c(-3, 3)) +
  labs(x="Local Vulnerability", y="Global Vulnerability", caption="\nFig. 6.6 Career types based on two types of poetic vulnerbality.\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")
  #labs(x="Local Vulnerability", y="Global Vulnerability")

#6.6
#### Foote Novelty ####
#this script detects moments of significant change within a poet's corpus that are akin to "periods"
#rather than observe single moments that exceed expectation as in the vulnerability score above
#this script looks for larger windows when the amount of similarity decreases significantly

data(stopwords_de)
data(stopwords_en)
data(stopwords_fr)
language1<-c("French")
language2<-c("french")
#remove bad words
#problems<-c("drum", "habt", "hast", "ichs", "ists", "sei", "wär", "weimar", "zwei", "seite", "apparat", "datumsangaben")
problems<-vector()
#set directories
homedir<-paste("~/Documents/2. Books/Enumerations/Enumerations - Data and Code/Data/txtlab_POETRY_CW")
lexdir<-paste("PoetryAuthors_French")
phondir<-paste("PoetryAuthors_French_PHON")
posdir<-paste("PoetryAuthors_French_POS")
foote.dir<-paste("PoetryAuthors_French_FooteNovelty_Graphs")
setwd(homedir)
filenames0<-list.files(lexdir, full.names=FALSE)
filenames1<-list.files(posdir, full.names=FALSE)
filenames2<-list.files(phondir, full.names=FALSE)
#ingest metadata about poets birth and death dates
meta<-read.csv("PoetryAuthors_French_Meta.csv")
results.df<-NULL
periods.df<-NULL
for (m in 1:length(filenames0)) {
  print(m)
  final.df<-NULL
  author<-as.character(meta$author[m])
  #LEXICAL
  dir<-paste(homedir, lexdir, sep = "/")
  setwd(dir)
  corpus1 <- VCorpus(DirSource(filenames0[m]), readerControl=list(language=language1))
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  corpus1 <- tm_map(corpus1, removeWords, problems)
  corpus1 <- tm_map(corpus1, stemDocument, language = language2)
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  scaling1<-rowSums(corpus1.matrix)
  wordcount.df<-data.frame(scaling1)
  #remove stopwords
  corpus1 <- tm_map(corpus1, removeWords, stopwords(language1))
  #remake dtm
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  #tfidf
  corpus.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)
  corpus.tfidf.mat<-as.matrix(corpus.tfidf, stringsAsFactors=F)
  
  #LSA
  #SET STOPWORDS MANUALLY
  dir<-paste(homedir, lexdir, filenames0[m], sep = "/")
  nums<-c(1:5000)
  myMatrix<-textmatrix(dir, stemming=TRUE, language=language2, minWordLength=2, maxWordLength=FALSE, minDocFreq=1, maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE, stopwords=stopwords_fr, vocabulary=NULL, phrases=NULL, removeXML=FALSE, removeNumbers=FALSE)
  myMatrix<-myMatrix[!row.names(myMatrix) %in% nums,]
  myMatrix<-myMatrix[!row.names(myMatrix) %in% problems,]
  myMatrix<-lw_logtf(myMatrix) * gw_idf(myMatrix)
  myLSAspace<-lsa(myMatrix, dims=dimcalc_share())
  cosine.dist2<-simil(t(as.textmatrix(myLSAspace)), method="cosine")
  cosine.matrix2<-as.matrix(cosine.dist2, stringsAsFactors=F)*100
  
  #POS
  dir<-paste(homedir, posdir, sep = "/")
  setwd(dir)
  corpus2 <- VCorpus(DirSource(filenames1[m]), readerControl=list(language="English"))
  corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
  corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
  #take 1-2 POSgrams
  BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
  corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
  corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
  corpus2.tfidf<-weightTfIdf(corpus2.dtm, normalize = TRUE)
  corpus2.tfidf.mat<-as.matrix(corpus2.tfidf, stringsAsFactors=F)
  
  #PHONEMES
  dir<-paste(homedir, phondir, sep = "/")
  setwd(dir)
  corpus3 <- VCorpus(DirSource(filenames2[m]))
  corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
  corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf), removePunctuation = FALSE, stopwords = FALSE, tolower=FALSE))
  corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F)
  corpus3.tfidf<-weightTfIdf(corpus3.dtm, normalize = TRUE)
  corpus3.tfidf.mat<-as.matrix(corpus3.tfidf, stringsAsFactors=F)
  
  #add POS & PHONEMES to LEXICAL DTM
  all.tfidf<-cbind(corpus.tfidf.mat, corpus2.tfidf.mat, corpus3.tfidf.mat)
  #similarity matrix
  cosine.dist1<-simil(all.tfidf, method = "cosine")
  cosine.matrix1<-as.matrix(cosine.dist1, stringsAsFactors=F)*100
  #adjust for length
  cosine.matrix1<-((1/log(scaling1))*cosine.matrix1)*10
  
  #combine LSA with LEX/POS/PHON
  cosine.matrix1[is.na(cosine.matrix1)]<-0 #Lex/POS/PHON Matrix
  cosine.matrix2[is.na(cosine.matrix2)]<-0 #LSA Matrix
  cosine.matrix<-(cosine.matrix1+cosine.matrix2)/2
  
  #run foote novelty
  cormat<-cosine.matrix
  cormat[cormat==0]<-100
  #scale the values
  cormat.scale <- apply(cormat, 2, function(x) (x-min(x))/(max(x)-min(x)))
  #set window of poems to detect "period", here 20
  win<-20 
  #make the matrix that slides across the similarity matrix to calculate windows of dissimilarity
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
  #smooth the foote novelty values
  foote.roll<-rollmean(foote.obs, k=20, na.pad=TRUE)
  foote.roll<-append(rep(NA,9), foote.roll) #make same length as original poem collection
  foote.roll<-append(foote.roll, rep(NA,10))

  #permute n times
  #here again we want to establish an outer bound above which we can say that the degree of dissimilarity
  #is significantly more than random variation within the corpus
  #this time we permute all values in the table but maintain the symmetrical structure
  
  #first construct lower triangle as above
  cormat<-cosine.matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  lower_tri <- get_lower_tri(cormat)
  lower_tri[lower_tri == 0] <- NA
  
  #then permute N times
  perm.vec<-vector()
  for (i in 1:200){
    lower_tri_perm<-NULL
      #permute every column of the similarity matrix
      for (j in 2:ncol(lower_tri)){
        #extract column
        col.v<-unname(lower_tri[j:nrow(lower_tri), (j-1)])
        if (length(col.v) > 1){
          #permute the column
          col.v<-sample(col.v)
        }
        #reconstruct the original matrix
        #add NA to missing values
        add.v<-rep(NA, (j-1))
        col.v<-append(add.v, col.v)
        #rebuild original matrix
        lower_tri_perm<-cbind(lower_tri_perm,col.v)
      }
    #copy lower_tri_perm to upper triangle to make symmetrical matrix
    lower_tri_perm[upper.tri(lower_tri_perm)]<-lower_tri_perm[lower.tri(lower_tri_perm)]
    lower_tri_perm[is.na(lower_tri_perm)]<-100
    #scale
    perm.scale <- apply(lower_tri_perm, 2, function(x) (x-min(x))/(max(x)-min(x)))
    #run foote novelty measure
    #this calculates the foote novelty score for every time k in the poet's career
    #it then stores those values in a single vector
    #then take the 90th percentile score to create upper significance band
    #amounts above this level only occur less than 10% of the time in all permutations
    perm.obs<-vector()
    for (k in 1:(ncol(perm.scale)-foote.win)){
      perm.sub<-perm.scale[k:(k+foote.win), k:(k+foote.win)]
      perm.sub.m<-perm.sub*foote.m
      foote.score<-sum(perm.sub.m)
      perm.obs<-append(perm.obs, foote.score)
    }
    perm.vec<-append(perm.vec, perm.obs)
  }
  #calc significance band
  perm.high<-quantile(perm.vec, c(.90))[[1]]
  foote.prep<-foote.roll[is.na(foote.roll) != TRUE]
  #smooth again to remove minor peaks
  foote2<-rollmean(foote.prep, k=floor(length(foote.obs)/10))
  foote2.prep<-foote2
  foote2.prep<-append(rep(NA, (length(foote.roll)-length(foote2))/2), foote2.prep)
  foote2.prep<-append(foote2.prep, rep(NA, (length(foote.roll)-length(foote2))/2))
  #find significant peaks
  #note, peaks cannot be within 20 poems of each other. A period is pre-defined as 20+ poems
  #this avoids minor variations and takes the largest peak
  sig<-which(foote2.prep > perm.high)
  if (length(sig) > 0){
    diff.sig<-which(diff(sig) > 19)
    diff.sig<-append(diff.sig, length(sig))
    change.v<-vector()
    if (length(diff.sig) > 0) {
      start<-1
      for (l in 1:(length(diff.sig))){
        sub<-foote2.prep[start:sig[diff.sig[l]]]
        sub[is.na(sub)]<-0
        change.p<-(which(sub == max(sub))+(start-1))
        change.v<-append(change.v, change.p)
        start<-sig[diff.sig[l]]+1
      }
    }
    #export plot
    #requires pre-existing directory
    #file.name<-paste(filenames0[m], "_FooteNovelty_Graph.pdf", sep="")
    #dir<-paste(homedir, foote.dir, sep="/")
    #setwd(dir)
    #graph
    #pdf(file.name,width=8,height=6)
    #plot(foote2.prep, type="line", main = author, xlab = "Poems", ylab="Change") #rolling mean of this rolling mean
    #abline(h=perm.high, lty=2)
    #abline(v=change.v)
    #dev.off()

    #calculate measures related to inferred periods
    #no. periods
    periods<-length(change.v)+1
    author.meta<-data.frame(row.names(cormat))
    colnames(author.meta)<-c("dates")
    author.split<-cSplit(author.meta, "dates", sep="_", type.convert=FALSE)
    periods.sub<-author.split[change.v,]
    periods.temp<-data.frame(row.names(cormat)[change.v])
    periods.temp<-cbind(author, periods.temp)
    #this table lists poems at the peaks for further analysis
    periods.df<-rbind(periods.df, periods.temp)
    period.marks<-as.numeric(periods.sub$dates_1)
    period.marks<-append(as.numeric(author.split$dates_1)[1], period.marks)
    period.marks<-append(period.marks,as.numeric(author.split$dates_1)[length(author.split$dates_1)])
    period.lengths<-diff(period.marks)
    #calculate the poet's longest period
    longest.period<-max(period.lengths)
    #is the shortest period the final period?
    if (which(period.lengths == min(period.lengths)) == length(period.lengths)){
      when.shortest.period<-c("final")
    } else {
      when.shortest.period<-c("not_final")
    }
    #length of the first period
    duration.first.period<-as.numeric(periods.sub$dates_1[1])-as.numeric(author.split$dates_1[1])
    year.first.period<-as.numeric(periods.sub$dates_1[1])
    year.last.period<-as.numeric(periods.sub$dates_1[length(periods.sub$dates_1)])
    #length of the last period
    duration.last.period<-as.numeric(author.split$dates_1[length(author.split$dates_1)])-as.numeric(periods.sub$dates_1[length(periods.sub$dates_1)])
    birth<-as.numeric(meta$birth.date[m])
    death<-as.numeric(meta$death.date[m])
    #age of poet when first period ends
    age.first.period<-year.first.period-birth
    #age of poet when last period starts
    age.last.period<-year.last.period-birth
  } else {
    periods<-0
    duration.first.period<-0
    year.first.period<-0
    year.last.period<-0
    duration.last.period<-0
    birth<-as.numeric(meta$birth.date[m])
    death<-as.numeric(meta$death.date[m])
    age.first.period<-0
    age.last.period<-0
    when.shortest.period<-c("none")
    longest.period<-0
  }
  gender<-meta$gender[m]
  century<-meta$century[m]
  temp.df<-data.frame(author, birth, death, gender, century, periods,+
                        year.first.period, age.first.period, duration.first.period,+ 
                        year.last.period, age.last.period, duration.last.period,+
                        longest.period, when.shortest.period)
  results.df<-rbind(results.df, temp.df)
}
setwd(homedir)
#this table lists all of the features related to period detection and local vulnerability
write.csv(results.df, file="Foote_French_Results.csv")
#this table lists the poems that occur at the moment of period change for further analysis
write.csv(periods.df, file="Foote_French_Poems.csv")


#### Fig. 6.7 #####
#example of period detection using Foote novelty on the poetry of J.W. Goethe
#this is identical to Script 6.6 but takes a single poet as input and only runs period detection
#this takes as input a poet's collected works in the 3 representations used here (LEX, POS, PHON)
#make sure to set language settings throughout (stemming and stopwords)
library(proxy)
#includes a list of words to be removed
#input directory names
#requires 3 representations of every poet's work (lexical, syntactic, phonetic)
filenames0<-c("Goethe_LEX")
filenames1<-c("Goethe_POS")
filenames2<-c("Goethe_PHON")
problems<-c("drum", "habt", "hast", "ichs", "ists", "sei", "wär", "weimar", "zwei", "seite", "apparat", "datumsangaben")
#LEXICAL
corpus1 <- VCorpus(DirSource(filenames0), readerControl=list(language="German"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1 <- tm_map(corpus1, removeWords, problems)
corpus1 <- tm_map(corpus1, stemDocument, language = "german")
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
scaling1<-rowSums(corpus1.matrix)
wordcount.df<-data.frame(scaling1)
#remove stopwords
corpus1 <- tm_map(corpus1, removeWords, stopwords("German"))
#remake dtm
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#tfidf
corpus.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)
corpus.tfidf.mat<-as.matrix(corpus.tfidf, stringsAsFactors=F)
#LSA
data(stopwords_de)
nums<-c(1:5000)
myMatrix<-textmatrix(filenames0, stemming=TRUE, language="german", minWordLength=2, maxWordLength=FALSE, minDocFreq=1, maxDocFreq=FALSE, minGlobFreq=FALSE, maxGlobFreq=FALSE, stopwords=stopwords_de, vocabulary=NULL, phrases=NULL, removeXML=FALSE, removeNumbers=FALSE)
myMatrix<-myMatrix[!row.names(myMatrix) %in% nums,]
myMatrix<-myMatrix[!row.names(myMatrix) %in% problems,]
myMatrix<-lw_logtf(myMatrix) * gw_idf(myMatrix)
myLSAspace<-lsa(myMatrix, dims=dimcalc_share())
#make first similarity matrix based on semantic representation
cosine.dist2<-simil(t(as.textmatrix(myLSAspace)), method="cosine")
cosine.matrix2<-as.matrix(cosine.dist2, stringsAsFactors=F)*100
#POS
corpus2 <- VCorpus(DirSource(filenames1), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
#take 1-2 POSgrams
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
corpus2.tfidf<-weightTfIdf(corpus2.dtm, normalize = TRUE)
corpus2.tfidf.mat<-as.matrix(corpus2.tfidf, stringsAsFactors=F)
#PHONEMES
corpus3 <- VCorpus(DirSource(filenames2))
corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf), removePunctuation = FALSE, stopwords = FALSE, tolower=FALSE))
corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F)
#tfidf
corpus3.tfidf<-weightTfIdf(corpus3.dtm, normalize = TRUE)
corpus3.tfidf.mat<-as.matrix(corpus3.tfidf, stringsAsFactors=F)
#combine POS & PHONEMES & LEXICAL Tables
all.tfidf<-cbind(corpus.tfidf.mat, corpus2.tfidf.mat, corpus3.tfidf.mat)
#make similarity matrix based on combined feature space
cosine.dist1<-simil(all.tfidf, method = "cosine")
cosine.matrix1<-as.matrix(cosine.dist1, stringsAsFactors=F)*100
#adjust for length
cosine.matrix1<-((1/log(scaling1))*cosine.matrix1)*10
#clean
cosine.matrix1[is.na(cosine.matrix1)]<-0
cosine.matrix2[is.na(cosine.matrix2)]<-0
#combine LSA similarity matrix with LEX/POS/PHON and take average similarity
cosine.matrix<-(cosine.matrix1+cosine.matrix2)/2
### foote novelty ###
#this script detects moments of significant change within a poet's corpus that are akin to "periods"
#rather than observe single moments that exceed expectation as in the vulnerability score above
#this script looks for larger windows when the amount of similarity decreases significantly
cormat<-cosine.matrix
cormat[cormat==0]<-100
#scale the values
cormat.scale <- apply(cormat, 2, function(x) (x-min(x))/(max(x)-min(x)))
#set window of poems to detect "period", here 20
win<-20 
#make the matrix that slides across the similarity matrix to calculate windows of dissimilarity
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
#smooth the foote novelty values
foote.roll<-rollmean(foote.obs, k=20, na.pad=TRUE)
foote.roll<-append(rep(NA,9), foote.roll) #make same length as original poem collection
foote.roll<-append(foote.roll, rep(NA,10))
#permute n times
#here again we want to establish an outer bound above which we can say that the degree of dissimilarity
#is significantly more than random variation within the corpus
#this time we permute all values in the table but maintain the symmetrical structure

#first construct lower triangle as above
cormat<-cosine.matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
lower_tri <- get_lower_tri(cormat)
lower_tri[lower_tri == 0] <- NA

#then permute N times
perm.vec<-vector()
for (i in 1:200){
  print(i)
  lower_tri_perm<-NULL
  #permute every column of the similarity matrix
  for (j in 2:ncol(lower_tri)){
    #extract column
    col.v<-unname(lower_tri[j:nrow(lower_tri), (j-1)])
    if (length(col.v) > 1){
      #permute the column
      col.v<-sample(col.v)
    }
    #reconstruct the original matrix
    #add NA to missing values
    add.v<-rep(NA, (j-1))
    col.v<-append(add.v, col.v)
    #rebuild original matrix
    lower_tri_perm<-cbind(lower_tri_perm,col.v)
  }
  #copy lower_tri_perm to upper triangle to make symmetrical matrix
  lower_tri_perm[upper.tri(lower_tri_perm)]<-lower_tri_perm[lower.tri(lower_tri_perm)]
  lower_tri_perm[is.na(lower_tri_perm)]<-100
  #scale
  perm.scale <- apply(lower_tri_perm, 2, function(x) (x-min(x))/(max(x)-min(x)))
  #run foote novelty measure
  #this calculates the foote novelty score for every time k in the poet's career
  #it then stores those values in a single vector
  #then take the 90th percentile score to create upper significance band
  #amounts above this level only occur less than 10% of the time in all permutations
  perm.obs<-vector()
  for (k in 1:(ncol(perm.scale)-foote.win)){
    perm.sub<-perm.scale[k:(k+foote.win), k:(k+foote.win)]
    perm.sub.m<-perm.sub*foote.m
    foote.score<-sum(perm.sub.m)
    perm.obs<-append(perm.obs, foote.score)
  }
  perm.vec<-append(perm.vec, perm.obs)
}
#calc significance band
perm.high<-quantile(perm.vec, c(.90))[[1]]
foote.prep<-foote.roll[is.na(foote.roll) != TRUE]
#smooth again to remove minor peaks
foote2<-rollmean(foote.prep, k=floor(length(foote.obs)/10))
foote2.prep<-foote2
foote2.prep<-append(rep(NA, (length(foote.roll)-length(foote2))/2), foote2.prep)
foote2.prep<-append(foote2.prep, rep(NA, (length(foote.roll)-length(foote2))/2))
#find significant peaks
#note, peaks cannot be within 20 poems of each other. A period is pre-defined as 20+ poems
#this avoids minor variations and takes the largest peak
sig<-which(foote2.prep > perm.high)
if (length(sig) > 0){
  diff.sig<-which(diff(sig) > 19)
  diff.sig<-append(diff.sig, length(sig))
  change.v<-vector()
  if (length(diff.sig) > 0) {
    start<-1
    for (l in 1:(length(diff.sig))){
      sub<-foote2.prep[start:sig[diff.sig[l]]]
      sub[is.na(sub)]<-0
      change.p<-(which(sub == max(sub))+(start-1))
      change.v<-append(change.v, change.p)
      start<-sig[diff.sig[l]]+1
    }
  }
}

#plot Fig. 6.7
tit.size=14
foote.df<-data.frame(foote2.prep)
foote.df$index<-c(1:length(foote2.prep))
ggplot(foote.df, aes(x=index, y=foote.df$foote2.prep/100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=tit.size), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  geom_hline(yintercept=perm.high/100, linetype="dashed") +
  geom_vline(xintercept=change.v) +
  annotate("text", x=(change.v[1]-10), y = 21/100, srt=90, label=substring(row.names(cosine.matrix)[change.v][1], 1, 4))+
  annotate("text", x=(change.v[2]-10), y = 21/100, srt=90, label=substring(row.names(cosine.matrix)[change.v][2], 1, 4))+
  annotate("text", x=(change.v[3]-10), y = 21/100, srt=90, label=substring(row.names(cosine.matrix)[change.v][3], 1, 4))+
  annotate("text", x=(change.v[4]-10), y = 21/100, srt=90, label=substring(row.names(cosine.matrix)[change.v][4], 1, 4))+
  #labs(x="Poems over time", y="Change")
  labs(x="Poems", y="Change", caption="\nFig. 6.7 Period predictions for J.W. Goethe's collected poems\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018")

#6.7
### Table 6.1 ###
## Period Statistics ##
a<-read.csv("FooteNovelty_Results_All.csv")

#% of poets w no period
length(which(a$periods == 0))/nrow(a)

#avg age when poet undergoes first period
mean(a$age.first.period)
median(a$age.first.period)

#mode of most common decade
table(substr(a$age.first.period, 1,1))

#duration of first period
mean(a$duration.first.period)
median(a$duration.first.period)
sort(a$duration.first.period)

#duration of longest period
mean(a$X.longest.period)
hist(a$X.longest.period)
t.test(a$X.longest.period)

#what % of poets have shortest period at end of life (if they have 2 or more periods)
sub<-a[a$periods > 1,]
length(which(sub$when.shortest.period == "final"))/nrow(sub)

#are late periods usually longer or shorter than early periods?
wilcox.test(a$duration.first.period, a$duration.last.period)

#what % of poets have majority of their vulnerable poems occurring late in life?
b<-read.csv("Vulnerability_Local_All.csv")
length(which(b$vuln.final.quart > .5))/nrow(b)

############# Section 3: Late Style ##################
#this set of scripts examines poets' late periods to understand the way aging and creativity intersect

#6.8
## Difficulty ##
#this script implements 3 different readability scores
#to assess the degree of difficulty over the course of a poet's career
#Make sure to change language below accordingly!!!!!
library(koRpus)
homedir<-paste("INSERT DIRECTORY")
lexdir<-paste("PoetryAuthors_German")
setwd(homedir)
filenames0<-list.files(lexdir, full.names=FALSE)
final.df<-NULL
for (m in 1:length(filenames0)){
  author<-filenames0[m]
  language<-lexdir
  dir1<-paste(homedir, lexdir,sep = "/")
  dir2<-paste(homedir, lexdir,filenames0[m],sep = "/")
  setwd(dir1)
  filenames1<-list.files(filenames0[m], full.names = FALSE)
  setwd(dir2)
  for (j in 1:length(filenames1)) {
    title<-filenames1[j]
    work<-scan(filenames1[j], what="character", quote="")
    work.clean<- gsub("\\d", "", work)
    text.whole<-paste(work.clean, collapse=" ")
    text.char<-as.String(text.whole)
    tag.doc<-tokenize(text.char, format = "obj", lang="de") #CHANGE LANGUAGE!!!!!
    #flesch.mod<-flesch(tag.doc)
    tuldava.mod<-tuldava(tag.doc, quiet=T)
    #fucks.mod<-fucks(tag.doc)
    #flesch.score<-flesch.mod @ Flesch $RE
    tuldava.score<-tuldava.mod @ Tuldava $Tuldava
    #fucks.score<-fucks.mod @ Fucks $Fucks
    #temp.df<-data.frame(language, author, title, flesch.score, tuldava.score, fucks.score)
    temp.df<-data.frame(language, author, title, tuldava.score)
    final.df<-rbind(final.df, temp.df)
  }
}
#write.csv(final.df, file="Difficulty_German_All.csv")

#Significance Test
a<-read.csv("Difficulty_All.csv")
difficulty.df<-NULL
for (i in 1:nlevels(a$author)){
  a.sub<-a[a$author == levels(a$author)[i],]
  #define late period: here defined as final quarter of poet's output
  final.quart<-round(nrow(a.sub)/4)
  #create the actual two groups: late, notlate
  diff.notlate<-a.sub[1:(nrow(a.sub)-final.quart),]
  diff.late<-a.sub[(nrow(a.sub)-(final.quart-1)):nrow(a.sub),]
  #calculate test statistic
  test<-median(diff.late$tuldava.score)-median(diff.notlate$tuldava.score)
  #run randomization test
  pred.v<-vector()
  for (j in 1:1000){
    #create fictional career by randomly shuffling the order of the poems (rows)
    permute.m<-a.sub[sample(nrow(a.sub), nrow(a.sub)),]
    #subset it by the same sample sizes of late, notlate in the actual data
    late.m<-permute.m[1:final.quart,]
    notlate.m<-permute.m[(final.quart+1):nrow(permute.m),]
    #calculcate test statistic
    test.p<-median(late.m$tuldava.score)-median(notlate.m$tuldava.score)
    #append to vector
    pred.v<-append(pred.v, test.p)
  }
  #for each author label significance using a two-taled test
  #if test statistic is above the 97.5 percentile, then it is significantly high for the late period
  if (test > quantile(pred.v, c(.975))[[1]]){
    difficulty<-c("late")
  #if test statistic is below the 2.5 percentile, then it is significantly low for the late period
  } else if (test < quantile(pred.v, c(.025))[[1]]){
    difficulty<-c("early")
  #if neither of the above then there is no significant difference between late and early period in terms of the test statistic
  } else {
    difficulty<-c("neither")
  }
  #store the test statistic
  late.tuldava.difference<-test
  #what % of the career is the most difficulty located across a window of 20 poems 
  max.difficulty.window.tuldava<-which(rollmean(a.sub$tuldava.score, k=20) == max(rollmean(a.sub$tuldava.score, k=20)))[[1]]/nrow(a.sub)
  author<-levels(a$author)[i]
  language<-as.character(a.sub$language[1])
  temp.df<-data.frame(author, language, difficulty, late.tuldava.difference, max.difficulty.window.tuldava)
  difficulty.df<-rbind(difficulty.df, temp.df)
}
#write.csv(difficulty.df, file="Difficulty_Results_All.csv")

#calculate scores based on difficulty results
a<-read.csv("Difficulty_Results_All.csv")

#what % of poets have significantly higher or lower difficulty late in their career?
length(which(a$difficulty != "neither"))/nrow(a)

#of this group, how many get more difficult (and not less)
length(which(a$difficulty == "late"))/nrow(a)
length(which(a$difficulty == "early"))/nrow(a)

#6.9
## Syntactic Irregularity ##
#this script takes as input poems rendered as parts of speech
#it calculates total tfidf score for each poem to measure the poems/windows where there
#is the greatest amount of syntactical irregularity/rarity
#in which documents do we see more frequent rare syntactical patterns?
#at what point in the career?
library(tm)
library(zoo)
language<-c("English")
filenames<-list.files("PoetryAuthors_English_POS", full.names = FALSE)
final.df<-NULL
for (m in 1:length(filenames)){
  print(m)
  #ingest the POS corpus
  corpus2 <- VCorpus(DirSource(filenames[m]), readerControl=list(language="English")) #don't change language since using POS
  corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
  corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
  #take 1-3 POSgrams
  BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1:3), paste, collapse = " "), use.names = FALSE)
  corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
  corpus2.tfidf<-weightTfIdf(corpus2.dtm, normalize = TRUE)
  corpus2.tfidf.mat<-as.matrix(corpus2.tfidf, stringsAsFactors=F)
  #separate by late period, defined as last quartile of all poems
  final.quart<-round(nrow(corpus2.tfidf.mat)/4)
  corpus.notlate<-corpus2.tfidf.mat[1:(nrow(corpus2.tfidf.mat)-final.quart),]
  corpus.late<-corpus2.tfidf.mat[(nrow(corpus2.tfidf.mat)-(final.quart-1)):nrow(corpus2.tfidf.mat),]
  #calculate the test statistic
  test<-median(rowSums(corpus.late))-median(rowSums(corpus.notlate))
  #run randomization test
  pred.v<-vector()
  for (j in 1:1000){
    #create fictional career by randomly shuffling the order of the poems (rows)
    permute.m<-corpus2.tfidf.mat[sample(nrow(corpus2.tfidf.mat), nrow(corpus2.tfidf.mat)),]
    #subset it by the same sample sizes of late, notlate in the actual data
    late.m<-permute.m[1:final.quart,]
    notlate.m<-permute.m[(final.quart+1):nrow(permute.m),]
    #calculcate test statistic
    test.p<-median(rowSums(late.m))-median(rowSums(notlate.m))
    #append to vector
    pred.v<-append(pred.v, test.p)
  }
  #for each author label significance using a two-taled test
  #if test statistic is above the 97.5 percentile, then it is significantly high for the late period
  if (test > quantile(pred.v, c(.975))[[1]]){
    irregularity<-c("late")
    #if test statistic is below the 2.5 percentile, then it is significantly low for the late period
  } else if (test < quantile(pred.v, c(.025))[[1]]){
    irregularity<-c("early")
    #if neither of the above then there is no significant difference between late and early period in terms of the test statistic
  } else {
    irregularity<-c("neither")
  }
  #store test statistic
  late.irregularity.difference<-test
  #what % of the career is the most syntactical diversity located across a window of 20 poems 
  all.df<-rowSums(corpus2.tfidf.mat)
  max.irregularity.window<-which(rollmean(all.df, k=20) == max(rollmean(all.df, k=20)))[[1]]/nrow(corpus2.tfidf.mat)
  author<-filenames[m]
  temp.df<-data.frame(author, language, irregularity, late.irregularity.difference, max.irregularity.window)
  final.df<-rbind(final.df, temp.df)
}
#french<-final.df
#german<-final.df
english<-final.df
all<-rbind(french, german, english)
#write.csv(all, file="Syntactic_Irregularity_All.csv")

#create measures for syntactic irregularity
a<-read.csv("Syntactic_Irregularity_All.csv")

#what % of poets have significant change in syntactic patterns late in their career
length(which(a$irregularity != "neither"))/nrow(a)

#what % of poets increase / decrease
length(which(a$irregularity == "late"))/nrow(a)
length(which(a$irregularity == "early"))/nrow(a)

#how much more likely is the max window of irregularity to be in the late period than any other?
quart4<-length(which(a$max.irregularity.window > 0.75))
quart3<-length(which(a$max.irregularity.window < 0.75 & a$max.irregularity.window > 0.5))
quart2<-length(which(a$max.irregularity.window < 0.5 & a$max.irregularity.window > 0.25))
quart1<-length(which(a$max.irregularity.window < 0.25))
length(which(a$max.irregularity.window > 0.75))/max(quart3, quart2, quart1)

#6.10
#### Vocabulary Richness #####
#whose vocabularies begin to decline in their late career?

library("SnowballC")
library("koRpus")

lang.stem<-c("english")
lang<-c("PoetryAuthors_English")
dir0<-paste("INSERT DIRECTORY")
dir1<-paste("INSERT DIRECTORY", lang, sep="")
setwd(dir0)
filenames0<-list.files(lang, full.names=FALSE)
setwd(dir1)
s.size<-499
ttr.df<-NULL
for (i in 1:length(filenames0)) {
  print(i)
  setwd(dir1)
  dir2<-paste(dir1, filenames0[i], sep="/")
  filenames1<-list.files(filenames0[i], full.names=FALSE)
  setwd(dir2)
  final.quart<-round(length(filenames1)/4)
  #create word vector of all early poems
  early<-vector()
  for (j in 1:(length(filenames1)-final.quart)){
    work<-scan(filenames1[j], what="character", quote="", quiet=T)
    early<-append(early, work)
  }
  early.clean<- gsub("\\d", "", early)
  early.clean<- tolower(early.clean)
  early.clean<-strsplit(early.clean, "\\W")
  early.clean<-unlist(early.clean)
  not.blanks<-which(early.clean!="")
  early.v<-early.clean[not.blanks]
  early.v<-wordStem(early.v, language=lang.stem)
  #create word vector for late poems
  late<-vector()
  for (j in ((length(filenames1)-final.quart)+1):length(filenames1)){
    work<-scan(filenames1[j], what="character", quote="", quiet=T)
    late<-append(late, work)
  }
  late.clean<- gsub("\\d", "", late)
  late.clean<- tolower(late.clean)
  late.clean<-strsplit(late.clean, "\\W")
  late.clean<-unlist(late.clean)
  not.blanks<-which(late.clean!="")
  late.v<-late.clean[not.blanks]
  late.v<-wordStem(late.v, language=lang.stem)
  #take 100 random contiguous samples from each and calculate TTR ratio
  early.ttr.v<-vector()
  for (k in 1:100) {
    beg<-sample(1:(length(early.v)-s.size),1)
    test<-early.v[beg:(beg+s.size)]
    ttr.sample<-length(unique(test))/length(test)
    early.ttr.v<-append(early.ttr.v, ttr.sample)
  }
  late.ttr.v<-vector()
  for (k in 1:100) {
    beg<-sample(1:(length(late.v)-s.size),1)
    test<-late.v[beg:(beg+s.size)]
    ttr.sample<-length(unique(test))/length(test)
    late.ttr.v<-append(late.ttr.v, ttr.sample)
  }
  #calculate the test statistic
  test.statistic<-median(late.ttr.v)-median(early.ttr.v)
  #run randomization test
  #combine early and late vectors
  all.v<-append(early.v, late.v)
  #then rerun random samples on fictional corpus
  pred.v<-vector()
  for (m in 1:1000) {
    #create fictional early samples
    early.ttr.v<-vector()
    for (k in 1:100) {
      beg<-sample(1:(length(all.v)-s.size),1)
      test<-all.v[beg:(beg+s.size)]
      ttr.sample<-length(unique(test))/length(test)
      early.ttr.v<-append(early.ttr.v, ttr.sample)
      }
    #create fictional late samples
    late.ttr.v<-vector()
    for (k in 1:100) {
      beg<-sample(1:(length(all.v)-s.size),1)
      test<-all.v[beg:(beg+s.size)]
      ttr.sample<-length(unique(test))/length(test)
      late.ttr.v<-append(late.ttr.v, ttr.sample)
      }
    #calculate test statistic
    perm.test<-median(late.ttr.v)-median(early.ttr.v)
    #store for every permutation
    pred.v<-append(pred.v, perm.test)
  }
  #if test statistic is above the 97.5 percentile, then it is significantly high for the late period
  if (test.statistic > quantile(pred.v, c(.975))[[1]]){
    richness<-c("late")
    #if test statistic is below the 2.5 percentile, then it is significantly low for the late period
  } else if (test.statistic < quantile(pred.v, c(.025))[[1]]){
    richness<-c("early")
    #if neither of the above then there is no significant difference between late and early period in terms of the test statistic
  } else {
    richness<-c("neither")
  }
  #store test statistic
  late.richness.difference<-test.statistic
  author<-filenames0[i]
  language<-lang.stem
  temp.df<-data.frame(author, language, richness, late.richness.difference)
  ttr.df<-rbind(ttr.df, temp.df)
}
#german<-ttr.df
#french<-ttr.df
#english<-ttr.df
all.df<-rbind(german, french, english)
#write.csv(all.df, "TTR_All_Late.csv")

#Examine all values
a<-read.csv("TTR_All_Late.csv")

#what % of poets have significantly higher or lower vocabulary richness late in their career?
length(which(a$richness != "neither"))/nrow(a)

#of this group, how many expand their vocabularies
sub<-a[a$richness != "neither",]
length(which(sub$richness == "late"))/nrow(a)
length(which(sub$richness == "early"))/nrow(a)

#6.11
## As a whole career (ignoring late v. early) ##
lang.stem<-c("english")
lang<-c("PoetryAuthors_English")
dir0<-paste("INSERT DIRECTORY")
dir1<-paste("INSERT DIRECTORY", lang, sep="")
setwd(dir0)
filenames0<-list.files(lang, full.names=FALSE)
setwd(dir1)
s.size<-499
ttr.df<-NULL
for (i in 1:length(filenames0)) {
  setwd(dir1)
  dir2<-paste(dir1, filenames0[i], sep="/")
  filenames1<-list.files(filenames0[i], full.names=FALSE)
  setwd(dir2)
  all<-vector()
  for (j in 1:length(filenames1)){
    work<-scan(filenames1[j], what="character", quote="")
    all<-append(all, work)
  }
  all.clean<- gsub("\\d", "", all)
  all.clean<- tolower(all.clean)
  all.clean<-strsplit(all.clean, "\\W")
  all.clean<-unlist(all.clean)
  not.blanks<-which(all.clean!="")
  all.v<-all.clean[not.blanks]
  all.v<-wordStem(all.v, language=lang.stem)
  ttr.v<-vector()
  for (k in 1:1000) {
    beg<-sample(1:(length(all.v)-s.size),1)
    test<-all.v[beg:(beg+s.size)]
    ttr.sample<-length(unique(test))/length(test)
    ttr.v<-append(ttr.v, ttr.sample)
  }
  ttr.avg<-mean(ttr.v)
  ttr.med<-median(ttr.v)
  ttr.sd<-sd(ttr.v)
  ttr.temp<-data.frame(filenames0[i], ttr.avg, ttr.med, ttr.sd)
  ttr.df<-rbind(ttr.df, ttr.temp)
}
#write.csv(ttr.df, file="TTR_English.csv")

#6.12
### Concreteness Score ###
library(tm)
library(SnowballC)
# this measures the extent to which the ratio between things and abstractions is
# higher / lower in the late period
# it takes as input files that have been transformed in the following 2 steps:
#1. only nouns are kept
#2. those nouns are translated into their hypernym representations using the first most common word sense
#it compares hypernyms represented as "physical entities" or "abstractions" and compares their ratio in the early and late period
#the more physical entities there are relative to abstractions the more "concrete" a poem is said to be
#thus for each period we get a single ratio of things to abstractions
#we then use a permutation to test whether the difference between these ratios in the late period is random fluctuation or signals a significant difference either high or low

#change languages accordingly
#English
language1<-c("English")
language2<-c("english")
physical.word<-c("physical_entity")
abstraction.word<-c("abstraction")
#French
# language1<-c("French")
# language2<-c("french")
# physical.word<-c("chose")
# abstraction.word<-c("abstraction")
#German
#language1<-c("German")

#because the German wordnet is differently configured I compare "Objekt" and any form of "kognitiv"
#this is a very close approximation to physical entity and abstraction (all abstractions usually contain a label for feeling and/or cognition, and all feelings in German also contain the stem for kognitiv)
#the code must be changed specifically in the script below; marked accordingly.

#select appropriate directory
filenames<-list.files("PoetryAuthors_English_Hypernyms", full.names = FALSE)

#run
final.df<-NULL
for (m in 1:length(filenames)){
  print(m)
  corpus3 <- VCorpus(DirSource(filenames[m]), readerControl=list(language=language1))
  corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
  corpus3 <- tm_map(corpus3, content_transformer(tolower))
  corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf)))
  corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F) 
  #divide periods
  final.quart<-round(nrow(corpus3.matrix)/4)
  rest<-nrow(corpus3.matrix)-final.quart
  #find observed score
  early.act<-corpus3.matrix[1:rest,]
  keep1<-which(colSums(early.act) > 0)
  early.act<-early.act[,keep1]
  late.act<-corpus3.matrix[(nrow(corpus3.matrix)-final.quart+1):nrow(corpus3.matrix),]
  keep2<-which(colSums(late.act) > 0)
  late.act<-late.act[,keep2]
  
  #for late period
  late.ratio.actual<-unname(colSums(late.act)[which(colnames(late.act) == physical.word)])/unname(colSums(late.act)[which(colnames(late.act) == abstraction.word)])
  
  #for German
  #late.ratio.actual<-unname(colSums(late.act)[which(colnames(late.act) == "objekt")])/sum(unname(colSums(late.act)[grep("kognitiv", colnames(late.act))]))
  
  #for early period
  early.ratio.actual<-unname(colSums(early.act)[which(colnames(early.act) == physical.word)]/colSums(early.act)[which(colnames(early.act) == abstraction.word)])
 
  #for German
  #early.ratio.actual<-unname(colSums(early.act)[which(colnames(early.act) == "objekt")])/sum(unname(colSums(early.act)[grep("kognitiv", colnames(early.act))]))
  
  #test statistic
  test<-late.ratio.actual-early.ratio.actual
  
  #ratio of late to early
  #the higher the ratio the more the late period increases its use of things rather than abstractions
  #it does not mean that there are absolutely more things (its possible to use more things relatively while still using fewer things overall)
  #in order to test for significance we will permute the overall matrix N times with replacement
  #do we see this degree of change between early and late in less than 5% of cases?
  #if so, then we conclude that there is significant difference in the concreteness in the late period
  #permute
  pred.v<-vector()
  for (i in 1:1000){
    #permute the whole matrix
    permute.m<-corpus3.matrix[sample(nrow(corpus3.matrix), nrow(corpus3.matrix)),]
    #subset it to the same size as the actual periods
    early.m<-permute.m[(final.quart+1):nrow(permute.m),]
    late.m<-permute.m[1:final.quart,]
    
    #calculate a predicted concrete/abstraction ratio
    late.ratio.pred<-colSums(late.m)[which(colnames(late.m) == physical.word)][[1]]/colSums(late.m)[which(colnames(late.m) == abstraction.word)][[1]]
    early.ratio.pred<-colSums(early.m)[which(colnames(early.m) == physical.word)][[1]]/colSums(early.m)[which(colnames(early.m) == abstraction.word)][[1]]
  
    #for German
    #late.ratio.pred<-unname(colSums(late.m)[which(colnames(late.m) == "objekt")])/sum(unname(colSums(late.m)[grep("kognitiv", colnames(late.m))]))
    #early.ratio.pred<-unname(colSums(early.m)[which(colnames(early.m) == "objekt")])/sum(unname(colSums(early.m)[grep("kognitiv", colnames(early.m))]))
    
    #test statistic
    test.p<-late.ratio.pred-early.ratio.pred
    
    #store
    pred.v<-append(pred.v, test.p)
  }
  #test for significance
  #question 1: does the late period have a significantly higher ratio of things to abstractions than by chance?
  if (test > quantile(pred.v, c(.975))[[1]]){
    concreteness<-c("late")
  #question 2: does the late period have a significantly lower ratio of things to abstractions than by chance?
  } else if (test < quantile(pred.v, c(.025))[[1]]){
    concreteness<-c("early")
  #or is there no difference between late and early period in terms of the thing/abstraction ratio
  } else {
    concreteness<-c("neither")
  }
  late.concreteness.diff<-test
  author<-filenames[m]
  language<-language1
  temp.df<-data.frame(author, language, concreteness, late.concreteness.diff)
  final.df<-rbind(final.df, temp.df)
}
#german<-final.df
#french<-final.df
english<-final.df
all<-rbind(german,french,english)
write.csv(all, file="Concreteness_All.csv", row.names = F)

#make calculations about all poets
a<-read.csv("Concreteness_All.csv")
#% of poets who have significant change in their concreteness in late period
length(which(a$concreteness != "neither"))/nrow(a)

#% of poets who have significant increase in late period
length(which(a$concreteness == "late"))/nrow(a)

#% of poets who have significant decrease
length(which(a$concreteness == "early"))/nrow(a)

#6.13
### Generality ###
#This looks at whether late writing is more general/particular than the earlier writing
#what % of words in the late period (group A) are hypernyms of words in the rest of the poetry (group B?
#i.e. how much more general is A than B?
#this counts all words in A that are hypernyms of B and sums
#to do so it loads regular words in A and the hypernyms of B and then subsets A %in% B
#it then compares the distributions of A IN B and B IN A to see which corpus is more 
#general than the other. 
#Version 1 = Winner Takes All
#if A has 20 "furnitures" and B has 1 "chair->hypernym furniture" this counts as 20
#because A has 20 words that are a hypernym of a word in B
#we want to observe how much generality (or specificity) a text has
#subset 1 according to hypernyms in 2
library("tm")
library("SnowballC")
library("RWeka")
root<-c("DIRECTORY") 
setwd(root)
group1<-c("PoetryAuthors_English")
group2<-c("PoetryAuthors_English_Hypernyms")
dir1<-paste("DIRECTORY", group1, sep="")
dir2<-paste("DIRECTORY", group2, sep="")
filenames1<-list.files(group1, full.names = FALSE)
filenames2<-list.files(group2, full.names=FALSE)
language1<-c("English")
final.df<-NULL
for (i in 1:length(filenames1)){
  print(i)
  #load regular words
  setwd(dir1)
  corpus1 <- VCorpus(DirSource(filenames1[i]), readerControl=list(language=language1))
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  #load hypernyms
  setwd(dir2)
  corpus2 <- VCorpus(DirSource(filenames2[i]), readerControl=list(language=language1))
  corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
  corpus2 <- tm_map(corpus2, content_transformer(tolower))
  corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("_", " ", x)))
  corpus2.dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
  corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F) 
  #divide into early and late
  final.quart<-round(nrow(corpus2.matrix)/4) #this is the definition of the late period
  #early hypernyms (no this is not mislabeled)
  late.hyp<-corpus2.matrix[(nrow(corpus2.matrix)-(final.quart-1)):nrow(corpus2.matrix),]
  late.hyp<-late.hyp[,colSums(late.hyp) != 0]
  early.reg<-corpus1.matrix[1:(nrow(corpus1.matrix)-final.quart),]
  early.reg<-early.reg[,colSums(early.reg) != 0]
  #late hypernyms (ditto)
  late.reg<-corpus1.matrix[(nrow(corpus1.matrix)-(final.quart-1)):nrow(corpus1.matrix),]
  late.reg<-late.reg[,colSums(late.reg) != 0]
  early.hyp<-corpus2.matrix[1:(nrow(corpus2.matrix)-final.quart),]
  early.hyp<-early.hyp[,colSums(early.hyp) != 0]
  #calculate test statistic = late hypernymy minus early hypernymy
  #hypernymy = % reg words of corpus A in hypernyms of corpus B
  late.actual<-sum(late.reg[,which(colnames(late.reg) %in% colnames(early.hyp))])/sum(late.reg)
  early.actual<-sum(early.reg[,which(colnames(early.reg) %in% colnames(late.hyp))])/sum(early.reg)
  test<-late.actual-early.actual
  #run randomization test
  pred.v<-vector()
  for (j in 1:1000){
    #permute both samples
    hyp.perm<-corpus2.matrix[sample(nrow(corpus2.matrix), nrow(corpus2.matrix)),]
    reg.perm<-corpus1.matrix[sample(nrow(corpus1.matrix), nrow(corpus1.matrix)),]
    #early hypernyms (no this is not mislabeled)
    late.hyp<-hyp.perm[(nrow(hyp.perm)-(final.quart-1)):nrow(hyp.perm),]
    late.hyp<-late.hyp[,colSums(late.hyp) != 0]
    early.reg<-reg.perm[1:(nrow(reg.perm)-final.quart),]
    early.reg<-early.reg[,colSums(early.reg) != 0]
    #late hypernyms (ditto)
    late.reg<-reg.perm[(nrow(reg.perm)-(final.quart-1)):nrow(reg.perm),]
    late.reg<-late.reg[,colSums(late.reg) != 0]
    early.hyp<-hyp.perm[1:(nrow(hyp.perm)-final.quart),]
    early.hyp<-early.hyp[,colSums(early.hyp) != 0]
    #calculate test statistic = late hypernymy minus early hypernymy
    #hypernymy = % reg words of corpus A in hypernyms of corpus B
    late.p<-sum(late.reg[,which(colnames(late.reg) %in% colnames(early.hyp))])/sum(late.reg)
    early.p<-sum(early.reg[,which(colnames(early.reg) %in% colnames(late.hyp))])/sum(early.reg)
    test.p<-late.p-early.p
    pred.v<-append(pred.v,test.p)
  }
  if (test > quantile(pred.v, c(.975))[[1]]){
    generality<-c("late")
    #question 2: does the late period have a significantly lower ratio of things to abstractions than by chance?
  } else if (test < quantile(pred.v, c(.025))[[1]]){
    generality<-c("early")
    #or is there no difference between late and early period in terms of the thing/abstraction ratio
  } else {
    generality<-c("neither")
  }
  late.generality.diff<-test
  author<-filenames1[i]
  language<-language1
  temp.df<-data.frame(author, language, generality, late.generality.diff)
  final.df<-rbind(final.df, temp.df)
}
#german<-final.df
#french<-final.df
english<-final.df
all<-rbind(german, french, english)
#write.csv(all, file="Generality_All.csv")

#make comparisons
a<-read.csv("Generality_All.csv")

#% of poets who show significant kind of change in their late career
length(which(a$generality != "neither"))/nrow(a)

#% increase
length(which(a$generality == "late"))/nrow(a)

#% decrease
length(which(a$generality == "early"))/nrow(a)

#6.14
### Fig. 6.8 ####
### Comparing poetic careers by all variables of challengingness
#load tables
a<-read.csv("Difficulty_Results_All.csv")
b<-read.csv("Syntactic_Irregularity_All.csv")
c<-read.csv("TTR_All_Late.csv")
d<-read.csv("Generality_All.csv")
e<-read.csv("Concreteness_All.csv")
a<-a[order(a$author),]
b<-b[order(b$author),]
c<-c[order(c$author),]
d<-d[order(d$author),]
e<-e[order(e$author),]
which(a$author != c$author)
#combine late period designations
all.late<-data.frame(a$author, a$difficulty, b$irregularity, c$richness, d$generality, e$concreteness)
late.score<-vector()
for (i in 1:nrow(all.late)){
  sub<-all.late[i,]
  late.s1<-length(which(sub[1,2:5] == "late"))
  late.s2<-length(which(sub[1,6] == "early"))
  late.s<-late.s1+late.s2
  late.score<-append(late.score, late.s)
}
all.late$late.score<-late.score
length(which(all.late$late.score > 1))/nrow(all.late)

#combine scores
all<-data.frame(a$author, a$late.tuldava.difference, b$late.irregularity.difference, c$late.richness.difference, d$late.generality.diff, -e$late.concreteness.diff)
row.names(all)<-all$a.author
all<-all[,-1]
colnames(all)<-c("Difficulty", "Irregularity", "Richness", "Generality", "Abstraction")
#scale scores
all.norm<-apply(all, 2, function (x) scale(x, center=T, scale=T))
row.names(all.norm)<-row.names(all)
#subset by those who have significant late changes
#all.sub<-all.norm[row.names(all.norm) %in% all.late$a.author[all.late$late.score > 1],]
all.sub<-read.csv("BiPlot.csv")
#names removed names for visualization
row.names(all.sub)<-all.sub$name.limit
all.sub<-all.sub[,-1]
all.sub<-all.sub[,-1]
pca<-princomp(all.sub)
biplot(pca, col=c("black", "gray50"), xlim=c(-0.6, 0.6), xlab="PC1", ylab="PC2")
biplot(pca, col=c("black", "gray50"), xlim=c(-0.6, 0.6), xlab="", ylab="")
title(sub="\n\nFig. 6.8 Relationship of poets late periods to features of stylistic challengingness\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")
#library(ggfortify)
#autoplot(prcomp(all.sub), data = all.sub, loadings = TRUE, loadings.label = TRUE, lty = 3,
#         label=T, shape=F, loadings.colour = "#000000", loadings.label.colour = "#606060")

### Fig. 6.9 ###
#Wanda Coleman Local Vulnerability Graph
#this is drawn from the outputs of Script 3.2 above
a<-read.csv("WandaColeman_Vulnerability_Table.csv")

ggplot(a, aes(x=X, y=obs.roll/100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, size=10), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_line() +
  geom_line(aes(x=X, y=a$high.roll/100), linetype="dotted") +
  geom_line(aes(x=X, y=a$low.roll/100), linetype="dotted") +
  geom_vline(xintercept = 596) +
  geom_vline(xintercept = 766) +
  ylim(0.025, 0.08) +
  #labs(x="Poems over time", y="Similarity")
  labs(x="Poems over time", y="Similarity", caption="\nFig. 6.9 Local vulnerability in the poetry of Wanda Coleman.\nVertical bars represent the collection Bathwater Wine (1998)\n\nSource: Andrew Piper, Enumerations: Data and Literary Study (2018)")

### Wanda Coleman Measures ###

#6.15
### Sentence Length ###
#comparing average lengths of sentences in Coleman's vulnerable and non-vulnerable work.
#NOTE: If this breaks, quit and restart R -- packages above will mask features for NLP packages and have them not work
require("NLP")
library("openNLP")
library("openNLPdata")
#Step 1: Run measure
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
#word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
#pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en")
setwd("~/Documents/2. Books/Enumerations/Enumerations - Chap 6 (Corpus)/Corpus - Graphs and Tables")
filenames<-list.files("WandaColeman", pattern="*.txt", full.names=FALSE)
setwd("~/Documents/2. Books/Enumerations/Enumerations - Chap 6 (Corpus)/Corpus - Graphs and Tables/WandaColeman")
#establish function to split sentences into words to assess sentence length
splitSentence<-function(x){
  x<-unlist(strsplit(x, " "))
  x<-x[x != ""]
  x<-length(x)
}
#run across all poems
length.df<-NULL
for (i in 1:length(filenames)) {
  work<-scan(filenames[i], what="character", quote="")
  work.clean<- gsub("\\d", "", work)
  text.whole<-paste(work.clean, collapse=" ") # collapse into single chunk
  text.char<-as.String(text.whole)
  a1 <- annotate(text.char, sent_token_annotator)
  sentences<-text.char[a1]
  length.v<-unlist(lapply(sentences, splitSentence))
  mean.len<-mean(length.v)
  poem<-filenames[i]
  temp.df<-data.frame(poem, mean.len)
  length.df<-rbind(length.df, temp.df)
}

#Step 2: Divide corpus in vulnerable and non-vulnerable poems and compare
a<-read.csv("WandaColeman_Vulnerability_Table.csv")
#find all poems that fall below the confidence interval for vulnerability
a.vuln<-a[a$diff.low > 0,]
a.vuln<-a.vuln[complete.cases(a.vuln),]
a.not<-a[a$diff.low < 0,]
a.not<-a.not[complete.cases(a.not),]
#subset length table by the vulnerable poems and not vulnerable poems and compare
length.vuln<-length.df[as.character(length.df$poem) %in% as.character(a.vuln$poems),]
length.not<-length.df[as.character(length.df$poem) %in% as.character(a.not$poems),]
median(length.not$mean.len)
median(length.vuln$mean.len)

#Step 3: Significance Test using bootstrapping
#take 1,000 samples of the not vulnerable work -- in how many case do you see a median value lower than the actual one?
med.v<-vector()
for (i in 1:1000){
  not.samp<-length.not[sample(nrow(length.not), nrow(length.vuln), replace = T), ]
  med<-median(not.samp$mean.len)
  med.v<-append(med.v, med)
}
quantile(med.v, c(0.01))
median(length.vuln$mean.len)

#6.16
#Punctuation per poem
library(stringr)
filenames<-list.files("WandaColeman", pattern="*.txt", full.names=FALSE)
punct.df<-NULL
for (i in 1:length(filenames)) {
  work<-scan(filenames[i], what="character", quote="")
  if (length(work) > 0){
    work.punct<-str_extract(work, "\\?|\\!|\\.|\\,|\\;|\\:|\\(|\\)")
    punct<-which(!is.na(work.punct))
    #calculate total words
    work.lower<-tolower(work) # all lower case
    work.words<-strsplit(work.lower, "\\W") # turn into a list of words
    work.word.vector<-unlist(work.words) #turn into a vector
    work.word.vector<-gsub("\\d", "", work.word.vector) #remove numbers
    work.word.vector<-work.word.vector[work.word.vector != ""]
    total.words<-length(work.word.vector)
    punct.per<-length(punct)/total.words
    poems<-filenames[i]
    temp.df<-data.frame(poems, punct.per)
    punct.df<-rbind(punct.df, temp.df)
  }
}

#Step 2: Divide corpus in vulnerable and non-vulnerable poems and compare
a<-read.csv("WandaColeman_Vulnerability_Table.csv")
#find all poems that fall below the confidence interval for vulnerability
a.vuln<-a[a$diff.low > 0,]
a.vuln<-a.vuln[complete.cases(a.vuln),]
a.not<-a[a$diff.low < 0,]
a.not<-a.not[complete.cases(a.not),]
#subset length table by the vulnerable poems and not vulnerable poems and compare
punct.vuln<-punct.df[as.character(punct.df$poem) %in% as.character(a.vuln$poems),]
punct.not<-punct.df[as.character(punct.df$poem) %in% as.character(a.not$poems),]
median(punct.vuln$punct.per)/median(punct.not$punct.per)

#Step 3: Significance Test using bootstrapping
#take 1,000 samples of the not vulnerable work -- in how many case do you see a median value lower than the actual one?
med.v<-vector()
for (i in 1:1000){
  not.samp<-punct.not[sample(nrow(punct.not), nrow(punct.vuln), replace = T), ]
  med<-median(not.samp$punct.per)
  med.v<-append(med.v, med)
}
quantile(med.v, c(0.99))
max(med.v)
median(punct.vuln$punct.per)

#6.17
#Vocabulary innovation
#what percentage of words appear in vulnerable poems that do not appear in non-vulnerable poems?
library(tm)
library(SnowballC)
corpus3 <- VCorpus(DirSource("WandaColeman"), readerControl=list(language="English"))
corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
corpus3 <- tm_map(corpus3, content_transformer(tolower))
corpus3 <- tm_map(corpus3, content_transformer(removePunctuation))
corpus3 <- tm_map(corpus3, content_transformer(removeNumbers))
corpus3 <- tm_map(corpus3, stemDocument, language = "english")
corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf)))
corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F) 

#Step 2: Divide corpus in vulnerable and non-vulnerable poems and compare
a<-read.csv("WandaColeman_Vulnerability_Table.csv")
#find all poems that fall below the confidence interval for vulnerability
a.vuln<-a[a$diff.low > 0,]
a.vuln<-a.vuln[complete.cases(a.vuln),]
a.not<-a[a$diff.low < 0,]
a.not<-a.not[complete.cases(a.not),]
#subset matrix by vuln and not vuln
vuln.m<-corpus3.matrix[row.names(corpus3.matrix) %in% as.character(a.vuln$poems),]
remove<-unname(which(colSums(vuln.m) == 0))
vuln.m<-vuln.m[,-remove]
not.m<-corpus3.matrix[row.names(corpus3.matrix) %in% as.character(a.not$poems),]
remove<-unname(which(colSums(not.m) == 0))
not.m<-not.m[,-remove]
#percent of words in vulnerable poems not in rest
actual<-length(which(!colnames(vuln.m) %in% colnames(not.m)))/ncol(vuln.m)
#number of new words in vulnerable poems
length(which(!colnames(vuln.m) %in% colnames(not.m)))
#number of new words as percentage of poems
length(which(!colnames(vuln.m) %in% colnames(not.m)))/nrow(vuln.m)

#permute
novelty.v<-vector()
for (i in 1:1000){
  permute.m<-corpus3.matrix[sample(nrow(corpus3.matrix), nrow(corpus3.matrix)),]
  early.m<-permute.m[1:nrow(not.m),]
  remove1<-unname(which(colSums(early.m) == 0))
  early.m<-early.m[,-remove1]
  late.m<-permute.m[(nrow(not.m)+1):(nrow(permute.m)-10),]
  remove2<-unname(which(colSums(late.m) == 0))
  late.m<-late.m[,-remove2]
  novelty.score<-length(which(!colnames(late.m) %in% colnames(early.m)))/ncol(late.m)
  novelty.v<-append(novelty.v, novelty.score)
}
#actual
length(which(!colnames(vuln.m) %in% colnames(not.m)))/ncol(vuln.m)
#predicted
quantile(novelty.v, c(0.99))
max(novelty.v)

#6.18
#POS Analysis
#What parts of speech are distinctive of the vulnerable poems?
corpus2 <- VCorpus(DirSource("WandaColeman_POS"), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("\\$", "", x)))
#take 1-2 POSgrams
#BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
#corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(tokenize = BigramTokenizer, wordLengths=c(1,Inf)))
corpus2.dtm<-DocumentTermMatrix(corpus2, control = list(wordLengths=c(1,Inf)))
corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F)
#remove punctuation
corpus2.sub<-corpus2.matrix[,-grep("[[:punct:]]", colnames(corpus2.matrix))]

#subset by vuln and not vuln
a<-read.csv("WandaColeman_Vulnerability_Table.csv")
#find all poems that fall below the confidence interval for vulnerability
a.vuln<-a[a$diff.low > 0,]
a.vuln<-a.vuln[complete.cases(a.vuln),]
a.not<-a[a$diff.low < 0,]
a.not<-a.not[complete.cases(a.not),]
#subset matrix by vuln and not vuln
vuln.m<-corpus2.sub[row.names(corpus2.sub) %in% as.character(a.vuln$poems),]
not.m<-corpus2.sub[row.names(corpus2.sub) %in% as.character(a.not$poems),]

#run distinctive POS test
word1<-colSums(vuln.m)
word2<-colSums(not.m)
all1<-sum(vuln.m)
all2<-sum(not.m)
fisher.df<-data.frame(word=colnames(vuln.m), group1=word1, group2=word2, fish.odds=0, fish.p=0)
for (i in 1:ncol(vuln.m)){
  cont.table<-data.frame(c(word1[i], all1-word1[i]), c(word2[i], all2-word2[i]))
  fish<-fisher.test(cont.table)
  fisher.df[i, c("fish.odds","fish.p")]<-c(fish$estimate[[1]], fish$p.value)
}
#remove insignificant words
fisher.final<-fisher.df[fisher.df$fish.p < 0.05,]
fisher.final<-fisher.final[order(-fisher.final$fish.odds),]

#6.19
#Abstraction Score
language1<-c("English")
language2<-c("english")
physical.word<-c("physical_entity")
abstraction.word<-c("abstraction")

corpus3 <- VCorpus(DirSource("WandaColeman_HYP"), readerControl=list(language=language1))
corpus3 <- tm_map(corpus3, content_transformer(stripWhitespace))
corpus3 <- tm_map(corpus3, content_transformer(tolower))
corpus3.dtm<-DocumentTermMatrix(corpus3, control=list(wordLengths=c(1,Inf)))
corpus3.matrix<-as.matrix(corpus3.dtm, stringsAsFactors=F) 
#divide vuln not vuln
a<-read.csv("WandaColeman_Vulnerability_Table.csv")
#find all poems that fall below the confidence interval for vulnerability
a.vuln<-a[a$diff.low > 0,]
a.vuln<-a.vuln[complete.cases(a.vuln),]
a.not<-a[a$diff.low < 0,]
a.not<-a.not[complete.cases(a.not),]
#subset matrix by vuln and not vuln
vuln.hyp<-corpus3.matrix[row.names(corpus3.matrix) %in% as.character(a.vuln$poems),]
not.hyp<-corpus3.matrix[row.names(corpus3.matrix) %in% as.character(a.not$poems),]
#for vuln poems
vuln.ratio.actual<-unname(colSums(vuln.hyp)[which(colnames(vuln.hyp) == physical.word)])/unname(colSums(vuln.hyp)[which(colnames(vuln.hyp) == abstraction.word)])
#for non-vuln
not.ratio.actual<-unname(colSums(not.hyp)[which(colnames(not.hyp) == physical.word)]/colSums(not.hyp)[which(colnames(not.hyp) == abstraction.word)])
#ration of vuln to not vuln
#the higher the ratio the more the late period increases its use of things rather than abstractions
#it does not mean that there are absolutely more things (its possible to use more things relatively while still using fewer things overall)
vuln.not.ratio<-vuln.ratio.actual/not.ratio.actual

#randomization test
pred.v<-vector()
for (i in 1:1000){
  #permute the entire matrix
  permute.m<-corpus3.matrix[sample(nrow(corpus3.matrix), nrow(corpus3.matrix)),]
  #subset it to the same sizes as the vulnerable and non-vulnerable poems
  vuln.m<-permute.m[1:nrow(vuln.hyp),]
  not.m<-permute.m[(nrow(vuln.hyp)+1):nrow(permute.m),]
  #calculate the difference in the ratios
  vuln.ratio.pred<-colSums(vuln.m)[which(colnames(vuln.m) == physical.word)][[1]]/colSums(vuln.m)[which(colnames(vuln.m) == abstraction.word)][[1]]
  not.ratio.pred<-colSums(not.m)[which(colnames(not.m) == physical.word)][[1]]/colSums(not.m)[which(colnames(not.m) == abstraction.word)][[1]]
  test.statistic<-vuln.ratio.pred/not.ratio.pred
  pred.v<-append(pred.v, vuln.ratio.pred)
}
#actual
vuln.not.ratio
#predicted
max(pred.v)
quantile(pred.v, c(0.99))

#6.20
#Generality - Coleman
corpus1 <- VCorpus(DirSource("WandaColeman"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
#load hypernyms
corpus2 <- VCorpus(DirSource("WandaColeman_HYP"), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, content_transformer(function(x) gsub("_", " ", x)))
corpus2.dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
corpus2.matrix<-as.matrix(corpus2.dtm, stringsAsFactors=F) 

#subset by vuln and not vuln
a<-read.csv("WandaColeman_Vulnerability_Table.csv")
#find all poems that fall below the confidence interval for vulnerability
a.vuln<-a[a$diff.low > 0,]
a.vuln<-a.vuln[complete.cases(a.vuln),]
a.not<-a[a$diff.low < 0,]
a.not<-a.not[complete.cases(a.not),]
#subset matrix by vuln and not vuln
vuln.hyp<-corpus2.matrix[row.names(corpus2.matrix) %in% as.character(a.vuln$poems),]
not.hyp<-corpus2.matrix[row.names(corpus2.matrix) %in% as.character(a.not$poems),]
vuln.reg<-corpus1.matrix[row.names(corpus1.matrix) %in% as.character(a.vuln$poems),]
not.reg<-corpus1.matrix[row.names(corpus1.matrix) %in% as.character(a.not$poems),]
#test statistic
#what % of vuln words are not-vuln hypernyms 
vuln.hypernymy<-sum(vuln.reg[,which(colnames(vuln.reg) %in% colnames(not.hyp))])/sum(vuln.reg)
#what % of not vuln words are hypernyms of vuln words
not.hypernymy<-sum(not.reg[,which(colnames(not.reg) %in% colnames(vuln.hyp))])/sum(not.reg)
#actual
actual<-vuln.hypernymy-not.hypernymy

#randomization test
pred.v<-vector()
for (j in 1:1000){
  print(j)
  #permute original matrices
  corpus1.perm<-corpus1.matrix[sample(nrow(corpus1.matrix), nrow(corpus1.matrix)),]
  corpus2.perm<-corpus2.matrix[sample(nrow(corpus2.matrix), nrow(corpus2.matrix)),]
  #subset hyp an
  vuln.hyp<-corpus2.perm[1:nrow(a.vuln),]
  not.hyp<-corpus2.perm[(nrow(a.vuln)+1):nrow(corpus2.perm),]
  vuln.reg<-corpus1.perm[1:nrow(a.vuln),]
  not.reg<-corpus1.perm[(nrow(a.vuln)+1):nrow(corpus2.perm),]
  #test statistic
  #what % of vuln words are not-vuln hypernyms 
  vuln.hypernymy<-sum(vuln.reg[,which(colnames(vuln.reg) %in% colnames(not.hyp))])/sum(vuln.reg)
  #what % of not vuln words are hypernyms of vuln words
  not.hypernymy<-sum(not.reg[,which(colnames(not.reg) %in% colnames(vuln.hyp))])/sum(not.reg)
  #actual
  pred<-vuln.hypernymy-not.hypernymy
  pred.v<-append(pred.v, pred)
}
#actual
vuln.hypernymy-not.hypernymy
#predicted
max(pred.v)
quantile(pred.v, c(0.99))
min(pred.v)



