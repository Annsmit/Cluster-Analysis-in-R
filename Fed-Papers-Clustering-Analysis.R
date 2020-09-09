       
#########################################################
## Andrea Smith Homework #4
#########################################################

library(tm)
#install.packages("tm")
library(stringr)
library(wordcloud)
install.packages("Snowball")
#library("Snowball")
#install.packages("slam")
library(slam)
library(quanteda)
#install.packages("quanteda")
library(SnowballC)
library(arules)
#install.packages('proxy')
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
#install.packages("factoextra")
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering

#Make sure working directory is set correctly
getwd()

## Next, load in the documents (the corpus)
FedCorpus <- Corpus(DirSource("Fed_Corpus"))
#Line of code to confirm how many Federalist Papers were loaded into R
(ndocs<-length(FedCorpus))
##The following will show you that you read in all the documents
(summary(FedCorpus))
#Apply English stop words 
(STOPS <-stopwords('english'))
#My stopwords
MyStopWords <- c("the", "also", "and", "I", "their")
getTransformations()

#Clean and prepare the text documents
CleanCorpus <- tm_map(FedCorpus, removePunctuation)
CleanCorpus <- tm_map(CleanCorpus, removeNumbers)
CleanCorpus <- tm_map(CleanCorpus, removeWords, STOPS)
CleanCorpus <- tm_map(CleanCorpus, removeWords, MyStopWords)
CleanCorpus <- tm_map(CleanCorpus, content_transformer(tolower))
CleanCorpus <- tm_map(CleanCorpus, stripWhitespace)

inspect(CleanCorpus)

#convert to matrix and normalize it
Fed_dtm <- DocumentTermMatrix(CleanCorpus, control = list(weighting = weightTfIdf))
                                #bounds = list(global = c(minTermFreq, maxTermFreq))))
Fed_mat <- as.matrix(Fed_dtm)
inspect(Fed_dtm)

#remove sparse terms
Fed_dtm <- removeSparseTerms(Fed_dtm, 0.90)
inspect(Fed_dtm)

#Find frequent terms
findFreqTerms(Fed_dtm,1)
CleanDF <- as.data.frame(inspect(Fed_dtm))
CleanDF

#Check word frequency
(WordFreq <- colSums(as.matrix(Fed_dtm)))

#Scale the dataframe
CleanDFScale <- scale(CleanDF)
View(CleanDF)

#Distance measures
e <- dist(CleanDFScale, method = "euclidean")
c <- dist(CleanDF, method="cosine")

##HAC
fite <- hclust(e, method ="ward.D2")
fitc <- hclust(c, method = "ward.D2")
plot(fite, main = "Euclidean")
plot(fitc, main = "Cosine")

#HAC Clusters rectangled
groups_C <- hclust(c,method="ward.D")
plot(groups_C, cex=.9, hang=-1)
rect.hclust(groups_C, k=4)

#Distance for kmeans
m <- get_dist(Fed_mat,method = "manhattan")
s <- get_dist(Fed_mat,method = "minkowski")

#heatmap
fviz_dist(m, gradient = list(low = "red", mid = "white", high = "blue"))
fviz_dist(s, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Scale the data
M <- t(Fed_mat)
MScale <- scale(M)
str(MScale)

#(kmeansFIT1)
set.seed(123)
k <- 4
kmeansFIT_1 <- kmeans(M, k)
kmeansFIT_1
summary(kmeansFIT_1)
(kmeansFIT_1$cluster)
kmeansFIT_1$centers

#print the cluster assignments
#cluster_assignment <- data.frame(Fed_mat,kmeansFIT_1$cluster)
#write.csv(cluster_assignment,"cluster_assignment.csv")
#View(cluster_assignment)

fviz_cluster(kmeansFIT_1, show_labels = TRUE, data = M, 
             label =8, palette = "Set2", ellipse.type = "convex", 
             main = "Kmeans")

################# Expectation Maximization ---------
## When Clustering, there are many options. 
## I cannot run this as it requires more than 18 Gigs...

ClusFI <- Mclust(M,G=6)
(ClusFI)
summary(ClusFI)
plot(ClusFI, what = "density", main ="")

## Convert to matrix and view
Fed_dtm_matrix = as.matrix(Fed_dtm)
str(Fed_mat)
(Fed_mat[c(1:85),c(2:4)])

#Word Cloud
wordcloud(colnames(Fed_mat), Fed_mat[13, ], max.words = 70)
(head(sort(as.matrix(Fed_mat)[13,], decreasing = TRUE), n=20))
