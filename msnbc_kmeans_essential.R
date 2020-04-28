
#library(stringr)
#library(tidytext)
#library(ggplot2)
#library(tidyverse)
#library(tidyr)
#library(sentimentr)
#library(rlang)

#library(tidytext)
#library(qdap)


library(readxl)


# libraries for kmeans 
library(tm)
library(proxy)
library(dplyr)

library(wordcloud)


# MSNBC data

msnbcData<-read_excel("msnbc_data.xlsx")
doc <-  msnbcData[,5]
doc$TitleRemoved <-gsub("[[:punct:]]", "", doc$TitleRemoved)
doc$TitleRemoved <-gsub("Will", "", doc$TitleRemoved)
doc$TitleRemoved <-gsub("[^0-9A-Za-z///' ]","", doc$TitleRemoved,ignore.case = TRUE)
doc$TitleRemoved <-gsub("Trumps", "Trump", doc$TitleRemoved, ignore.case = TRUE)
msnbcDf <- as.data.frame(msnbcData)
msnbc <- msnbcDf %>% select("TitleRemoved")
#msnbc$Title <- as.character(msnbc$TitleRemoved)
#title<-msnbc$Title
title2<-as.vector(msnbc)

msnbc_title <- tibble(line = 1:2759, text = title2$TitleRemoved)
msnbc_title <- as.data.frame(msnbc_title)


# https://ethen8181.github.io/machine-learning/clustering_old/tf_idf/tf_idf.html


# 
# 1. [TFIDF] :
# @vector = pass in a vector of documents  
TFIDF <- function(vector) {
  # tf 
  news_corpus  <- Corpus( VectorSource(vector) )
  control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
  tf <- TermDocumentMatrix(news_corpus, control = control_list) %>% as.matrix()
  
  # idf
  idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) %>% diag()
  return( crossprod(tf, idf) )
}

# tf-idf matrix using news' title 
msnbc_tf_idf <- TFIDF(msnbc_title$text)

# 2. [Cosine] :
# distance between two vectors
# 3. calculate pair-wise distance matrix 
msnbc_d1 <- dist( msnbc_tf_idf, method = "cosine" )

# 4. heirachical clustering 
msnbc.cluster1 <- hclust(msnbc_d1, method = "ward.D")

# split into 10 clusters
msnbc.groups1 <- cutree(msnbc.cluster1, 5)

# you can look at the distribution size of each cluster 
# table(groups1)
#msnbc
msnbc.cluster.1 <- msnbc_title$text[msnbc.groups1 == 1 ]
#length(fox.cluster.1)
msnbc.cluster.2 <- msnbc_title$text[msnbc.groups1 == 2 ]
msnbc.cluster.3 <- msnbc_title$text[msnbc.groups1 == 3 ]
msnbc.cluster.4 <- msnbc_title$text[msnbc.groups1 == 4 ]
msnbc.cluster.5 <- msnbc_title$text[msnbc.groups1 == 5 ]




wordcloud(msnbc.cluster.1, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(msnbc.cluster.2, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(msnbc.cluster.3, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(msnbc.cluster.4, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(msnbc.cluster.5, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

