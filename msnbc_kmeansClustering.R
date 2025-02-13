
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(readxl)
library(sentimentr)
library(rlang)

library('cluster')    # clustering algorithms
library('factoextra') # clustering algorithms & visualization
library('quanteda')
library(tm)
#library(sentimentr)
#library(stringr)
library(tidytext)
library(qdap)




# MSNBC


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






msnbc_words <- msnbc_title %>%
  unnest_tokens(word, text)

msnbc_words <- msnbc_words%>%
  anti_join(stop_words)

msnbc_words %>%
  count(word, sort=TRUE)


msnbc_words %>%
  count(word, sort=TRUE)%>%
  filter(n>50)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n),hjust=-0.3)

msnbc_bigrams <- msnbc_title%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

msnbc_bigrams %>%
  count(bigram, sort=TRUE)%>%
  separate(bigram, c("word1", "word2"), sep= " ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  unite(bigram, word1,word2, sep = " ")%>%
  filter(n>15)%>%
  mutate(word=reorder(bigram,n))%>%
  ggplot(aes(x=word, y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.3)






# environment 
library(tm)
library(proxy)
library(dplyr)



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
#Cosine <- function(x, y) {
#  similarity <- sum(x * y) / ( sqrt( sum(y ^ 2) ) * sqrt( sum(x ^ 2) ) )
  
  # given the cosine value, use acos to convert back to degrees
  # acos returns the radian, multiply it by 180 and divide by pi to obtain degrees
#  return( acos(similarity) * 180 / pi )
#}
# pr_DB$delete_entry("Cosine")

# 3. calculate pair-wise distance matrix 
# pr_DB$set_entry( FUN = Cosine , names = c("Cosine"))
# d1 <- dist(msnbc_tf_idf, method = "Cosine")
# pr_DB$delete_entry("Cosine")

d1 <- dist( msnbc_tf_idf, method = "cosine" )

# 4. heirachical clustering 
msnbc.cluster1 <- hclust(d1, method = "ward.D")
plot(msnbc.cluster1)
rect.hclust(msnbc.cluster1, 5)


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



library(wordcloud)

wordcloud(msnbc.cluster.1, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(msnbc.cluster.2, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(msnbc.cluster.3, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(msnbc.cluster.4, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(msnbc.cluster.5, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

