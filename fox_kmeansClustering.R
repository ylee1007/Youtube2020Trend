


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


# FOX

# Get title from excel


fox <- read_excel("fox_data_removedduplicates.xlsx")
docFox <-  fox[,1]
docFox$Title <-gsub("[[:punct:]]", "", docFox$Title)
docFox$Title <-gsub("fox", "", docFox$Title, ignore.case = TRUE) #dropping fox
docFox$Title <-gsub("[^0-9A-Za-z///' ]","", docFox$Title,ignore.case = TRUE)
docFox$Title <-gsub("Trumps", "Trump", docFox$Title, ignore.case = TRUE)


fox_title <- as.character(docFox$Title)
fox_title <- tibble(line= 1:1764, text=fox_title)
fox_title <- as.data.frame(fox_title)



# word frequency


fox_words <- fox_title %>%
  unnest_tokens(word, text)

fox_words <- fox_words%>%
  anti_join(stop_words)

fox_words %>%
  count(word, sort=TRUE)


fox_words %>%
  count(word, sort=TRUE)%>%
  filter(n>50)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n),hjust=-0.3)

fox_bigrams <- fox_title%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

fox_bigrams %>%
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


# tf-idf + kmeans


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
fox_tf_idf <- TFIDF(fox_title$text)

# 2. [Cosine] :
# distance between two vectors
#Cosine <- function(x, y) {
#  similarity <- sum(x * y) / ( sqrt( sum(y ^ 2) ) * sqrt( sum(x ^ 2) ) )
  
  # given the cosine value, use acos to convert back to degrees
  # acos returns the radian, multiply it by 180 and divide by pi to obtain degrees
#  return( acos(similarity) * 180 / pi )
#}

# 3. calculate pair-wise distance matrix 
#pr_DB$set_entry( FUN = Cosine , names = c("Cosine"))
#d1 <- dist(fox_tf_idf, method = "Cosine")
#pr_DB$delete_entry("Cos")
d1 <- dist(fox_tf_idf, method = "cosine")

# 4. heirachical clustering 
cluster1 <- hclust(d1, method = "ward.D")
plot(cluster1)
rect.hclust(cluster1, 5)


# split into 10 clusters
groups1 <- cutree(cluster1, 5)

# you can look at the distribution size of each cluster 
# table(groups1)

fox.cluster.1 <- fox_title$text[groups1 == 1 ]
#length(fox.cluster.1)
fox.cluster.2 <- fox_title$text[groups1 == 2 ]
fox.cluster.3 <- fox_title$text[groups1 == 3 ]
fox.cluster.4 <- fox_title$text[groups1 == 4 ]
fox.cluster.5 <- fox_title$text[groups1 == 5 ]


# Tried to cluster the first group -> failed :)



# tf-idf matrix using news' title 
#fox_tf_idf.c1 <- TFIDF(fox.cluster.1)


# 3. calculate pair-wise distance matrix 
#pr_DB$set_entry( FUN = Cosine , names = c("Cosine"))
#d1.c1 <- dist(fox_tf_idf.c1, method = "Cosine")
#pr_DB$delete_entry("Cosine")

# 4. heirachical clustering 
#cluster1.c1 <- hclust(d1, method = "ward.D")
#plot(cluster1.c1)
#rect.hclust(cluster1.c1, 5)


# split into 10 clusters
#groups1.c1 <- cutree(cluster1.c1, 5)


#length(fox_title$text[groups1.c1 == 2 ])
# 1566



# Graphs

library(wordcloud)

wordcloud(fox.cluster.1, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.2, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.3, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.4, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.5, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

