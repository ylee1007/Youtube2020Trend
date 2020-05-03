library(readxl)
# libraries for kmeans 
library(tm)
library(proxy)
library(dplyr)
library(wordcloud)

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



# https://ethen8181.github.io/machine-learning/clustering_old/tf_idf/tf_idf.html

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
# 3. calculate pair-wise distance matrix 
fox_d1 <- dist(fox_tf_idf, method = "cosine")

# 4. heirachical clustering 
fox_cluster1 <- hclust(fox_d1, method = "ward.D")


# split into 10 clusters
fox_groups1 <- cutree(fox_cluster1, 5)

fox.cluster.1 <- fox_title$text[fox_groups1 == 1 ]
#length(fox.cluster.1)
fox.cluster.2 <- fox_title$text[fox_groups1 == 2 ]
fox.cluster.3 <- fox_title$text[fox_groups1 == 3 ]
fox.cluster.4 <- fox_title$text[fox_groups1 == 4 ]
fox.cluster.5 <- fox_title$text[fox_groups1 == 5 ]
# length of each cluster
cluster.length <- c(length(fox.cluster.1), length(fox.cluster.2), length(fox.cluster.3), length(fox.cluster.4), length(fox.cluster.5))

# Graphs

wordcloud(fox.cluster.1, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.2, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.3, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.4, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.5, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


# cluster 1

c1 <- fox.cluster.1
c1_title <- as.character(c1)
c1_title <- tibble(line= 1:cluster.length[1], text=c1)
c1_title <- as.data.frame(c1_title)

###
library(sentimentr)
c1_title_TRUMP <- c1_title[(grepl("Trump",c1_title$text)),]

sent_Trump_c1 <-sentiment_by(c1_title_TRUMP$text)
mean(sent_Trump_c1$ave_sentiment)

c1_title_TRUMP$text%>%
  extract_sentiment_terms()

library(tidytext)
c1_Trump_bigrams <- c1_title_TRUMP%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

c1_Trump_bigrams2 <- c1_Trump_bigrams[(grepl("trump",c1_Trump_bigrams$bigram)),]

c1_Trump_bigrams2 %>%
  count(bigram, sort=TRUE)%>%
  separate(bigram, c("word1", "word2"), sep= " ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  unite(bigram, word1,word2, sep = " ")%>%
  filter(n>2)%>%
  mutate(word=reorder(bigram,n))%>%
  ggplot(aes(x=word, y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.3)



