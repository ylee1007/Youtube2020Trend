library(readxl)
# libraries for kmeans 
library(tm)
library(proxy)
library(dplyr)
library(wordcloud)
library(tidyr)
library(ggplot2)
library(tidytext)
library(plotrix)

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
fox.cluster.length <- c(length(fox.cluster.1), length(fox.cluster.2), length(fox.cluster.3), length(fox.cluster.4), length(fox.cluster.5))
fox.cluster <- list(fox.cluster.1, fox.cluster.2, fox.cluster.3, fox.cluster.4, fox.cluster.5)
# Graphs

wordcloud(fox.cluster.1, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.2, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.3, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.4, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
wordcloud(fox.cluster.5, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


################3


####################### only cluster 2 and 3 includes Trump #######################

for(i in 1:5){
  fox.c <- fox.cluster[[i]]
  fox.c_title <- as.character(fox.c)
  fox.c_title <- tibble(line= 1:fox.cluster.length[i], text=fox.c)
  fox.c_title <- as.data.frame(fox.c_title)
  
  c.fox_title_TRUMP <- fox.c_title[(grepl("Trump",fox.c_title$text)),]

  if(nrow(c.fox_title_TRUMP) > 4) {
    c.fox_Trump_bigrams <-c.fox_title_TRUMP%>%
      unnest_tokens(bigram, text, token="ngrams", n=2)
    
    c.fox_Trump_bigrams2 <- c.fox_Trump_bigrams[(grepl("trump",c.fox_Trump_bigrams$bigram)),]
    
    plot <- (c.fox_Trump_bigrams2 %>%
               count(bigram, sort=TRUE)%>%
               separate(bigram, c("word1", "word2"), sep= " ")%>%
               filter(!word1 %in% stop_words$word)%>%
               filter(!word2 %in% stop_words$word)%>%
               unite(bigram, word1,word2, sep = " ")%>%
               filter(n>4 & n<22)%>%
               mutate(word=reorder(bigram,n))%>%
               ggplot(aes(x=word, y=n))+
               geom_col()+
               xlab(NULL)+
               coord_flip()+
               geom_text(aes(label=n), hjust=-0.3))
    print(plot)
  } else {
    plot.new()
    textbox(c(0,1), 1, textlist = "no 'Trump' word in the title",  border="red")
    
  }
  
}

