library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(readxl)
library(sentimentr)
library(rlang)

# MSNBC

fox <- read_excel("fox_data_removedduplicates.xlsx")
docFox <-  fox[,1]
docFox$Title <-gsub("[[:punct:]]", "", docFox$Title)
docFox$Title <-gsub("fox", "", docFox$Title, ignore.case = TRUE) #dropping fox
docFox$Title <-gsub("[^0-9A-Za-z///' ]","", docFox$Title,ignore.case = TRUE)
docFox$Title <-gsub("Trumps", "Trump", docFox$Title, ignore.case = TRUE)

fox_title <- as.character(docFox$Title)
fox_title <- tibble(line= 1:1764, text=fox_title)
fox_title <- as.data.frame(fox_title)


fox_words <- fox_title %>%
  unnest_tokens(word, text)

fox_words <- fox_words%>%
  anti_join(stop_words)

fox_words %>%
  count(word, sort=TRUE)

#fox_words<-fox_words[!(msnbc_words$word=="msnbc"),]

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

fox_title_TRUMP <- fox_title[(grepl("Trump",fox_title$text)),]

sent_Trump_fox <-sentiment_by(fox_title_TRUMP$text)
mean(sent_Trump_fox$ave_sentiment)

fox_title_TRUMP$text%>%
  extract_sentiment_terms()


fox_Trump_bigrams <- fox_title_TRUMP%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

fox_Trump_bigrams2 <- fox_Trump_bigrams[(grepl("trump",fox_Trump_bigrams$bigram)),]

fox_Trump_bigrams2 %>%
  count(bigram, sort=TRUE)%>%
  separate(bigram, c("word1", "word2"), sep= " ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  unite(bigram, word1,word2, sep = " ")%>%
  filter(n>5)%>%
  mutate(word=reorder(bigram,n))%>%
  ggplot(aes(x=word, y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.3)








############ kmeans #########################

library('cluster')    # clustering algorithms
library('factoextra') # clustering algorithms & visualization
library('quanteda')

length(fox_words$word)
tempFox <- dfm(fox_words$word, tolower= TRUE)

k <- kmeans(x = tempFox, centers = 4)

fviz_cluster(k, data = tempFox)



# run kmeans for all clusters up to 15

cost_df <- data.frame()

for(i in 1:15){
  # run kmeans for each level of i
  kmeans <- kmeans(x = tempFox, centers = i, iter.max = 15)
  
  #combine cluster number and cost together, write to df
  cost_df <- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}

names(cost_df) <- c("cluster", "cost")

plot(cost_df$cluster, cost_df$cost, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "Cost")





############################

# https://blog.naver.com/seodaeho91/221216925399


library(data.table)

library(rJava)
# library(KoNLP)
library(arules)
library(igraph)
library(combinat)
library(tm)
#library(GMD)
library(proxy)

#foxData <- fox$"Title 1"
foxData <- docFox$Title

foxData <- Corpus(VectorSource(foxData)) 
foxData <- tm_map(foxData, removePunctuation)

# foxData <- tm_map(foxData, removeNumbers)

foxData <- tm_map(foxData, tolower)

# apply tf-idf
myTdm <- TermDocumentMatrix(foxData, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))

# make it as matrix
myFoxData <- t((as.matrix(myTdm)))

# find optimal k for kmeans

#data <- dist(myFoxData, method = "cosine")
library(slam)
cosine_dist_mat <- 1 - crossprod_simple_triplet_matrix(myTdm)/(sqrt(col_sums(myTdm^2) %*% t(col_sums(myTdm^2))))
# https://stackoverflow.com/questions/29750519/r-calculate-cosine-distance-from-a-term-document-matrix-with-tm-and-proxy

data <- dist(myFoxData)

cost_df <- data.frame()

for(i in 1:15){
  # run kmeans for each level of i
  kmeans <- kmeans(x = data, centers = i, iter.max = 15)
  
  #combine cluster number and cost together, write to df
  cost_df <- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}

names(cost_df) <- c("cluster", "cost")

plot(cost_df$cluster, cost_df$cost, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "Cost")



clust<-kmeans(data,7)
class_table<-table(clust$cluster)  

value <- list()
for(i in 1:length(class_table)){
  value[[i]] <- fox[which(clust$cluster==as.numeric(names(class_table)[i])),]
}  



### visualize clusters ###


library(SnowballC)
library(wordcloud)
library(RColorBrewer)

cluster1Contens <- value[[1]][[15]]

wordcloud(cluster1Contens, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster2Contens <- value[[2]][[15]]

wordcloud(cluster2Contens, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster3Contens <- value[[3]][[15]]

wordcloud(cluster3Contens, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

cluster4Contens <- value[[4]][[15]]

wordcloud(cluster4Contens, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster5Contens <- value[[5]][[15]]

wordcloud(cluster5Contens, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster6Contens <- value[[6]][[15]]

wordcloud(cluster6Contens, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


######## title

cluster1Title <- value[[1]][[1]]

wordcloud(cluster1Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster2Title <- value[[2]][[1]]

wordcloud(cluster2Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster3Title <- value[[3]][[1]]

wordcloud(cluster3Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

cluster4Title <- value[[4]][[1]]

wordcloud(cluster4Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster5Title <- value[[5]][[1]]

wordcloud(cluster5Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster6Title <- value[[6]][[1]]

wordcloud(cluster6Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

cluster7Title <- value[[7]][[1]]

wordcloud(cluster7Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

#cluster8Title <- value[[8]][[1]]

#wordcloud(cluster8Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

##############################


c1_title <-as.character(value[[6]][[1]])
c1_title <- tibble(line= 1:193, text=c1_title)
c1_title <- as.data.frame(c1_title)

data("stop_words")

c1_words <- c1_title %>%
  unnest_tokens(word, text)

c1_words <- c1_words%>%
  anti_join(stop_words)

c1_words %>%
  count(word, sort=TRUE)
# dropping msnbc
# msnbc_words<-msnbc_words[!(msnbc_words$word=="msnbc"),]


c1_words %>%
  count(word, sort=TRUE)%>%
  filter(n>10)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n),hjust=-0.3)

#############################

c1_view <- mean(value[[1]][["View"]])
c2_view <- mean(value[[2]][["View"]])
c3_view <- mean(value[[3]][["View"]])
c4_view <- mean(value[[4]][["View"]])
c5_view <- mean(value[[5]][["View"]])
c6_view <- mean(value[[6]][["View"]])

c_viewTable <- as.table(c(c1_view,c2_view,c3_view,c4_view,c5_view,c6_view))
names(c_viewTable) <- c("c1","c2","c3","c4","c5","c6")
plot(c_viewTable)






