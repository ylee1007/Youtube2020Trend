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

msnbc <- read_excel("msnbc_data.xlsx")
docMsnbc <-  msnbc[,5]
docMsnbc$TitleRemoved <-gsub("[[:punct:]]", "", docMsnbc$TitleRemoved)
docMsnbc$TitleRemoved <-gsub("will", "", docMsnbc$TitleRemoved, ignore.case = TRUE)
docMsnbc$TitleRemoved <-gsub("[^0-9A-Za-z///' ]","", docMsnbc$TitleRemoved,ignore.case = TRUE)
docMsnbc$TitleRemoved <-gsub("Trumps", "Trump", docMsnbc$TitleRemoved, ignore.case = TRUE)

msnbc_title <- as.character(docMsnbc$TitleRemoved)
msnbc_title <- tibble(line= 1:2759, text=msnbc_title)
msnbc_title <- as.data.frame(msnbc_title)


msnbc_words <- msnbc_title %>%
  unnest_tokens(word, text)

msnbc_words <- msnbc_words%>%
  anti_join(stop_words)

msnbc_words %>%
  count(word, sort=TRUE)
#dropping fox
#fox_words<-fox_words[!(msnbc_words$word=="msnbc"),]

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

msnbc_title_TRUMP <- msnbc_title[(grepl("Trump",msnbc_title$text)),]

sent_Trump_msnbc <-sentiment_by(msnbc_title_TRUMP$text)
mean(sent_Trump_msnbc$ave_sentiment)

msnbc_title_TRUMP$text%>%
  extract_sentiment_terms()


msnbc_Trump_bigrams <- msnbc_title_TRUMP%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

msnbc_Trump_bigrams2 <- msnbc_Trump_bigrams[(grepl("trump",msnbc_Trump_bigrams$bigram)),]

msnbc_Trump_bigrams2 %>%
  count(bigram, sort=TRUE)%>%
  separate(bigram, c("word1", "word2"), sep= " ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  unite(bigram, word1,word2, sep = " ")%>%
  filter(n>3)%>%
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

#MsnbcData <- msnbc$"Meta Keyword 1"
MsnbcData <- docMsnbc$TitleRemoved


MsnbcData <- Corpus(VectorSource(MsnbcData)) 
MsnbcData <- tm_map(MsnbcData, removePunctuation)
MsnbcData <- tm_map(MsnbcData, removeWords, stopwords("english"))
MsnbcData <- tm_map(MsnbcData, content_transformer(tolower))
# MsnbcData <- tm_map(MsnbcData, removeNumbers)

# apply tf-idf
myTdm_msnbc <- TermDocumentMatrix(MsnbcData, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))

# make it as matrix
myMsnbcData <- t((as.matrix(myTdm_msnbc)))

# find optimal k for kmeans

#data <- dist(myFoxData, method = "cosine")
library(slam)
cosine_dist_mat_m <- 1 - crossprod_simple_triplet_matrix(myTdm_msnbc)/(sqrt(col_sums(myTdm_msnbc^2) %*% t(col_sums(myTdm_msnbc^2))))
# https://stackoverflow.com/questions/29750519/r-calculate-cosine-distance-from-a-term-document-matrix-with-tm-and-proxy

data_m <- dist(myMsnbcData)

cost_df_m <- data.frame()

for(i in 1:15){
  # run kmeans for each level of i
  kmeans <- kmeans(x = data_m, centers = i, iter.max = 15)
  
  #combine cluster number and cost together, write to df
  cost_df_m <- rbind(cost_df_m, cbind(i, kmeans$tot.withinss))
}

names(cost_df_m) <- c("cluster", "cost")

plot(cost_df_m$cluster, cost_df_m$cost, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "Cost")



clust<-kmeans(data_m,9)
class_table<-table(clust$cluster)  

value <- list()
for(i in 1:length(class_table)){
  value[[i]] <- msnbc[which(clust$cluster==as.numeric(names(class_table)[i])),]
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

cluster1Title <- value[[1]][[5]]

wordcloud(cluster1Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster2Title <- value[[2]][[5]]

wordcloud(cluster2Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster3Title <- value[[3]][[5]]

wordcloud(cluster3Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

cluster4Title <- value[[4]][[5]]

wordcloud(cluster4Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster5Title <- value[[5]][[5]]

wordcloud(cluster5Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


cluster6Title <- value[[6]][[5]]

wordcloud(cluster6Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

cluster7Title <- value[[7]][[5]]

wordcloud(cluster7Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

cluster8Title <- value[[8]][[5]]

wordcloud(cluster8Title, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))

##############################


c1_title <-as.character(value[[6]][[1]])
c1_title <- tibble(line= 1:274, text=c1_title)
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