library(tm)
#library(sentimentr)
#library(stringr)
library(tidytext)
library(qdap)

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



#msnbc_title <-as.character(msnbc$Title)
#msnbc_title <- tibble(line= 1:2759, text=msnbc_title)
#msnbc_title <- as.data.frame(msnbc_title)
#data("stop_words")
#msnbc_words <- msnbc_title %>%
#  unnest_tokens(word, text)

#msnbc_words <- msnbc_words%>%
#  anti_join(stop_words)

#msnbc_words %>%
#  count(word, sort=TRUE)

#build corpus
#corpus <- Corpus(VectorSource(title))
corpus <- VCorpus(VectorSource(c(doc$TitleRemoved)))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))

#create term document matrix
tdm <- TermDocumentMatrix(corpus, control = list(minWordLength=c(1,Inf)))
t <- removeSparseTerms(tdm, sparse = 0.98)
m <- as.matrix(t)
#m3<-as.matrix(trainTT_dfm)
#plot frequency terms
freq <- rowSums(m)
#freq <- rowSums(trainTT_dfm, )
#freq <- subset
barplot(freq, las = 2, col = rainbow(25))

#Heirarchical word/title clustering using dendrogram
distance <- dist(scale(m))
print(distance, digits = 2)
hc <- hclust(distance, method="ward.D")
# plot(hc, hang=-1)
plot(hc, hang=-1)
rect.hclust(hc, k=5)

#nonhirachical k-mean clustering of title word
m1 <- t(m)
set.seed(222)
k <-5
kc <- kmeans(m1, k)
