library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(readxl)
library(sentimentr)
msnbc<-read_excel("list_mode_export_msnbc_final.xlsx")

msnbc_title <-as.character(msnbc$Title)
msnbc_title <- as.character(msnbc$ï..Title)
msnbc_title <- tibble(line= 1:2759, text=msnbc_title)
msnbc_title <- as.data.frame(msnbc_title)

data("stop_words")

msnbc_words <- msnbc_title %>%
  unnest_tokens(word, text)

msnbc_words <- msnbc_words%>%
  anti_join(stop_words)

msnbc_words %>%
  count(word, sort=TRUE)
#dropping msnbc
msnbc_words<-msnbc_words[!(msnbc_words$word=="msnbc"),]


msnbc_words %>%
  count(word, sort=TRUE)%>%
  filter(n>100)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n),hjust=-0.3)


msnbc_bigrams <- msnbc_title%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

unnecessary <- read.csv("unnes.csv") #MSNBC

msnbc_bigrams %>%
  count(bigram, sort=TRUE)%>%
  separate(bigram, c("word1", "word2"), sep= " ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word1 %in% unnecessary$ï..word)%>%
  filter(!word2 %in% unnecessary$ï..word)%>%
  unite(bigram, word1,word2, sep = " ")%>%
  filter(n>50)%>%
  mutate(word=reorder(bigram,n))%>%
  ggplot(aes(x=bigram, y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.3)



msnbc_title_TRUMP <- msnbc_title[(grepl("Trump",msnbc_title$text)),]

sent_Trump_msnbc <-sentiment_by(msnbc_title_TRUMP$text)
mean(sent_Trump_msnbc$ave_sentiment)

msnbc_title_TRUMP$text%>%
  extract_sentiment_terms()


msnbc_Trump_bigrams <-msnbc_title_TRUMP%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

msnbc_Trump_bigrams2 <- msnbc_Trump_bigrams[(grepl("trump",msnbc_Trump_bigrams$bigram)),]

msnbc_Trump_bigrams2 %>%
  count(bigram, sort=TRUE)%>%
  separate(bigram, c("word1", "word2"), sep= " ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word1 %in% unnecessary$ï..word)%>%
  filter(!word2 %in% unnecessary$ï..word)%>%
  unite(bigram, word1,word2, sep = " ")%>%
  filter(n>3)%>%
  mutate(word=reorder(bigram,n))%>%
  ggplot(aes(x=word, y=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.3)


#################################################################
#fox
fox <- read.csv("list_mode_export_fox.csv")
fox_title <- as.character(fox$Title)
fox_title <- tibble(line= 1:1764, text=fox_title)
fox_title <- as.data.frame(fox_title)


fox_words <- fox_title %>%
  unnest_tokens(word, text)

fox_words <- fox_words%>%
  anti_join(stop_words)

fox_words %>%
  count(word, sort=TRUE)
#dropping fox
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
