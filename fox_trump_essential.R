library(readxl)
library(dplyr)
library(sentimentr)
library(tidytext)
library(tidyverse)
library(ggplot2)


######################################################################################

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

######################################################################################

fox_title_TRUMP <- fox_title[(grepl("Trump",fox_title$text)),]

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
