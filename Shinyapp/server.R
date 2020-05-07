library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(tidytext)

# libraries for kmeans 
library(tm)
library(proxy)
#library(dplyr)
library(wordcloud)

library(glmnet)
library(caret)

######################################### cluster analyze by month#######################################
fox <- read_excel("../fox_data_removedduplicates.xlsx")
fox <- fox%>%
  select("Title", "Date")
Apr_fox <-fox%>%
  filter(str_detect(Date, "2020. 4."))
Mar_fox <- fox%>%
  filter(str_detect(Date, "2020. 3. "))
Feb_fox <- fox%>%
  filter(str_detect(Date, "2020. 2. "))
Jan_fox <- fox%>%
  filter(str_detect(Date, "2020. 1. "))
Jan_fox_title <-as.character(Jan_fox$Title)
Jan_fox_title <- tibble(line= 1:460, text=Jan_fox_title)
data("stop_words")

Jan_fox_words <- Jan_fox_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Jan_fox_words$word <-gsub("[^0-9A-Za-z///' ]","" , Jan_fox_words$word ,ignore.case = TRUE)
Jan_fox_words$word <-gsub("[[:punct:]]", "", Jan_fox_words$word )
Jan_fox_words$word<-gsub("fox", "", Jan_fox_words$word)
Jan_fox_words$word<-gsub("trumps", "trump", Jan_fox_words$word, ignore.case=TRUE)
Jan_fox_words$word<-removeNumbers(Jan_fox_words$word, ucp = FALSE)

Feb_fox_title <-as.character(Feb_fox$Title)
Feb_fox_title <- tibble(line= 1:468, text=Feb_fox_title)
Feb_fox_words <- Feb_fox_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Feb_fox_words$word <-gsub("[^0-9A-Za-z///' ]","" , Feb_fox_words$word ,ignore.case = TRUE)
Feb_fox_words$word <-gsub("[[:punct:]]", "", Feb_fox_words$word )
Feb_fox_words$word<-gsub("fox", "", Feb_fox_words$word)
Feb_fox_words$word<-gsub("trumps", "trump", Feb_fox_words$word, ignore.case=TRUE)
Feb_fox_words$word<-removeNumbers(Feb_fox_words$word, ucp = FALSE)

Mar_fox_title <-as.character(Mar_fox$Title)
Mar_fox_title <- tibble(line= 1:582, text=Mar_fox_title)
Mar_fox_words <- Mar_fox_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Mar_fox_words$word <-gsub("[^0-9A-Za-z///' ]","" , Mar_fox_words$word ,ignore.case = TRUE)
Mar_fox_words$word <-gsub("[[:punct:]]", "", Mar_fox_words$word )
Mar_fox_words$word<-gsub("fox", "", Mar_fox_words$word)
Mar_fox_words$word<-gsub("trumps", "trump", Mar_fox_words$word, ignore.case=TRUE)
Mar_fox_words$word<-removeNumbers(Mar_fox_words$word, ucp = FALSE)

Apr_fox_title <-as.character(Apr_fox$Title)
Apr_fox_title <- tibble(line= 1:254, text=Apr_fox_title)
Apr_fox_words <- Apr_fox_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Apr_fox_words$word <-gsub("[^0-9A-Za-z///' ]","" , Apr_fox_words$word ,ignore.case = TRUE)
#Apr_fox_words$word <-gsub("[0-9]+", "", Apr_fox_words$word)
Apr_fox_words$word<-removeNumbers(Apr_fox_words$word, ucp = FALSE)
Apr_fox_words$word <-gsub("[[:punct:]]", "", Apr_fox_words$word )
#Apr_fox_words$word <-gsub("[[:digit:]]+","", Apr_fox_words$word )
Apr_fox_words$word<-gsub("fox", "", Apr_fox_words$word)
Apr_fox_words$word<-gsub("trumps", "trump", Apr_fox_words$word, ignore.case=TRUE)
#Apr_fox_words$word<-gsub(" ", "", Apr_fox_words$word, fixed = TRUE)
################################################################
msnbc <- read_excel("../msnbc_data.xlsx")
msnbc <- msnbc%>%
  select("TitleRemoved","Date" )
Apr_msnbc <-msnbc%>%
  filter(str_detect(Date, "2020. 4."))
Mar_msnbc <- msnbc%>%
  filter(str_detect(Date, "2020. 3. "))
Feb_msnbc <- msnbc%>%
  filter(str_detect(Date, "2020. 2. "))
Jan_msnbc <- msnbc%>%
  filter(str_detect(Date, "2020. 1. "))

Jan_msnbc_title <-as.character(Jan_msnbc$TitleRemoved)
Jan_msnbc_title <- tibble(line= 1:666, text=Jan_msnbc_title)
Jan_msnbc_words <- Jan_msnbc_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Jan_msnbc_words$word <-gsub("[^0-9A-Za-z///' ]","" , Jan_msnbc_words$word ,ignore.case = TRUE)
Jan_msnbc_words$word <-gsub("[[:punct:]]", "", Jan_msnbc_words$word )
Jan_msnbc_words$word<-gsub("msnbc", "", Jan_msnbc_words$word)
Jan_msnbc_words$word<-gsub("trumps", "trump", Jan_msnbc_words$word, ignore.case=TRUE)
Jan_msnbc_words$word<-removeNumbers(Jan_msnbc_words$word, ucp = FALSE)

Feb_msnbc_title <-as.character(Feb_msnbc$TitleRemoved)
Feb_msnbc_title <- tibble(line= 1:718, text=Feb_msnbc_title)
Feb_msnbc_words <- Feb_msnbc_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Feb_msnbc_words$word <-gsub("[^0-9A-Za-z///' ]","" , Feb_msnbc_words$word ,ignore.case = TRUE)
Feb_msnbc_words$word <-gsub("[[:punct:]]", "", Feb_msnbc_words$word )
Feb_msnbc_words$word<-gsub("msnbc", "", Feb_msnbc_words$word)
Feb_msnbc_words$word<-gsub("trumps", "trump", Feb_msnbc_words$word, ignore.case=TRUE)
Feb_msnbc_words$word<-removeNumbers(Feb_msnbc_words$word, ucp = FALSE)

Mar_msnbc_title <-as.character(Mar_msnbc$TitleRemoved)
Mar_msnbc_title <- tibble(line= 1:936, text=Mar_msnbc_title)
Mar_msnbc_words <- Mar_msnbc_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Mar_msnbc_words$word <-gsub("[^0-9A-Za-z///' ]","" , Mar_msnbc_words$word ,ignore.case = TRUE)
Mar_msnbc_words$word <-gsub("[[:punct:]]", "", Mar_msnbc_words$word )
Mar_msnbc_words$word<-gsub("msnbc", "", Mar_msnbc_words$word)
Mar_msnbc_words$word<-gsub("trumps", "trump", Mar_msnbc_words$word, ignore.case=TRUE)
Mar_msnbc_words$word<-removeNumbers(Mar_msnbc_words$word, ucp = FALSE)

Apr_msnbc_title <-as.character(Apr_msnbc$TitleRemoved)
Apr_msnbc_title <- tibble(line= 1:439, text=Apr_msnbc_title)
Apr_msnbc_words <- Apr_msnbc_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Apr_msnbc_words$word <-gsub("[^0-9A-Za-z///' ]","" , Apr_msnbc_words$word ,ignore.case = TRUE)
Apr_msnbc_words$word <-gsub("[[:punct:]]", "", Apr_msnbc_words$word )
Apr_msnbc_words$word<-gsub("msnbc", "", Apr_msnbc_words$word)
#Apr_words$word <-gsub('[[:digit:]]',"", Apr_words$word )
Apr_msnbc_words$word<-gsub("trumps", "trump", Apr_msnbc_words$word, ignore.case=TRUE)
Apr_msnbc_words$word<-removeNumbers(Apr_msnbc_words$word, ucp = FALSE)
######################################################################################################
######연아######
fox <- read_excel("../fox_data_removedduplicates.xlsx")
docFox <-  fox[,1]
docFox$Title <-gsub("[[:punct:]]", "", docFox$Title)
docFox$Title <-gsub("fox", "", docFox$Title, ignore.case = TRUE) #dropping fox
docFox$Title <-gsub("[^0-9A-Za-z///' ]","", docFox$Title,ignore.case = TRUE)
docFox$Title <-gsub("Trumps", "Trump", docFox$Title, ignore.case = TRUE)

fox_title <- as.character(docFox$Title)
fox_title <- tibble(line= 1:1764, text=fox_title)
fox_title <- as.data.frame(fox_title)
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

# split into 5 clusters
fox_groups1 <- cutree(fox_cluster1, 5)

fox.cluster.1 <- fox_title$text[fox_groups1 == 1 ]
#length(fox.cluster.1)
fox.cluster.2 <- fox_title$text[fox_groups1 == 2 ]
fox.cluster.3 <- fox_title$text[fox_groups1 == 3 ]
fox.cluster.4 <- fox_title$text[fox_groups1 == 4 ]
fox.cluster.5 <- fox_title$text[fox_groups1 == 5 ]

# MSNBC data

msnbcData<-read_excel("../msnbc_data.xlsx")
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
msnbc.cluster.2 <- msnbc_title$text[msnbc.groups1 == 2 ]
msnbc.cluster.3 <- msnbc_title$text[msnbc.groups1 == 3 ]
msnbc.cluster.4 <- msnbc_title$text[msnbc.groups1 == 4 ]
msnbc.cluster.5 <- msnbc_title$text[msnbc.groups1 == 5 ]

######################################################################################################
#Trump
fox_title_TRUMP <- fox_title[(grepl("Trump",fox_title$text)),]

fox_Trump_bigrams <- fox_title_TRUMP%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

fox_Trump_bigrams2 <- fox_Trump_bigrams[(grepl("trump",fox_Trump_bigrams$bigram)),]

msnbc_title_TRUMP <- msnbc_title[(grepl("Trump",msnbc_title$text)),]

msnbc_Trump_bigrams <-msnbc_title_TRUMP%>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

msnbc_Trump_bigrams2 <- msnbc_Trump_bigrams[(grepl("trump",msnbc_Trump_bigrams$bigram)),]

######################################################################################################
##model
fox_msnbc_matrix <- read.csv("../fox_msnbc_matrix_revised.csv")

summary(fox_msnbc_matrix$View)
#plot(density(fox_msnbc_matrix$View))
#hist(fox_msnbc_matrix$View, breaks=100, col="red")
fox_msnbc_matrix$view.logic <- ifelse(fox_msnbc_matrix$View >= 250000, "large", "small")
fox_msnbc_matrix <- fox_msnbc_matrix[,-1]
#msnbc_matrix <- msnbc_matrix[,-2]


#preparing the data
set.seed(12345)
training.samples <- fox_msnbc_matrix$view.logic%>%
  createDataPartition(p=0.8, list=FALSE )
train.data <- fox_msnbc_matrix[training.samples,]
test.data <- fox_msnbc_matrix[-training.samples,]


# Dumy code categorical predictor variables
x <- model.matrix(view.logic~., train.data, na.omit=TRUE)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(train.data$view.logic =="large", 1, 0)

library(glmnet)
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
######################################################################################################
####function to draw bigram
fox.cluster <- list(fox.cluster.1, fox.cluster.2, fox.cluster.3, fox.cluster.4, fox.cluster.5)
msnbc.cluster <- list(msnbc.cluster.1, msnbc.cluster.2, msnbc.cluster.3,msnbc.cluster.4,msnbc.cluster.5)

draw_Cluster_Bigram <- function(num_Clust, press){
  news.c <- press[[num_Clust]]
  news.c_title <- as.character(news.c)
  news.c_title <- tibble(line= 1:length(news.c), text=news.c)
  news.c_title <- as.data.frame(news.c_title)
  
  c.news_title_TRUMP <- news.c_title[(grepl("Trump",news.c_title$text)),]
  
  if(nrow(c.news_title_TRUMP) > 4) {
    c.news_Trump_bigrams <-c.news_title_TRUMP%>%
      unnest_tokens(bigram, text, token="ngrams", n=2)
    
    c.news_Trump_bigrams2 <- c.news_Trump_bigrams[(grepl("trump",c.news_Trump_bigrams$bigram)),]
    
    plot <- (c.news_Trump_bigrams2 %>%
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
    return(print(plot))
  } else {
    plot.new()
    return(textbox(c(0,1), 1, textlist = "no 'Trump' word in the title of this cluster",  border="red"))
  }
}
random_title <- function(cluster){
  c.sample <- sample(x = cluster, size = 5, replace = FALSE)
  c.sample <- sample(x = cluster, size = 5, replace = FALSE)
  #paste(c.sample[1], c.sample[2],c.sample[3],c.sample[4],c.sample[5], sep="\n")
  #paste(c.sample, sep="\n")
  return(c.sample)
  #return(print(c.sample))
  #c.sample <- paste(c.sample,"\n", sep="")
  #return(c.sample)
  #return(cat(c.sample, sep = "\n"))
  #return(cat("5 Random title from the cluster: ", c.sample, sep = "\n"))
}
######################################################################################################
server <- function(input, output) {
  output$selected_var <- renderText({ # test ouput code
    paste("Your var name is ", paste0(input$month, "_fox_words"))
    paste("type of file is ", typeof(Jan_fox_words))
    paste0(input$month, input$pressName2)
  })
  var <- reactive({ # 왜 이게 안돼!!!!
    paste0(input$month, input$pressName2)
  })
  output$monthPlot <- renderPlot({ 
    variable <- switch(paste0(input$month, input$pressName2), 
                   "JanFox" = Jan_fox_words,
                   "FebFox" = Feb_fox_words,
                   "MarFox" = Mar_fox_words,
                   "AprFox" = Apr_fox_words,
                   "JanMSNBC" = Jan_msnbc_words,
                   "FebMSNBC" = Feb_msnbc_words,
                   "MarMSNBC" = Mar_msnbc_words,
                   "AprMSNBC" = Apr_msnbc_words)
    num <- switch(paste0(input$month, input$pressName2), 
                  "JanFox" = 21,
                  "FebFox" = 21,
                  "MarFox" = 26,
                  "AprFox" = 13,
                  "JanMSNBC" = 40,
                  "FebMSNBC" = 37,
                  "MarMSNBC" = 35,
                  "AprMSNBC" = 20)
    variable %>% filter(word != "") %>%
      count(word, sort=TRUE)%>%
      filter(n>num)%>%
      mutate(word=reorder(word, n))%>%
      ggplot(aes(word, n))+
      geom_col()+
      xlab(NULL)+
      coord_flip()+
      geom_text(aes(label=n),hjust=-0.3)
    })
  output$overallPlot1 <- renderPlot({ 
    cluster <- switch(input$pressName, 
                       "Fox" = fox.cluster.1,
                       "MSNBC" = msnbc.cluster.1
    )
    wordcloud(cluster, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
  })
  output$overallPlot2 <- renderPlot({ 
    cluster <- switch(input$pressName, 
                      "Fox" = fox.cluster.2,
                      "MSNBC" = msnbc.cluster.2
    )
    wordcloud(cluster, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
  })
  output$overallPlot3 <- renderPlot({ 
    cluster <- switch(input$pressName, 
                      "Fox" = fox.cluster.3,
                      "MSNBC" = msnbc.cluster.3
    )
    wordcloud(cluster, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
  })
  output$overallPlot4 <- renderPlot({ 
    cluster <- switch(input$pressName, 
                      "Fox" = fox.cluster.4,
                      "MSNBC" = msnbc.cluster.4
    )
    wordcloud(cluster, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
  })
  output$overallPlot5 <- renderPlot({ 
    cluster <- switch(input$pressName, 
                      "Fox" = fox.cluster.5,
                      "MSNBC" = msnbc.cluster.5
    )
    wordcloud(cluster, max.words = 100, min.freq = 3, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
  })
  output$trumpFox <- renderPlot({ 
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
  })
  output$trumpMsnbc <- renderPlot({ 
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
  })
  output$model <- renderPlot({ 
    plot(cv.lasso)
  })
  output$cluster1Size <- renderValueBox({
    press <- switch(paste0(input$pressName), 
                  "Fox" = fox.cluster.1,
                  "MSNBC" = msnbc.cluster.1)
    valueBox(
      paste0(length(press)), "Cluster 1 Size", icon = icon("layer-group"),
      color = "red"
    )
  })
  output$cluster2Size <- renderValueBox({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.2,
                    "MSNBC" = msnbc.cluster.2)
    valueBox(
      paste0(length(press)), "Cluster 2 Size", icon = icon("layer-group"),
      color = "orange"
    )
  })
  output$cluster3Size <- renderValueBox({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.3,
                    "MSNBC" = msnbc.cluster.3)
    valueBox(
      paste0(length(press)), "Cluster 3 Size", icon = icon("layer-group"),
      color = "green"
    )
  })
  output$cluster4Size <- renderValueBox({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.4,
                    "MSNBC" = msnbc.cluster.4)
    valueBox(
      paste0(length(press)), "Cluster 4 Size", icon = icon("layer-group"),
      color = "blue"
    )
  })
  output$cluster5Size <- renderValueBox({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.5,
                    "MSNBC" = msnbc.cluster.5)
    valueBox(
      paste0(length(press)), "Cluster 5 Size", icon = icon("layer-group"),
      color = "purple"
    )
  })
  
  output$bigramPlot1 <- renderPlot({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster,
                    "MSNBC" = msnbc.cluster)
    draw_Cluster_Bigram(1, press)
  })
  output$bigramPlot2 <- renderPlot({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster,
                    "MSNBC" = msnbc.cluster)
    draw_Cluster_Bigram(2, press)
  })
  output$bigramPlot3 <- renderPlot({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster,
                    "MSNBC" = msnbc.cluster)
    draw_Cluster_Bigram(3, press)
  })
  output$bigramPlot4 <- renderPlot({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster,
                    "MSNBC" = msnbc.cluster)
    draw_Cluster_Bigram(4, press)
  })
  output$bigramPlot5 <- renderPlot({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster,
                    "MSNBC" = msnbc.cluster)
    draw_Cluster_Bigram(5, press)
  })
  output$title1 <- renderUI({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.1,
                    "MSNBC" = msnbc.cluster.1)
    title<- random_title(press)
    #paste(title[1],title[2],title[3],title[4],title[5], sep="\n")
    HTML(paste("<h3>5 Random titles from the cluster:</h3>",title[1],title[2],title[3],title[4],title[5], sep="<br/>"))
  })
  output$title2 <- renderUI({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.2,
                    "MSNBC" = msnbc.cluster.2)
    title<-random_title(press)
    HTML(paste("<h4>5 Random titles from the cluster:</h4>",title[1],title[2],title[3],title[4],title[5], sep="<br/>"))
  })
  output$title3 <- renderUI({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.3,
                    "MSNBC" = msnbc.cluster.3)
    #HTML(random_title(press), sep="<br/>")
    title<-random_title(press)
    HTML(paste("<h2>5 Random titles from the cluster:</h2>",title[1],title[2],title[3],title[4],title[5], sep="<br/>"))
  })
  output$title4 <- renderUI({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.4,
                    "MSNBC" = msnbc.cluster.4)
    #random_title(press)
    title<-random_title(press)
    HTML(paste("<b>5 Random titles from the cluster:</b>",title[1],title[2],title[3],title[4],title[5], sep="<br/>"))
  })
  output$title5 <- renderUI({
    press <- switch(paste0(input$pressName), 
                    "Fox" = fox.cluster.5,
                    "MSNBC" = msnbc.cluster.5)
    #random_title(press)
    title<-random_title(press)
    HTML(paste("<b>5 Random titles from the cluster:</b>",title[1],title[2],title[3],title[4],title[5], sep="<br/>"))
  })
}