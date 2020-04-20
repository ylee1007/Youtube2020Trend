# package
library(utils)
library(dplyr)
library(readxl)
library(quanteda)
library(stringr)
library('cluster')    # clustering algorithms
library('factoextra') # clustering algorithms & visualization

# read data as xlsx file
msnbcData<-read_excel("list_mode_export_msnbc_final.xlsx")

msnbcDf <- as.data.frame(msnbcData)
msnbc <- msnbcDf %>% select("Title", "Address", "Meta Description 1", "Meta Keyword 1" ,"Hash")
msnbc$Title <- as.character(msnbc$Title)
msnbc$Address <- as.character(msnbc$Address)
msnbc$Meta.Description.1 <- as.character(msnbc$`Meta Description 1`)
msnbc$Meta.Keyword.1<- as.character(msnbc$`Meta Keyword 1`)
msnbc$Hash <- as.character(msnbc$Hash)

str(msnbc)

# Title
TitleWord <-word(msnbc$Title)
Title_Token <- tokens(msnbc$Title, what = "word", remove_numbers = TRUE, 
                      remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
trainTT <- Title_Token %>% tokens_tolower() %>% tokens_remove("msbnc") %>% 
  tokens_select(stopwords(), selection = "remove") %>% tokens_wordstem(language="english")
trainTT_dfm <- dfm(trainTT, tolower= FALSE)
counts = topfeatures(trainTT_dfm, n = 1000, decreasing = TRUE, 
                     scheme = c("count", "docfreq"), group = NULL)
counts
# remove msnbc
top1000_withMsnbc <- names(counts)
top1000 <- top1000_withMsnbc[2:1001]

top1000_dfm <- dfm_select(trainTT_dfm, pattern = top1000)
top1000_matrix <- as.matrix(top1000_dfm)
dim(top1000_matrix)
top1000_df <- as.data.frame(top1000_matrix)

barplot(counts[1:50]) # have msnbc

k <- kmeans(trainTT_dfm, centers = 5, nstart= 25)
fviz_cluster(k, data = trainTT_dfm)
