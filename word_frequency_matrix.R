library(readxl)
library(tm)
library(dplyr)
msnbc <- read_excel("list_mode_export_msnbc_final.xlsx")

doc <- msnbc[,5]
doc <- as.vector(doc)
# doc<-gsub("[^a-zA-Z0-9 ]","",doc[,1])
doc$TitleRemoved <-gsub("[[:punct:]]", "", doc$TitleRemoved)
#doc$TitleRemoved <-gsub("^\'", "", doc$TitleRemoved)
doc$TitleRemoved <-gsub("[^0-9A-Za-z///' ]","" , doc$TitleRemoved ,ignore.case = TRUE)
#doc$TitleRemoved <-gsub(".'.'","" , doc$TitleRemoved ,ignore.case = TRUE)
doc$TitleRemoved<-gsub("Trumps", "Trump", doc$TitleRemoved, ignore.case=TRUE)
#docsd<-pull(doc, TitleRemoved)
tm_corpus <- VCorpus(VectorSource(c(doc$TitleRemoved)))


tm_corpus <- tm_map(tm_corpus, tolower)
tm_corpus <- tm_map(tm_corpus, removePunctuation)
tm_corpus <- tm_map(tm_corpus, removeWords, stopwords("english"))
tm_corpus <- tm_map(tm_corpus, removeNumbers)
tm_corpus <- tm_map(tm_corpus, PlainTextDocument)
#tm_corpus <- tm_map(tm_corpus, stemDocument, language="english")
tm_corpus <- tm_map(tm_corpus, stripWhitespace)
tm_corpus <- tm_map(tm_corpus, PlainTextDocument)
# removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
# tm_corpus <- tm_map(tm_corpus, removeSpecialChars)

tdm <- TermDocumentMatrix(tm_corpus)
matrix <-as.matrix(tdm)
matrix <- t(matrix)

write.csv(matrix,"msnbc_matrix.csv")

##########remove the global environment first and jump over to fox ################
######################################################################3
#fox
fox <- read_excel("fox_data_removedduplicates.xlsx")

doc.1 <- fox[,1]
doc.1 <- as.vector(doc.1)
# doc<-gsub("[^a-zA-Z0-9 ]","",doc[,1])
doc.1$Title <-gsub("[[:punct:]]", "", doc.1$Title)
#doc$TitleRemoved <-gsub("^\'", "", doc$TitleRemoved)
doc.1$Title <-gsub("[^0-9A-Za-z///' ]","" , doc.1$Title ,ignore.case = TRUE)
#doc$TitleRemoved <-gsub(".'.'","" , doc$TitleRemoved ,ignore.case = TRUE)
doc.1$Title<-gsub("Trumps", "Trump", doc.1$Title, ignore.case=TRUE)
#docsd<-pull(doc, TitleRemoved)
tm_corpus <- VCorpus(VectorSource(c(doc.1$Title)))

tm_corpus <- tm_map(tm_corpus, tolower)
tm_corpus <- tm_map(tm_corpus, removePunctuation)
tm_corpus <- tm_map(tm_corpus, removeWords, stopwords("english"))
tm_corpus <- tm_map(tm_corpus, removeNumbers)
tm_corpus <- tm_map(tm_corpus, PlainTextDocument)
#tm_corpus <- tm_map(tm_corpus, stemDocument, language="english")
tm_corpus <- tm_map(tm_corpus, stripWhitespace)
tm_corpus <- tm_map(tm_corpus, PlainTextDocument)
# removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
# tm_corpus <- tm_map(tm_corpus, removeSpecialChars)

tdm <- TermDocumentMatrix(tm_corpus)
matrix <-as.matrix(tdm)
matrix <- t(matrix)

write.csv(matrix,"fox_matrix.csv")


