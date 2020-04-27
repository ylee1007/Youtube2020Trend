library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(tidytext)

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

Feb_fox_title <-as.character(Feb_fox$Title)
Feb_fox_title <- tibble(line= 1:468, text=Feb_fox_title)
Feb_fox_words <- Feb_fox_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Feb_fox_words$word <-gsub("[^0-9A-Za-z///' ]","" , Feb_fox_words$word ,ignore.case = TRUE)
Feb_fox_words$word <-gsub("[[:punct:]]", "", Feb_fox_words$word )
Feb_fox_words$word<-gsub("fox", "", Feb_fox_words$word)
Feb_fox_words$word<-gsub("trumps", "trump", Feb_fox_words$word, ignore.case=TRUE)

Mar_fox_title <-as.character(Mar_fox$Title)
Mar_fox_title <- tibble(line= 1:582, text=Mar_fox_title)
Mar_fox_words <- Mar_fox_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Mar_fox_words$word <-gsub("[^0-9A-Za-z///' ]","" , Mar_fox_words$word ,ignore.case = TRUE)
Mar_fox_words$word <-gsub("[[:punct:]]", "", Mar_fox_words$word )
Mar_fox_words$word<-gsub("fox", "", Mar_fox_words$word)
Mar_fox_words$word<-gsub("trumps", "trump", Mar_fox_words$word, ignore.case=TRUE)

Apr_fox_title <-as.character(Apr_fox$Title)
Apr_fox_title <- tibble(line= 1:254, text=Apr_fox_title)
Apr_fox_words <- Apr_fox_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Apr_fox_words$word <-gsub("[^0-9A-Za-z///' ]","" , Apr_fox_words$word ,ignore.case = TRUE)
Apr_fox_words$word <-gsub("[[:punct:]]", "", Apr_fox_words$word )
#Apr_fox_words$word <-gsub('[[:digit:]]+',"", Apr_fox_words$word )
Apr_fox_words$word<-gsub("fox", "", Apr_fox_words$word)
Apr_fox_words$word<-gsub("trumps", "trump", Apr_fox_words$word, ignore.case=TRUE)

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

Feb_msnbc_title <-as.character(Feb_msnbc$TitleRemoved)
Feb_msnbc_title <- tibble(line= 1:718, text=Feb_msnbc_title)
Feb_msnbc_words <- Feb_msnbc_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Feb_msnbc_words$word <-gsub("[^0-9A-Za-z///' ]","" , Feb_msnbc_words$word ,ignore.case = TRUE)
Feb_msnbc_words$word <-gsub("[[:punct:]]", "", Feb_msnbc_words$word )
Feb_msnbc_words$word<-gsub("msnbc", "", Feb_msnbc_words$word)
Feb_msnbc_words$word<-gsub("trumps", "trump", Feb_msnbc_words$word, ignore.case=TRUE)

Mar_msnbc_title <-as.character(Mar_msnbc$TitleRemoved)
Mar_msnbc_title <- tibble(line= 1:936, text=Mar_msnbc_title)
Mar_msnbc_words <- Mar_msnbc_title %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
Mar_msnbc_words$word <-gsub("[^0-9A-Za-z///' ]","" , Mar_msnbc_words$word ,ignore.case = TRUE)
Mar_msnbc_words$word <-gsub("[[:punct:]]", "", Mar_msnbc_words$word )
Mar_msnbc_words$word<-gsub("msnbc", "", Mar_msnbc_words$word)
Mar_msnbc_words$word<-gsub("trumps", "trump", Mar_msnbc_words$word, ignore.case=TRUE)

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
    variable %>%
      count(word, sort=TRUE)%>%
      filter(n>num)%>%
      mutate(word=reorder(word, n))%>%
      ggplot(aes(word, n))+
      geom_col()+
      xlab(NULL)+
      coord_flip()+
      geom_text(aes(label=n),hjust=-0.3)
    })
}