library(shinydashboard)
library(ggvis)

header <- dashboardHeader(
  title = "2020 Youtube Trend"
)

body <- dashboardBody(
  fluidRow(
    column(width = 3,
           box(title = 'Overall 2020 News Trend', width = NULL, status = "warning", solidHeader = TRUE,
               selectInput("pressName", "Select News:",
                           choices = c(
                             "Fox" = "Fox",
                             "MSNBC" = "MSNBC"
                           ),
                           selected = "Fox"
               ),
               selectInput("numK", "Select number of K:",
                           choices = c(
                             "2" = 2,
                             "3" = 3,
                             "4" = 4,
                             "5" = 5,
                             "6" = 6,
                             "7" = 7,
                             "8" = 8
                           ),
                           selected = "5"
               ),
               p(
                 class = "text-muted",
                 paste("Note: Data includes video from 1/1 to 4/12.")
               ),
               actionButton("go", "Show Graph", icon("chart-bar"))
               #submitButton("submit", "Show Graph", icon("chart-bar"))
           )
    ),
    column(9,
           tabBox(
             title = "Cluster Graph", width = NULL,
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1",
             tabPanel("Cluster 1", plotOutput("overallPlot1")),
             tabPanel("Cluster 2", plotOutput("overallPlot2")),
             tabPanel("Cluster 3", plotOutput("overallPlot3")),
             tabPanel("Cluster 4", plotOutput("overallPlot4")),
             tabPanel("Cluster 5", plotOutput("overallPlot5"))
           )
    )
  ),
  fluidRow(
    column(width = 3,
           box(title = 'Monthly News Trend', width = NULL, status = "warning", solidHeader = TRUE,
               selectInput("pressName2", "Select News:",
                           choices = c(
                             "Fox" = "Fox",
                             "MSNBC" = "MSNBC"
                           ),
                           selected = "Fox"
               ),
               selectInput("month", "Select Month:",
                           choices = c(
                             "January" = "Jan",
                             "Feburary" = "Feb",
                             "March" = "Mar",
                             "April" = "Apr"
                           ),
                           selected = "Jan"
               ),
               p(
                 class = "text-muted",
                 paste("Note: April data includes video from 4/1 to 4/12.")
               )
               #actionButton("go", "Show Graph", icon("chart-bar"))
           )
    ),
    column(width = 9,
           box(width = NULL, solidHeader = TRUE, height = 420,
               # TODO put graph
               #textOutput("selected_var"),
               plotOutput("monthPlot")
           )
    )
  ),
  fluidRow(
    column(width = 5,
           box(title = 'Model', width = NULL, status = "warning", solidHeader = TRUE, height = 300
               # TODO put graph
           )
    ),
    
    column(width = 7,
           fluidRow(
             column(width = 6,
           box(title = 'Fox bigram (Trump)', status = "warning", width = NULL, solidHeader = TRUE, height = 460,
               plotOutput("trumpFox")
           )),
           column(width = 6,
                  box(title = 'MSNBC bigram (Trump)', status = "warning", width = NULL, solidHeader = TRUE, height = 460,
                      plotOutput("trumpMsnbc")
                  ))
           )
    )
  )
)

dashboardPage(skin="red",
  header,
  dashboardSidebar(disable = TRUE),
  body
)