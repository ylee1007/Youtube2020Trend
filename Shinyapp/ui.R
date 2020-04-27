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
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               # TODO put graph
               ggvisOutput("plot1"), height = 300
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
                             "January" = 1,
                             "Feburary" = 2,
                             "March" = 3,
                             "April" = 4
                           ),
                           selected = "3"
               ),
               p(
                 class = "text-muted",
                 paste("Note: April data includes video from 4/1 to 4/12.")
               ),
               actionButton("go", "Show Graph", icon("chart-bar"))
           )
    ),
    column(width = 9,
           box(width = NULL, solidHeader = TRUE, height = 300
               # TODO put graph
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
           box(title = 'Fox vs. MSNBC bigram (Trump)', status = "warning", width = NULL, solidHeader = TRUE, height = 300
               # TODO put graph
           )
    )
  )
)

dashboardPage(skin="red",
  header,
  dashboardSidebar(disable = TRUE),
  body
)