library(shinydashboard)
library(ggvis)

header <- dashboardHeader(
  title = "Trump on Youtube: an analysis of the title in major news Youtube channels", titleWidth = 700
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Fox vs. MSNBC", tabName = "main", icon = icon("far fa-newspaper"),
             menuSubItem('Analysis', tabName = "mainAnalysis", icon = icon('fas fa-chart-line')),
             menuSubItem('Model', tabName = "model", icon = icon("bar-chart-o"))
             ),
    menuItem("Monthly Analysis", tabName = "monthly", icon = icon("bar-chart-o")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "monthly",
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
      )
    ),
    tabItem(tabName = "mainAnalysis",
            fluidRow(
              column(width = 12,
                     fluidRow(
                       column(width = 6,
                              box(title = 'Trump Bigram on Fox', status = "warning", width = NULL, solidHeader = TRUE, height = 460,
                                  plotOutput("trumpFox")
                              )),
                       column(width = 6,
                              box(title = 'Trump Bigram on MSNBC', status = "warning", width = NULL, solidHeader = TRUE, height = 460,
                                  plotOutput("trumpMsnbc")
                              ))
                     )
              )
            ),
            fluidRow(
        column(width = 3,
               fluidRow(
                 column(width = 12,
               box(title = '2020 News Topic Trend', width = NULL, status = "warning", solidHeader = TRUE,
                   selectInput("pressName", "Select News:",
                               choices = c(
                                 "Fox" = "Fox",
                                 "MSNBC" = "MSNBC"
                               ),
                               selected = "Fox"
                   ),
                   #selectInput("numK", "Select number of K:",
                   #             choices = c(
                   #               "2" = 2,
                   #               "3" = 3,
                   #               "4" = 4,
                   #               "5" = 5,
                   #               "6" = 6,
                   #               "7" = 7,
                   #               "8" = 8
                   #             ),
                   #             selected = "5"
                   # ),
                   p(
                     class = "text-muted",
                     paste("Note: Data includes video from 1/1 to 4/12.")
                   )
                   #actionButton("go", "Show Graph", icon("chart-bar"))
                   #submitButton("submit", "Show Graph", icon("chart-bar"))
               )
                 )
               ),
               fluidRow(
                 column(width = 12
                        #tabBox(
                         # title = "Cluster Graph", width = NULL,
                          # The id lets us use input$tabset1 on the server to find the current tab
                          #id = "tabset1",
                          #tabPanel("Cluster 1", plotOutput("overallPlot1")),
                          #tabPanel("Cluster 2", plotOutput("overallPlot2")),
                          #tabPanel("Cluster 3", plotOutput("overallPlot3")),
                          #tabPanel("Cluster 4", plotOutput("overallPlot4")),
                          #tabPanel("Cluster 5", plotOutput("overallPlot5"))
                        #)
                        )
               )
        ),
        column(9,
               valueBoxOutput("cluster1Size"),
               valueBoxOutput("cluster2Size"),
               valueBoxOutput("cluster3Size"),
               valueBoxOutput("cluster4Size"),
               valueBoxOutput("cluster5Size")
        )
      ),
      fluidRow(
        column(width =6,
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
        ),
        column(width =6,
               tabBox(
                 title = "Trump Bigram", width = NULL,
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "tabset1",
                 tabPanel("Cluster 1", plotOutput("bigramPlot1"), htmlOutput("title1")),
                 tabPanel("Cluster 2", plotOutput("bigramPlot2"), htmlOutput("title2")),
                 tabPanel("Cluster 3", plotOutput("bigramPlot3"), htmlOutput("title3")),
                 tabPanel("Cluster 4", plotOutput("bigramPlot4"), htmlOutput("title4")),
                 tabPanel("Cluster 5", plotOutput("bigramPlot5"), htmlOutput("title5"))
               )
        )
      )
    ),
    tabItem(tabName = "model",
      fluidRow(
        column(width = 12,
               box(title = 'Penalized Logistic Regression Model', width = NULL, status = "warning", solidHeader = TRUE, height = 460,
                   plotOutput("model")
               )
        )
      )
    )
  )
)

dashboardPage(skin="red",
  header,
  sidebar,
  body
)