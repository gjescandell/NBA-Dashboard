library(shinyTime)
library(shiny)
library(plotly)
library(shinydashboard)
library(shinyalert)
library(formattable)
library(kableExtra)
library(tableHTML)
library(shinythemes)
library(shinycssloaders)
library(lubridate)
library(leaflet)

tagList(
  tags$head(tags$link(rel="shortcut icon", href='nba.ico')),
  tags$head(tags$link(rel="icon", href='nba.ico')),
  tags$style(HTML("
    .navbar-brand {
    padding: 5px 15px 10px 15px;
    }
    
    h3{
    text-transform: uppercase;
    font-family: NBA Bucks Normal;
    font-weight: 900;
    text-align: center;
    }
    
    #rTeam1, #rTeam2, #ResultVS{
           text-transform: uppercase;
           font-size: 24px;
           font-family: NBA Bucks Normal;
           font-weight: 900;
           text-align:center;
           padding:40px;
           margin:auto;
    }
    
    #ResultVS{
      font-size: 26px;
      font-weight: 990;
      padding: 45px 0px 45px 0px ;
    
  ")),
  
  ui <- fluidPage(theme = shinytheme('flatly'),title = "NBA Stats",
                  navbarPage(title = img(src = "nba.ico", height= 45, width = 45),
                             tabPanel("Player Stats",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     h4("Player (4 max.)"),
                                                     uiOutput("uirender_players"),
                                                     h4("Seasons"),
                                                     uiOutput("uirender_seasonsPlayer")
                                        ),
                                        mainPanel(
                                          tabsetPanel(
                                            tabPanel("Profiles",
                                                     fluidRow(
                                                       column(3,girafeOutput("player1_MPG", height ="300px")%>% withSpinner(color="#2C3E50")),
                                                       column(3,girafeOutput("player1_PER", height ="300px")%>% withSpinner(color="#2C3E50")),
                                                       column(3,girafeOutput("player1_BPM", height ="300px")%>% withSpinner(color="#2C3E50")),
                                                       column(3,girafeOutput("player1_WS", height ="300px")%>% withSpinner(color="#2C3E50")),
                                                       column(3, radioButtons(
                                                         "var_type",
                                                         "Choose variable:",
                                                         c("MPG" = "MPG", "PER" = "PER", "BPM" = "BPM", "WS" = "WS"),
                                                         selected = "PER"
                                                       )),
                                                       column(9, plotlyOutput("linePerPlayer",height ="300px")%>% withSpinner(color="#2C3E50"))
                                                     )
                                                     
                                            ),
                                            tabPanel("Season",
                                                     )
                             )))),
                             # TEAM -----------------------------------
                             tabPanel("Team Stats",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     h4("Player (4 max.)"),
                                                     #uiOutput("uirender_players"),
                                                     h4("Seasons"),
                                                     #uiOutput("uirender_seasonsPlayer")
                                        ),
                                        mainPanel(
                                          tabsetPanel(
                                            tabPanel("Summary",
                                                     fluidRow(
                                                       # column(3,girafeOutput("player1_MPG")%>% withSpinner(color="#2C3E50")),
                                                       # column(3,girafeOutput("player1_PER")%>% withSpinner(color="#2C3E50")),
                                                       # column(3,girafeOutput("player1_BPM")%>% withSpinner(color="#2C3E50")),
                                                       # column(3,girafeOutput("player1_WS")%>% withSpinner(color="#2C3E50")),
                                                       # column(3, radioButtons(
                                                       #   "var_type",
                                                       #   "Choose variable:",
                                                       #   c("MPG" = "MPG", "PER" = "PER", "BPM" = "BPM", "WS" = "WS"),
                                                       #   selected = "PER"
                                                       # )),
                                                       # column(9, plotlyOutput("linePerPlayer",height ="300px")%>% withSpinner(color="#2C3E50"))
                                                     )
                                                     
                                            ),
                                            tabPanel("Season",
                                            )
                                          )))),

                             # MATCHES -----------------------------------
                             tabPanel("Match Results",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     h4("Teams"),
                                                     uiOutput("uirender_teamsGame"),
                                                     h4("Seasons"),
                                                     uiOutput("uirender_seasonsGame")
                                        ),
                                        mainPanel(
                                          tabsetPanel(
                                            tabPanel("Summary",
                                                     fluidRow(
                                                       h3("Match history"),
                                                       column(12, dataTableOutput("matchTable")%>% withSpinner(color="#2C3E50"))
                                                     )
                                            ),
                                            tabPanel("Comparison", 
                                                     fluidRow(
                                                       column(4,plotlyOutput("compCircle")%>% withSpinner(color="#2C3E50")),
                                                       column(8, fluidRow(
                                                         column(5,textOutput("rTeam1")),
                                                         column(2,textOutput("ResultVS")),
                                                         column(5,textOutput("rTeam2"))
                                                         )),
                                                       column(12,plotlyOutput("compBardiff",height ="300px")%>% withSpinner(color="#2C3E50"))
                                                       
                                                     )
                                                     
                                            ),
                                            tabPanel("Attendance", 
                                                     fluidRow(
                                                       column(5, plotlyOutput("attendAVG",height ="280px")%>% withSpinner(color="#2C3E50")),
                                                       column(7, plotlyOutput("attendPerGame",height ="300px")%>% withSpinner(color="#2C3E50")),
                                                       column(12, plotlyOutput("attendMax",height ="400px")%>% withSpinner(color="#2C3E50"))
                                                     )
                                                     )
                                                     
                                            )
                                          )
                                        )
                                     
                             )

                             , tabPanel("", icon = icon("question-circle"),
                                        tags$iframe(style="height:850px; width:100%; scrolling=yes",
                                                    src="https://www.nba-reference.com/")%>% withSpinner(color="#2FA4E7"))
                         
                          )
          )
)
  
  