require(shiny)
library(ggplot2)
library(readxl)
library(DT)
library(data.table)
library(dplyr)
library(plotly)
library(lubridate)
library(shinyTime)
library(rgdal)
library(igraph)
library(leaflet)
library(raster)
library(stringr)
library(tidyr)
library(readr)
library(extrafont)
require(ggiraph)
library(ggiraph)

# Load color
df_color <- read_excel('data/Colors.xlsx', sheet=1)
names(df_color) <- c("Team", "Color_1", "Color_2")


# Load Games -------------------------------------------------------------------

dfg0 <- read_excel('data/Games.xlsx', sheet=1)
dfg1 <- read_excel('data/Games.xlsx', sheet=2)
dfg2 <- read_excel('data/Games.xlsx', sheet=3)
dfg3 <- read_excel('data/Games.xlsx', sheet=4)
dfg0$Season <- '18-19'
dfg1$Season <- '19-20'
dfg2$Season <- '20-21'
dfg3$Season <- '21-22'
df_games <- do.call("rbind", list(dfg0, dfg1, dfg2, dfg3))

df_games <- df_games %>% dplyr::select(-Notes)
names(df_games) <- c("Date", "Time", "Visitor", "PTS_V", "Home", "PTS_H", "OT", "Attend", "Arena", "Season")


df_games$Time <- substr(df_games$Time,1,nchar(df_games$Time)-1)
df_games$Time <- str_pad(df_games$Time, 5, "left", '0')

#df_games['Date'] = as.Date(df_games$Date, format='%Y-%m-%d %I:%M%p')
df_games$Date <- as.Date(df_games$Date, "%Y-%m-%d", tz="EST")

df_games$Visitor <- as.factor(df_games$Visitor)
df_games$Home <- as.factor(df_games$Home)
df_games$Arena <- as.factor(df_games$Arena)
df_games$Season <- as.factor(df_games$Season)

# Load Teams -------------------------------------------------------------------

dft0 <- read.csv("data/teams_18-19.csv", header = TRUE)
dft1 <- read.csv("data/teams_19-20.csv", header = TRUE)
dft2 <- read.csv("data/teams_20-21.csv", header = TRUE)
dft3 <- read.csv("data/teams_21-22.csv", header = TRUE)
dft0$Season <- '18-19'
dft1$Season <- '19-20'
dft2$Season <- '20-21'
dft3$Season <- '21-22'

df_teams <- do.call("rbind", list(dft0, dft1, dft2, dft3))

df_teams <- df_teams %>% dplyr::select(Rk, Team, Age, W, L, MOV, SRS, NRtg, Pace, TS., Season)

df_teams <- df_teams[df_teams$Team != "League Average",]

df_teams$Team<- stringr::str_replace(df_teams$Team, '\\*', '')

df_teams$Season <- as.factor(df_teams$Season)

# Load Players -------------------------------------------------------------------

dfp0 <- read.csv("data/players_18-19.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-Player.additional)
dfp1 <- read.csv("data/players_19-20.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-Player.additional)
dfp2 <- read.csv("data/players_20-21.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-Player.additional)
dfp3 <- read.csv("data/players_21-22.csv", header = TRUE, encoding = "UTF-8")
dfp0$Season <- '18-19'
dfp1$Season <- '19-20'
dfp2$Season <- '20-21'
dfp3$Season <- '21-22'

df_players <- do.call("rbind", list(dfp0, dfp1, dfp2, dfp3))

df_players <- df_players %>%
  separate(Player, c("Player", "user"), sep="\\\\")

df_players <- df_players %>%
  mutate(MPG = round(MP/G, 2))

df_players <- df_players %>% dplyr::select(Player, Pos, Age, Tm, MPG, PER, BPM, WS, Season)

df_players$Pos <- as.factor(df_players$Pos)
df_players$Player <- as.factor(df_players$Player)
df_players$Tm <- as.factor(df_players$Tm)

df_players$Season <- as.factor(df_players$Season)

df_percentplayers <- df_players %>%
  group_by(Player) %>%
  summarise(across(everything(), list(mean))) %>%
  dplyr::select(Player, MPG_1, PER_1, BPM_1, WS_1) %>%
  rename(MPG = MPG_1, PER = PER_1, BPM = BPM_1, WS = WS_1) %>%
  mutate_all(list(diff = ~ (. - mean(.)))) %>%
  mutate(percMPG = (MPG_diff - min(MPG_diff)) / (max(MPG_diff) - min(MPG_diff)),
         percPER = (PER_diff - min(PER_diff)) / (max(PER_diff) - min(PER_diff)),
         percBPM = (BPM_diff - min(BPM_diff)) / (max(BPM_diff) - min(BPM_diff)),
         percWS = (WS_diff - min(WS_diff)) / (max(WS_diff) - min(WS_diff)))


# SERVER -------------  
shinyServer <- function(input, output, session){
    
  # UI ----
  output$dateRange <- renderUI({
    dateRangeInput(inputId='dateRange',
                   label='Initial Date / End Date',
                   min = min(df_games$Date),
                   max = max(df_games$Date),
                   start = min(df_games$Date),
                   end = max(df_games$Date)
     )
   })
  
  # GAMES ----
  #   INPUTS
  output$uirender_teamsGame<- renderUI({
    selectizeInput(inputId = "teams_selectedGame", 
                   label = "", 
                   choices = levels(df_games$Visitor), 
                   #selected = selected$teams, 
                   multiple = TRUE,
                   options= list(maxItems = 2)
                   )
    
  })
  
  output$uirender_seasonsGame<- renderUI({
    selectInput(inputId = "seasons_selectedGame", 
                label = "", 
                choices = levels(df_games$Season), 
                #selected = selected$seasons, 
                multiple = TRUE
    )
  })

  
  #   OUTPUTS
  matchData <- reactive(
      df_games %>% 
        filter(Visitor %in% input$teams_selectedGame & Home %in% input$teams_selectedGame) %>%
        filter(Season %in% input$seasons_selectedGame) %>%
        mutate(id_game = row_number())
    )
  
  myColor <- reactive(
    pull(df_color[which(df_color$Team  %in% input$teams_selectedGame), "Color_1"])
  )
  myTeam <- reactive(
    pull(df_color[which(df_color$Team  %in% input$teams_selectedGame), "Team"])
  )
  
  #Summary
  output$matchTable = renderDataTable(
    datatable(matchData(), filter = "none",
              rownames = F, options = list(dom = 't', scrollX = TRUE,
                                           columnDefs = list(list(className = 'dt-center', targets = "_all")))
              )
  )
  
  #Pie Chart
  ptsSum <- function(df){
    df <- subset( df, select = -c(Date, Time, OT, Attend, Arena, Season ) )
    df <- cbind(df[c(1,3)], stack(df[c(2,4)]))
    
    df <- df %>%
      mutate(Home = replace(Home, which(ind == "PTS_V"), NA)) %>%
      mutate(Visitor = replace(Visitor, which(ind == "PTS_H"), NA)) %>%
      mutate(Team = coalesce(Home, Visitor)) %>%
      dplyr::select(Team, values)
  }
  
  compData <- reactive(
    ptsSum(matchData())
  )
  
  compPie <- reactive(
    compData() %>%
      group_by(Team) %>%
      summarise(Points = sum(values))
  )
  
  output$compCircle <- renderPlotly({
    fig <- plot_ly(compPie(), labels = ~Team, values = ~Points, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste( Points, ' points'),
                   marker = list(colors = myColor(),
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE)
    fig <- fig %>% layout( #title = 'Total points scored',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
  })
  
  #Difference Bar chart
  compDiff <- reactive(
    matchData() %>%
      mutate(Diff = PTS_V - PTS_H) %>%
      mutate(Winner = if_else(Diff>0, Visitor, Home))
  )
  
  output$compBardiff <- renderPlotly({
    
    ggplot(compDiff(), aes(x = id_game, y = Diff, text = paste("Season: ", Season))) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill = ifelse(compDiff()$Winner == myTeam()[1], myColor()[1], myColor()[2]),      # Background color
               color = "white") + # Border color
      xlab("Match played") +
      ylab("Point difference")+ theme(axis.title.x = element_blank())
    })
  
  #Result text
  compT <- reactive(
    compDiff() %>%
      group_by(Winner) %>%
      tally()
  )
  
  output$rTeam1 <- renderText({
    paste(myTeam()[1])
  })
  
  output$ResultVS <- renderText({
    paste(ifelse(is.na(compT()$n[2]), 0, compT()$n[1]), " - ", ifelse(is.na(compT()$n[2]), compT()$n[1], compT()$n[2]))
    })
  
  output$rTeam2 <- renderText({
    paste(myTeam()[2])
  })
  
  #Attendance Average Bar chart
  teamAttend <- reactive(
    matchData() %>%
      group_by(Home) %>%
      dplyr::summarize(Mean = mean(Attend, na.rm=TRUE))
  )
  
  output$attendAVG <- renderPlotly({
    ggplot(teamAttend(), aes(x = Mean, y = Home)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill = myColor(),      # Background color
               color = "white") + # Border color
      xlab("Average") +
      ylab("Team") + theme_classic()
  })
  
  #Line chart per game
  output$attendPerGame <- renderPlotly({
    ggplot(matchData(), aes(x=id_game, y=Attend, text=paste(Date), group=Home , color=Home)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values=myColor()) +
      #geom_hline(yintercept = mean(Attend)) + 
      xlab("Match") +
      ylab("Attendance") + 
      theme_classic()
      
  })
  
  #Arenas sorted bar chart
  arenaAtt <- function(){
    d1 <- df_games %>% 
      filter(Season %in% input$seasons_selectedGame) %>%
      group_by(Arena) %>%
      summarise(Attend = if_else(max(Attend, na.rm=TRUE) <= 0, 0, max(Attend, na.rm=TRUE)))
    
    
    d2 <- df_games %>% 
      filter(Season %in% input$seasons_selectedGame) %>%
      group_by(Arena) %>%
      count(Home) %>% 
      top_n(1)
    
    merge(d1, d2[, c("Arena", "Home")], by="Arena")
    
  }
  arenaAttend <- reactive(
    arenaAtt()
  )
  
  output$attendMax <- renderPlotly({
    ggplot(arenaAttend(), aes(x = reorder(Arena, -Attend), y = Attend, fill=Home)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               color = "white") + # Border color
      ylab("Max. Attendance") + 
      #scale_fill_manual(name="Home",values=df_color[, c("Team", "Color_1")]) + 
      theme(axis.text.x=element_text(angle=45, hjust=1))
    
  })
  
  
  
  # TEAMS ----
  #   INPUTS
  output$uirender_team<- renderUI({
    selectizeInput(inputId = "team_selected", 
                   label = "", 
                   choices = levels(df_teams$Team), 
                   multiple = FALSE
    )
  })
  
  output$uirender_seasonsTeam<- renderUI({
    selectInput(inputId = "seasons_selectedTeam", 
                label = "", 
                choices = levels(df_teams$Season), 
                #selected = selected$seasons, 
                multiple = TRUE
    )
  })
  
  #   OUTPUTS
  teamData <- reactive(
    df_teams %>% 
      filter(Team %in% input$team_selected) %>%
      filter(Season %in% input$seasons_selectedTeam)
  )
  
  
  
  # PLAYERS ----
  #   INPUTS
  
  selected <- reactiveValues(players = vector())
  
  output$uirender_players<- renderUI({
    selectizeInput(inputId = "players_selected", 
                   label = "", 
                   choices = levels(df_players$Player), 
                   selected = selected$players,
                   multiple = TRUE,
                   options= list(maxItems = 4)
    )
    
  })
  
  output$uirender_age<- renderUI({
    sliderInput("sliderAge", label = "", min = min(df_players$Age),
                max = max(df_players$Age), value = c(min(df_players$Age), max(df_players$Age)))
    
  })
  
  output$uirender_seasonsPlayer<- renderUI({
    selectInput(inputId = "seasons_selectedPlayer", 
                label = "", 
                choices = levels(df_players$Season), 
                #selected = selected$seasons, 
                multiple = TRUE
    )
  })
  
  #   OUTPUTS
  # playerFilterData <- reactive(
  #   df_players %>%
  #     filter(Age > input$sliderAge[1] & Age < input$sliderAge[2])
  # )
  
  playerData <- reactive(
    df_players %>% 
      filter(Player %in% input$players_selected)
  )
  
  playerPerSeasonData <- reactive(
    playerData() %>%
      filter(Season %in% input$seasons_selectedPlayer)
  )
  
  percPlayer_data <- reactive(
    df_percentplayers %>%
      filter(Player %in% input$players_selected)
    
    )
  
  
  output$player1_MPG <- renderGirafe({
    donut_data <- percPlayer_data()
    
    donut_plot <- ggplot(donut_data, aes(y = percMPG, fill = Player)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = paste0(Player, ": ", formattable::percent(percMPG),"<br>Minutes Per Game: ", MPG)),
        width = 0.3,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = round(donut_data$MPG[donut_data$percMPG == max(donut_data$percMPG)],1),
        size = 20,
        color = "#2C3E50"
      ) +
      #scale_fill_manual(values = c("lightgray", "#2C3E50")) +
      coord_polar(theta = "y") +
      theme_void() +
      ggtitle("MPG", subtitle = "Minutes Per Game") +
      theme(plot.title = element_text(size = 35, face="bold", hjust = 0.5, margin = margin(20,0,0,0,unit="pt")),
            plot.subtitle = element_text(color = "darkgray", size = 20, hjust = 0.5))
    
    ggiraph(ggobj = donut_plot)
  
  })
  
  output$player1_PER <- renderGirafe({
    donut_data <- percPlayer_data()
    
    donut_plot <- ggplot(donut_data, aes(y = percPER, fill = Player)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = paste0(Player, ": ", formattable::percent(percPER),"<br>Player Efficiency Rate: ", PER)),
        width = 0.3,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = round(donut_data$PER[donut_data$percPER == max(donut_data$percPER)],1),
        size = 20,
        color = "#2C3E50"
      ) +
      #scale_fill_manual(values = c("lightgray", "#2C3E50")) +
      coord_polar(theta = "y") +
      theme_void()+
      ggtitle("PER", subtitle = "Player Efficiency Rate") +
      theme(plot.title = element_text(size = 35, face="bold", hjust = 0.5, margin = margin(20,0,0,0,unit="pt")),
            plot.subtitle = element_text(color = "darkgray", size = 20, hjust = 0.5))
    
    ggiraph(ggobj = donut_plot)
    
  })
  
  output$player1_BPM <- renderGirafe({
    donut_data <- percPlayer_data()
    
    donut_plot <- ggplot(donut_data, aes(y = percBPM, fill = Player)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = paste0(Player, ": ", formattable::percent(percBPM),"<br>Box Plus Minus: ", BPM)),
        width = 0.3,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = round(donut_data$BPM[donut_data$percBPM == max(donut_data$percBPM)],1),
        size = 20,
        color = "#2C3E50"
      ) +
      #scale_fill_manual(values = c("lightgray", "#2C3E50")) +
      coord_polar(theta = "y") +
      theme_void()+
      ggtitle("BPM", subtitle = "Box Plus-Minus") +
      theme(plot.title = element_text(size = 35, face="bold", hjust = 0.5, margin = margin(20,0,0,0,unit="pt")),
                                                         plot.subtitle = element_text(color = "darkgray", size = 20, hjust = 0.5))
    
    ggiraph(ggobj = donut_plot)
    
  })
  
  output$player1_WS <- renderGirafe({
    donut_data <- percPlayer_data()
    
    donut_plot <- ggplot(donut_data, aes(y = percWS, fill = Player)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = paste0(Player, ": ", formattable::percent(percWS),"<br>Win Share: ", WS)),
        width = 0.3,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = round(donut_data$WS[donut_data$percWS == max(donut_data$percWS)],1),
        size = 20,
        color = "#2C3E50"
      ) +
      #scale_fill_manual(values = c("lightgray", "#2C3E50")) +
      coord_polar(theta = "y") +
      theme_void()+
      ggtitle("WS", subtitle = "Win Shares") +
      theme(plot.title = element_text(size = 35, face="bold", hjust = 0.5, margin = margin(20,0,0,0,unit="pt")),
            plot.subtitle = element_text(color = "darkgray", size = 20, hjust = 0.5))
    
    ggiraph(ggobj = donut_plot)
    
  })
  
  
  linePlayer_data <- reactive(
    playerData() %>% 
      dplyr::select(Player, Season, y = input$var_type)
    
  )
  
  output$linePerPlayer <- renderPlotly({
    ggplot(linePlayer_data(), aes(x=Season, y=y, text=paste(y), group=Player , color=Player)) +
      geom_line() +
      geom_point() +
      geom_hline(yintercept = mean(df_percentplayers[[input$var_type]]), linetype='dotted') + 
      xlab("Season") +
      ylab(input$var_type) + 
      theme_classic()
    
  })
  
  
}

