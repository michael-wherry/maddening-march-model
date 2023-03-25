library(tidyverse)
library(geosphere)
library(lubridate)
library(dplyr)
library(ggplot2)
library(e1071)
library(magrittr)
library(shiny)
library(magick)

df_team_data <- read.csv("brosius data/Tournament Team Data (Including 2023).csv")

#Data Frames for matchup predictions
df_first_round <- read.csv('Data/firstRound.csv')
df_second_round <- read.csv('Data/secondRound.csv')
df_third_round <- read.csv('Data/thirdRound.csv')
df_fourth_round <- read.csv('Data/fourthRound.csv')
df_fifth_round <- read.csv('Data/fifthRound.csv')
df_sixth_round <- read.csv('Data/sixthRound.csv')
df_national_champion <- read.csv('Data/nationalChampion.csv')

#Possible dataframe for showing teams probability regardless of matchups
#df_predictions <- read.csv("Data/predictionScores.csv")

#shinyApp
column_names<-colnames(df_team_data) #for input selections 
ui<-fluidPage( 
  
  titlePanel(title = "March Madness 2023 Predictions and Metrics"),
  fluidRow(
    column(2,
           selectInput("round",
                       'Choose a round:',
                       choices = c("First Round", "Second Round", "Sweet 16", "Elite 8", 
                                   "Final Four", "Championship Game", "National Champion"))),
    column(4,
           selectInput("team1",
                       "Choose a team:",
                       choices = c("All", distinct(df_team_data, df_team_data$TEAM)),
                       selected = "All")),
    column(6,
           selectInput("team2",
                       "Choose a team:",
                       choices = c("All", distinct(df_team_data, df_team_data$TEAM)),
                       selected = "All")),
    #Figure out how to make the metric selection reactive for graphs showing teams stats over past couple years
    #coulumn(8, 
          #  selectInput("metric",
                     #   "Choose a metric:",
                 #       )
      
  ),
  #Allows users to track our predictions throughout every round
  column(12, plotOutput('plot_05')),
  column(12,dataTableOutput("table_01")),
  #Allows users to compare metrics we used in our model
  column(5,plotOutput('plot_01')),
  column(5,plotOutput('plot_02')),
  column(5,plotOutput('plot_03')),
  column(5,plotOutput('plot_04'))
  )

server<-function(input,output){
  #didn't get this working
  plot_theme <- theme(legend.background = element_rect(fill = "#333333"),
                      legend.key = element_rect(fill = "#333333"),
                      legend.text = element_text(color = "white", size = rel(1.2)),
                      legend.title = element_text(color = "white", size = rel(1.5)),
                      axis.title = element_text(color = "white", size = rel(1.5)),
                      axis.text = element_text(color = "#BBBBBB", size = rel(1.0)),
                      panel.grid = element_line(color = "#555555"), 
                      panel.border = element_rect(fill = NA), 
                      panel.background = element_rect(fill = "#333333"), 
                      plot.background = element_rect(fill = "#333333"))
  
df_filter_team01 <- reactive({
  subset(df_team_data, TEAM == input$team1)
})
df_filter_team02 <- reactive({
  subset(df_team_data, TEAM == input$team2)
})
#Makes a reactive selection for dataframes so users can pick what round and results they want to see
selected_df <- reactive({
  switch(input$round,
         "First Round" = df_first_round,
         "Second Round" = df_second_round,
         "Sweet 16" = df_third_round,
         "Elite 8" = df_fourth_round,
         "Final Four" = df_fifth_round,
         "Championship Game" = df_sixth_round,
         "National Champion" = df_national_champion)
})
#Make the following 4 plots reactive based on user input of metric for y axis where right now it is hard set to kenpom stats
output$plot_01 <- renderPlot({
plot_data <- df_filter_team01() %>%
  ggplot(aes(x = YEAR, y = KENPOM.ADJUSTED.OFFENSE)) +
  geom_line() +
  geom_smooth() +
  ylim(60,130) +
  xlab("Season") +
  ylab("Offensive Efficiency") +
  ggtitle(paste0(input$team1, " Offensive Efficiency"))

plot_data
})
output$plot_02 <- renderPlot({
  plot_data <- df_filter_team01() %>%
    ggplot(aes(x = YEAR, y = KENPOM.ADJUSTED.DEFENSE)) +
    geom_line() +
    geom_smooth() +
    ylim(60,130) +
    xlab("Season") +
    ylab("Defensive Efficiency") +
    ggtitle(paste0(input$team1, " Defensive Efficiency"))
  
  plot_data
})
output$plot_03 <- renderPlot({
  plot_data <- df_filter_team02() %>%
    ggplot(aes(x = YEAR, y = KENPOM.ADJUSTED.OFFENSE)) +
    geom_line() +
    geom_smooth() +
    ylim(60,130) +
    xlab("Season") +
    ylab(" Offensive Efficiency") +
    ggtitle(paste0(input$team2, " Offensive Efficiency"))
  
  plot_data
})
output$plot_04 <- renderPlot({
  plot_data <- df_filter_team02() %>%
    ggplot(aes(x = YEAR, y = KENPOM.ADJUSTED.DEFENSE)) +
    geom_line() + 
    geom_smooth() +
    ylim(60,130) +
    xlab("Season") +
    ylab("Defensive Efficiency") +
    ggtitle(paste0(input$team2, " Defensive Efficiency"))
  
  plot_data
})
#First plot and table in shiny just out of order in script
#Plot shows what teams are left in the round and their prediction score of advancing
#geom_text used since the team names are too long for axis ticks.  Still overlap for round one selection
#but after round one it looks good.  Still need to make columns color related to team colors.
output$plot_05 <- renderPlot({
  plot_data <- selected_df() %>%
    ggplot(aes(x = TEAM, y = PREDICTIONS, fill = TEAM)) +
    geom_col(show.legend = F) +
    geom_text(aes(x = TEAM, y = PREDICTIONS, label = TEAM)) +
    facet_wrap(~REGION) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  plot_data
})
#Table shows exact matchups and denotes which team wins and is interactive so you can search any for any team
#that is still in the tournament during that given round
output$table_01<- renderDataTable(selected_df(), callback = JS(
  "table.on( 'search.dt', function () {",
  "Shiny.setInputValue( 'search', table.search() );",
  "} );"
))
}
shinyApp(ui=ui, server=server)

