library(tidyverse)
library(geosphere)
library(lubridate)
library(dplyr)
library(ggplot2)
library(e1071)
library(magrittr)
library(shiny)

#df_team_data <- read.csv("brosius data/Tournament Team Data (Including 2023).csv")

#shinyApp
column_names<-colnames(df_team_data) #for input selections 
ui<-fluidPage( 
  
  titlePanel(title = "March Madness 2023 Team Statistics"),
  fluidRow(
    column(2,
           selectInput("team1",
                       "Choose a team:",
                       choices = c("All", distinct(df_team_data, df_team_data$TEAM)),
                       selected = "All")),
    column(4,
           selectInput("team2",
                       "Choose a team:",
                       choices = c("All", distinct(df_team_data, df_team_data$TEAM)),
                       selected = "All")),
    column(6,
           selectInput("metric",
                       "Choose a metric:",
                       choices = colnames(select(df_team_data))),
  ),
  column(5,plotOutput('plot_01')),
  column(5,plotOutput('plot_02')),
  column(5,plotOutput('plot_03')),
  column(5,plotOutput('plot_04')),
  column(3,dataTableOutput("table_01")),
  column(3,dataTableOutput("table_02"))
  ))

server<-function(input,output){
df_filter_team01 <- reactive({
  subset(df_team_data, TEAM == input$team1)
})
df_filter_team02 <- reactive({
  subset(df_team_data, TEAM == input$team2)
})
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
  output$table_01<- renderDataTable(df_filter_team01())
  #output$table_02<- renderDataTable(df_filter_team02())
}
shinyApp(ui=ui, server=server)

