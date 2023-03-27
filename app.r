library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(magick)
library(dplyr)
library(shiny)
library(DT)

team_colors <- c("Purdue" = "#CEA531", "Fairleigh Dickinson" = "#7D1623", "Texas Southern" = "#800000",
                 "Memphis" = "#273BE2", "Florida Atlantic" = "#002D62", "Tennessee" = "#F77F00",
                 "Louisiana Lafayette" = "#D50032", "Duke" = "#012169", "Oral Roberts" = "#002855",
                 "Kansas St." = "#4F2683", "Montana St." = "#0C2340", "Kentucky" = "#0033A0",
                 "Providence" = "#000000", "Marquette" = "#0033A0", "Vermont" = "#006438",
                 "Michigan St." = "#18453B", "USC" = "#990000", "Houston" = "#C8102E",
                 "Northern Kentucky" = "#FFCC00", "Iowa" = "#FFCD00", "Auburn" = "#0C2340",
                 "Indiana" = "#990000", "Kent St." = "#002855", "Miami FL" = "#F47321",
                 "Drake" = "#002D62", "Xavier" = "#0C2340", "Kennesaw St." = "#FFCC00",
                 "Iowa St." = "#990000", "Mississippi St." = "#660000", "Pittsburgh" = "#003594",
                 "Texas" = "#BF5700", "Colgate" = "#800000", "Texas A&M" = "#500000", "Penn St." = "#0C2340",
                 "Alabama" = "#9E1B32", "Southeast Missouri St." = "#9D2235", "Texas A&M Corpus Chris" = "#003594",
                 "Maryland" = "#CE1126", "West Virginia" = "#FFC600", "Virginia" = "#232D4B",
                 "Furman" = "#4B0082", "San Diego St." = "#990000", "College of Charleston" = "#800000",
                 "Baylor" = "#2C5E4F", "UC Santa Barbara" = "#002663", "Creighton" = "#002D62",
                 "North Carolina St." = "#CC0000", "Arizona" = "#CC0033", "Princeton" = "#FF8C00",
                 "Missouri" = "#F1B82D", "Utah St." = "#003DA5", "Kansas" = "#0051BA",
                 "Howard" = "#002D62", "Arkansas" = "#9D2235", "Illinois" = "#E84A27",
                 "Connecticut" = "#0C2340", "Iona" = "#800000", "Saint Mary's" = "#990000",
                 "VCU" = "#FFCC00", "Gonzaga" = "#BA0C2F", "Grand Canyon" = "#46166B",
                 "TCU" = "#4D1979", "Arizona St." = "#660021", "Nevada" = "#001b39", 
                 "UCLA" = "#2774AE", "UNC Asheville" = "#7BAFD4", "Northwestern" = "#4E2A84", "Boise St." = "#0033A0")

df_team_data <- read.csv("brosius data/Tournament Team Data (Including 2023).csv") %>%
  rename_with(~ gsub("..", "", .x, fixed = TRUE))

plot_theme <- ggdark::dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", linewidth = 0.2),
        panel.grid.minor = element_line(color = "grey30", linewidth = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

# Data Frames for matchup predictions
# Universal key indicates who fights who for any given round
df_keys <- read.csv("brosius data/round predictions.csv")

df_first_round <- read.csv('Data/firstRound.csv') %>%
  mutate(UNIVERSAL.KEY = GAME.KEY)

df_second_round <- read.csv('Data/secondRound.csv') %>%
  mutate(UNIVERSAL.KEY = MATCHUP.KEY)

df_third_round <- read.csv('Data/thirdRound.csv') %>%
  mutate(UNIVERSAL.KEY = as.factor(left_join(., df_keys, by = "TEAM")[["SWEET.16"]])) %>%
  group_by(REGION) %>%
  mutate(UNIVERSAL.KEY = c("A", "A", "B", "B"))

df_fourth_round <- read.csv('Data/fourthRound.csv') %>%
  mutate(UNIVERSAL.KEY = "A")

df_fifth_round <- read.csv('Data/fifthRound.csv') %>%
  mutate(UNIVERSAL.KEY = "A")

df_sixth_round <- read.csv('Data/sixthRound.csv') %>%
   mutate(UNIVERSAL.KEY = "A")

df_national_champion <- read.csv('Data/nationalChampion.csv') %>%
   mutate(UNIVERSAL.KEY = "A")

# Possible dataframe for showing teams probability regardless of matchups
# df_predictions <- read.csv("Data/predictionScores.csv")

column_names<-colnames(df_team_data) #for input selections 
ui<-fluidPage(
  theme = shinytheme("darkly"),

  # Sets text elements of data.table since the RStudio browser 
  # uses old version of the web kit
  tags$script(HTML({"
    $(document).ready(function() {
      var isRStudio = /rstudio/.test(navigator.userAgent.toLowerCase());
      if (isRStudio) {
        $('<style>.dataTables_wrapper .dataTables_info { color: white !important; }</style>').appendTo('head');
      }
    });
  "})),

  # Custom CSS for the DataTable to change font color
  # I learned CSS for this...
  tags$style(HTML({"
    .dataTables_wrapper {
      color: white;
    }
    table.dataTable thead th,
    table.dataTable tfoot th,
    .dataTables_filter label,
    .dataTables_length label{
      color: white;
      font-weight: bold;
    }
    .dataTables_wrapper .dataTables_info {  # Updated selector with higher specificity
    color: white;
    font-weight: bold;
    }
    .dataTables_paginate .paginate_button {
      background-color: #3c3c3c;
      color: white;
      border: none;
    }
    .dataTables_paginate .paginate_button.current,
    .dataTables_paginate .paginate_button:hover {
      background-color: #565656;
      color: white;
      border: none;
    }
    .dataTables_paginate .paginate_button.disabled {
      background-color: #3c3c3c;
      color: #999999;
      border: none;
      cursor: not-allowed;
    }
    table.dataTable thead tr {
      background-color: #3c3c3c;
    }
  "})),
  
  titlePanel(title = "March Madness 2023 Predictions and Metrics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("round",
                  'Choose a round:',
                  choices = c("First Round", "Second Round", "Sweet 16", "Elite 8", 
                              "Final Four", "Championship Game", "National Champion")),
    
      selectInput("team1",
                "Choose a team:",
                choices = distinct(df_team_data, df_team_data$TEAM),
                selected = "Alabama"),
      
      selectInput("team2",
                  "Choose a team:",
                  choices = distinct(df_team_data, df_team_data$TEAM),
                  selected = "Houston"),
      
      selectInput("metric",
                  "Choose a metric:",
                  choices = colnames(select(df_team_data, -YEAR, -SEED, -TEAM, -ROUND, -CHAMPION, -TEAM.1)),
                  selected = "KENPOM.ADJUSTED.OFFENSE")
    ),
    mainPanel(
      fluidRow(
        column(6,plotOutput('plot_01')),
        column(6,plotOutput('plot_02'))
      )
    )
  ),
  fluidRow(
    #Allows users to track our predictions throughout every round
    column(12, plotOutput('plot_03')),
    
    column(12, DTOutput("table_01"))
  )
)

server<-function(input,output){
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
    metric_string <- str_to_title(gsub("\\.", " ", input$metric))
    
    ggplot(df_filter_team01(), aes_string(x = "YEAR", y = input$metric)) +
    geom_line() +
    geom_smooth(color = team_colors[input$team1]) +
    ylim(60,130) +
    xlab("Season") +
    ylab(metric_string) +
    ggtitle(paste(input$team1, metric_string, sep = "'s ")) +
    plot_theme
  })
  
  output$plot_02 <- renderPlot({
    metric_string <- str_to_title(gsub("\\.", " ", input$metric))
    
    ggplot(df_filter_team02(), aes_string(x = "YEAR", y = input$metric)) +
      geom_line() +
      geom_smooth(color = team_colors[input$team2]) +
      ylim(60,130) +
      xlab("Season") +
      ylab(metric_string) +
      ggtitle(paste(input$team2, metric_string, sep = "'s ")) +
      plot_theme
  })
  
  #First plot and table in shiny just out of order in script
  #Plot shows what teams are left in the round and their prediction score of advancing
  #Clustering indicates match-ups, where observations that are clustered together are opponents
  output$plot_03 <- renderPlot({
    num_teams <- length(unique(selected_df()$TEAM))
    text_angle <- ifelse(num_teams <= 16, 0, 90)
    threshold <- mean(selected_df()$PREDICTIONS)
    
    ggplot(selected_df(), aes(x = UNIVERSAL.KEY, y = PREDICTIONS, fill = TEAM)) +
      geom_bar(show.legend = F, position = "dodge", stat = "identity") +
      scale_fill_manual(values = team_colors) +
      geom_text(aes(label = TEAM, y = ifelse(PREDICTIONS > threshold, PREDICTIONS / 2, max(PREDICTIONS)/2)), position = position_dodge(width = 0.9), size = 4.1, fontface = "bold", angle = text_angle) +
      facet_wrap(~REGION, scales = "free_x") +
      plot_theme +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  })
  
  #Table shows exact match-ups and denotes which team wins and is interactive so you can search any for any team
  #that is still in the tournament during that given round
  output$table_01<- renderDT(
                      selected_df() %>%
                        datatable() %>%
                        formatStyle(
                          columns = colnames(selected_df()),
                          backgroundColor = styleEqual(levels = c("All"), values = c("transparent")),
                          color = "white",
                          fontWeight = "bold"
                    )
                )
}
shinyApp(ui=ui, server=server)

