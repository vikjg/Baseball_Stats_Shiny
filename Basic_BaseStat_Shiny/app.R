# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Two-Game Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Game 1", tabName = "game1"),
      menuItem("Game 2", tabName = "game2"),
      menuItem("Pitch Percentages", tabName = "pitchp")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "game1",
        fluidRow(
          selectInput("team", label = "Choose Game 1 Stats",
                       choices = c("Away Team Batting", "Away Team Pitching",
                                      "Home Team Batting", "Home Team Pitching"),
                      selected = "Away Team Batting"),
                       
          fluidRow(
            DTOutput("selected_g1")
          )
        )
      ),
      tabItem(
        tabName = "game2",
        fluidRow(
          selectInput("teamg2", label = "Choose Game 2 Stats",
                      choices = c("Home Team Batting", "Home Team Pitching",
                                  "Away Team Batting", "Away Team Pitching"),
                      selected = "Home Team Batting"),
          
          fluidRow(
            DTOutput("selected_g2")
          )
        )
      ),
      tabItem(
        tabName = "pitchp",
        fluidRow(
          selectInput("pitch", label = "Choose Pitching Stats",
                      choices = c("Game 1: Home Team", "Game 2: Home Team",
                                  "Game 1: Away Team", "Game 2:Away Team"),
                      selected = "Game 1: Home Team"),
          
          fluidRow(
            DTOutput("selected_p"),
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Load the CSV data
  pitch_data <- read.csv("PitchData.csv")
  
  source("pitcher_df_func.R")
  source("pitcher_perc.R")
  source("pitcher_basics.R")
  source("batter_df_func.R")
  source("batter_comp.R")
  source("pitch_pie.R")
  
  #Game 1 Pitcher Data
  t1g1p <- pitcher_df(pitch_data, "0", "1")
  t2g1p <- pitcher_df(pitch_data, "1", "1")
  t1g1pb <- pitcher_basics(pitch_data, "0", "1")
  t2g1pb <- pitcher_basics(pitch_data, "1", "1")
  t1g1perc <- pitcher_perc(t1g1p, t1g1pb)
  t2g1perc <- pitcher_perc(t2g1p, t2g1pb)
  pie_t1g1 <- pitch_pie(t1g1perc)
  pie_t2g1 <- pitch_pie(t2g1perc)
  
  #Game 2 Pitcher Data
  t1g2p <- pitcher_df(pitch_data, "1", "2")
  t2g2p <- pitcher_df(pitch_data, "0", "2")
  t1g2pb <- pitcher_basics(pitch_data, "1", "2")
  t2g2pb <- pitcher_basics(pitch_data, "0", "2")
  t1g2perc <- pitcher_perc(t1g2p, t1g2pb)
  t2g2perc <- pitcher_perc(t2g2p, t2g2pb)
  pie_t1g2 <- pitch_pie(t1g2perc)
  pie_t2g2 <- pitch_pie(t2g2perc)
  
  #Game 1 Batter Data
  t1g1o <- batter_df(pitch_data, "1", "1")
  t2g1o <- batter_df(pitch_data, "0", "1")
  t1g1comp <- batter_comp(t1g1o)
  t2g1comp <- batter_comp(t2g1o)
  
  #Game 2 Batter Data
  t1g2o <- batter_df(pitch_data, "0", "2")
  t2g2o <- batter_df(pitch_data, "1", "2")
  t1g2comp <- batter_comp(t1g2o)
  t2g2comp <- batter_comp(t2g2o)
  
  # Render tables in each tab
  output$selected_g1 <- renderDT({
    team <- switch(input$team,
      "Away Team Batting" = t1g1comp,
      "Home Team Batting" = t2g1comp,
      "Away Team Pitching" = t1g1pb,
      "Home Team Pitching" = t2g1pb)
      datatable(team, options = list(autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
})
  
  output$selected_g2 <- renderDT({
    teamg2 <- switch(input$teamg2,
                   "Home Team Batting" = t1g2comp,
                   "Away Team Batting" = t2g2comp,
                   "Home Team Pitching" = t1g2pb,
                   "Away Team Pitching" = t2g2pb)
    datatable(teamg2, options = list(autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
  
  output$selected_p <- renderDT({
    pitch <- switch(input$pitch,
                     "Game 1: Home Team" = t2g1perc,
                     "Game 1: Away Team" = t1g1perc,
                     "Game 2: Home Team" = t1g2perc,
                     "Game 2:Away Team" = t2g2perc)
    datatable(pitch, options = list(autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
  
  
}

# Run the Shiny app
shinyApp(ui, server)
  

