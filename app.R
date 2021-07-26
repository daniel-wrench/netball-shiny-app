#
# DATA471: Assignment 2 Part 2
# Netball data Shiny app
#
# NEXT STEPS:
# Improve appearance and layout of app
#
# ISSUES:
#
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)

theme_set(theme_bw(base_size = 20))

# Load data
netball_data <- read_csv("ANZ_Premiership_2017_2020.csv")
head(netball_data)
nrow(netball_data)

# Bar plot of totals per team
team_summary <- netball_data |> 
    group_by(Team)  |> 
    summarise("Wins" = sum(W), "Bonus points" = sum(BP))

team_summary_long <- team_summary %>%
    pivot_longer(cols = c("Wins", "Bonus points"), names_to = "Statistic", values_to = "Total")

# Line plot of points per team over time
ggplot(data = netball_data, aes(x = Year, y = Pts, colour = Team)) + geom_line()

# Scatter plot of goals for vs. goals against
goals_summary <- netball_data |> 
    filter(Year %in% c(2017, 2018, 2019)) |> 
    group_by(Team)  |> 
    summarise("GF" = sum(GF), "GA" = sum(GA))

ggplot(data = goals_summary, aes(x = GA, y = GF, colour = Team, size = 10)) + 
    geom_point() + 
    labs(title = "Total goals for vs. total goals against, for each team, for given time period",
         x = "Total goals for",
         y = "Total goals against") +
    theme_bw()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ANZ Premiership Data"),
    
    checkboxGroupInput("teams", "Teams to plot", team_summary$Team, selected = "Central Pulse"),

    plotOutput("barplot"),
    
    plotOutput("lineplot"),

    sliderInput("years",
               "Year range:",
               min = 2017,
               max = 2020,
               value = c(2017,2020),
               step = 1,
               sep = "",
               width = '20%'
    ),
    
    plotOutput("scatterplot")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$barplot <- renderPlot({
        selected_data <- team_summary_long %>% filter(Team %in% input$teams)
        
        ggplot(data=selected_data, aes(x = Team, y = Total, fill = Statistic)) + 
            geom_col(position = "dodge") + 
            scale_fill_manual(values=c("#999999", "#E69F00"))
    })
    
    output$lineplot <- renderPlot({
        selected_data <- netball_data %>% filter(Team %in% input$teams)

        ggplot(data=selected_data, aes(x = Year, y = Pts, colour = Team)) + 
            geom_line(size = 2) + labs(title = "Total season points over time, by team")
    })
    
    output$scatterplot <- renderPlot({

        goals_summary <- netball_data |> 
            filter(Year %in% input$years) |> 
            group_by(Team)  |> 
            summarise("GF" = sum(GF), "GA" = sum(GA))
        
        ggplot(data = goals_summary, aes(x = GA, y = GF, colour = Team)) + 
            geom_point(size = 4, show.legend = FALSE) + 
            geom_text(aes(label=Team), vjust = "inward", hjust = "inward", size = 6) + 
            labs(title = "Total goals for and against, by team",
                 x = "Total goals for",
                 y = "Total goals against") + 
            theme(legend.position = "none")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
