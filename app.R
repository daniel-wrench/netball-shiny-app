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

# Load data
netball_data <- read_csv("ANZ_Premiership_2017_2020.csv")
head(netball_data)
nrow(netball_data)

# Bar plot of totals per team
team_summary <- netball_data %>% 
    group_by(Team) %>%
    summarise("Wins" = sum(W), "Bonus points" = sum(BP))

team_summary_long <- team_summary %>%
    pivot_longer(cols = c("Wins", "Bonus points"), names_to = "Statistic", values_to = "Total")

# Line plot of points per team over time
ggplot(data = netball_data, aes(x = Year, y = Pts, colour = Team)) + geom_line()

# Scatter plot of goals for vs. goals against
ggplot(data = netball_data, aes(x = GA, y = GF, colour = Team)) + geom_point()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ANZ Premiership Data"),
    
    checkboxGroupInput("teams", "Teams to plot", team_summary$Team),

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
        
        ggplot(data=selected_data, aes(x = Team, y = Total, fill = Statistic)) + geom_col(position = "dodge")
    })
    
    output$lineplot <- renderPlot({
        selected_data <- netball_data %>% filter(Team %in% input$teams)

        ggplot(data=selected_data, aes(x = Year, y = Pts, colour = Team)) + 
            geom_line()
    })
    
    output$scatterplot <- renderPlot({
        selected_data <- netball_data %>% filter(Year %in% input$years)
        
        ggplot(data = selected_data, aes(x = GA, y = GF, colour = Team)) + 
            geom_point() +
            labs(title = "Goals for vs. goals against, by team and year",
                 x = "Goals against",
                 y = "Goals for")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
