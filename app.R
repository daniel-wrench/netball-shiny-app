#
# DATA471: Assignment 2 Part 2
# Netball data Shiny app
#
# NEXT STEPS:
# Insert plots into Shiny app 
# Add reactivity
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

ggplot(data=team_summary_long, aes(x = Team, y = Total, fill = Statistic)) + geom_col(position = "dodge")

# Line plot of points per team over time
ggplot(data=netball_data, aes(x = Year, y = Pts, colour = Team)) + geom_line()

# Scatter plot of goals for vs. goals against
ggplot(data = netball_data, aes(x = GA, y = GF, colour = Team)) + geom_point()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ANZ Premiership Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scorePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$scorePlot <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
