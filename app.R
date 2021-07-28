#
# DATA471: Assignment 2 Part 2
# Netball data Shiny app
#
# NEXT STEPS:
# Improve layout of app
#
# ISSUES:
#
# Text labels in scatterplot overlap for some ranges, check_overlap argument 
# makes some disappear -> may need legend as well or instead
#
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://ggplot2.tidyverse.org/articles/articles/faq-axes.html?q=axis#label-placement

library(shiny)
library(tidyverse)
library(readr)

theme_set(theme_bw(base_size = 20))

# Load data
netball_data <- read_csv("ANZ_Premiership_2017_2020.csv")
head(netball_data)
nrow(netball_data)

# Summarise data for some plots
team_summary <- netball_data |> 
    group_by(Team)  |> 
    summarise("Wins" = sum(W), "Bonus points" = sum(BP))

team_summary_long <- team_summary %>%
    pivot_longer(cols = c("Wins", "Bonus points"), names_to = "Statistic", values_to = "Total")

# Define UI for application that draws a series of plots
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
               value = c(2018,2019),
               step = 1,
               sep = "",
               width = '20%'
    ),
    
    plotOutput("scatterplot")

)

# Define server logic required to draw the plots
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
            geom_line(size = 2) + ylim(c(0,50)) + labs(title = "Total season points over time, by team")
    })
    
    output$scatterplot <- renderPlot({
        
    years_to_plot <- seq(input$years[1], input$years[2])
    
        goals_summary <- netball_data |> 
            filter(Year %in% years_to_plot) |> 
            group_by(Team)  |> 
            summarise("GF" = sum(GF), "GA" = sum(GA))
        
        ggplot(data = goals_summary, aes(x = GA, y = GF, colour = Team)) + 
            geom_point(size = 4) +
            # show.legend = FALSE)
            geom_text(aes(label=Team), 
                      vjust = "inward", 
                      hjust = "inward", 
                      size = 6) + 
            #ylim(0, 3300) + xlim(0, 3500) +
            labs(title = "Total goals for and against, by team",
                 x = "Total goals for",
                 y = "Total goals against") + 
            theme(legend.position = "none")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
