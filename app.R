#
# DATA471: Assignment 2 Part 2
# Netball data Shiny app
#
# NEXT STEPS:
#
# ISSUES:
#
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://ggplot2.tidyverse.org/articles/articles/faq-axes.html?q=axis#label-placement

library(shiny)
library(tidyverse)
library(readr)

theme_set(theme_light(base_size = 15))

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
    titlePanel("ANZ Netball Premiership Data"),
    
    fluidRow(
        column(12, "This app displays statistics from the 2017-2020 seasons for the ANZ Premiership Netball league. Interact with the options to customise the plots."),
        column(12, h4("View statistics for specific teams")),
        column(4,
            checkboxGroupInput("teams", 
                               "Teams to plot", 
                               team_summary$Team, 
                               selected = "Central Pulse")
        ),
        column(4,plotOutput("barplot")),
        column(4,plotOutput("lineplot"))
        ),
    fluidRow(
        column(12, h4("View statistics for a specific range of years")),
        column(4,
            sliderInput("years",
                        "Year range:",
                        min = 2017,
                        max = 2020,
                        value = c(2018,2019),
                        step = 1,
                        sep = ""
                        #width = '20%'
            )),
        column(8,
                     plotOutput("scatterplot"))
            )
)

# Define server logic required to draw the plots
server <- function(input, output) {
    
    output$barplot <- renderPlot({
        selected_data <- team_summary_long %>% filter(Team %in% input$teams)
        
        ggplot(data=selected_data, aes(x = Team, y = Total, fill = Statistic)) + 
            geom_col(position = "dodge") + 
            labs(title = "Total bonus points \nand wins, 2017-2020") +
            scale_fill_manual(values=c("#999999", "#E69F00")) + 
            theme(legend.position = "bottom", 
                  axis.text.x = element_text(angle = 18),
                  legend.title = element_blank())
    })
    
    output$lineplot <- renderPlot({
        selected_data <- netball_data %>% filter(Team %in% input$teams)

        ggplot(data=selected_data, aes(x = Year, y = Pts, colour = Team)) + 
            geom_line(size = 2) + ylim(c(0,50)) + 
            labs(title = "Total points by year") + 
            theme(legend.position = "bottom", legend.title = element_blank()) + 
            guides(colour = guide_legend(nrow=3))
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
            labs(title = "Total goals for and against, for selected period",
                 x = "Total goals for",
                 y = "Total goals against") + 
            theme(legend.position = "none")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
