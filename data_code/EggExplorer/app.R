# Kingston Home Egg Explorer - 2020

library(shiny)
#library(feather)
library(ggplot2)
library(lubridate)
library(tidyverse)
#library(openair)
library(shinythemes)
library(gghighlight)

# Prepping the data file --------------------------------------------

webmasterk<- read_csv("big_egg2.csv") 

# %>%
#   select( #selects certain variables from dataset
#     timestamp_local, pm25, pm10
#   ) %>%

webmasterk <- webmasterk %>%
  rename( # Renames them so that they display nicely in plots
    YMD = time
  )



# Define UI for application ---------------------------------------
ui <- fluidPage(
    # Styling
    theme = shinytheme("darkly"),
    #for shinyapps.io verion:
    #tags$head(includeCSS("app.css")),
    #tags$head(includeCSS("/Users/owenoconnor/Documents/College/Spring2021/CSC_132/FinalProject/Code/finalproj/www/app.css")),
    
    # tags$script(src = "app.js"),
    
    # Application title
    titlePanel( div(column(width = 4, tags$a(href="https://landairwater.bard.edu/projects/kaqi/", tags$img(src = "bcslaw-logo.png", height = 50, width = 400))),
                    column(width = 8, h2("Kingston NY Particulate Matter"))
    ),
    windowTitle="Kingston Particulate Matter"
    ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("To explore the data from the KAQI home Air Quality Eggs
                select the variables and the date range that you would like to display.",tags$br(),tags$br(),
),
            
            dateRangeInput("dateRange1", "Date range:",
                           start = "2020-03-15",
                           end   = "2020-12-31",
                           min = "2020-03-15",
                           max = "2020-12-31"),
            checkboxGroupInput("variable", "Observations to display:",
                               c("pm2p5.house1out" = "pm2p5.house1out",
                                 "pm2p5.house2out" = "pm2p5.house2out",
                                 "pm2p5.house1in" = "pm2p5.house1in",
                                 "pm2p5.house2in" = "pm2p5.house2in",
                                 "pm2p5.house3in" = "pm2p5.house3in"
                               ),
                               selected = "pm2p5.house1out"
            ),
selectInput("highlight", label = ("Burn observations to highlight:"),
            choices = list("none" = 1, "outdoorburn.house2" = 2, "indoorburn.house1" = 3, "outdoorburn.house1" = 4),
            selected = 1),

        ), # End Sidebar Panel

        # Main display on right hand side
        mainPanel(
            plotOutput("pmPlot"), verbatimTextOutput("summary")
        ) # End of mainPanel
    ) # End of sideBarLayout
) # End of fluidPage


# Define server ---------------------------------------------------
server <- function(input, output) {
    
    # Subsets dataset based on user daterange and variable selections
    data_1 <-  reactive({subset(webmasterk, webmasterk$YMD >= ymd(input$dateRange1[1]) & webmasterk$YMD <= ymd(input$dateRange1[2]) ) %>%
            select(
                YMD,
                outdoorburn.house2,
               indoorburn.house1,
               outdoorburn.house1,
                c(input$variable)
            ) })
    

    output$summary <- renderPrint({
        dataset <- data_1() %>%
            select(
                -YMD,
                -outdoorburn.house2,
               -indoorburn.house1,
               -outdoorburn.house1
            )
        summary(dataset)
    })
    
    output$pmPlot <- renderPlot({
        # Displays gentle error message if no variables are selected in checkbox
        #  validate( 
        #    need(input$variable != "", "Please select at least one variable to display")
        #   )
        
        # Time series point chart displaying data that user selects
        
        if (input$highlight == 1) {
        
        data_1() %>%
            pivot_longer(starts_with("PM"), names_to = "Household", values_to = "observation") %>%
            ggplot(aes(x = YMD, y = observation, color = `Household`) ) +
            geom_point() +
            labs(
                y = expression(Mass - (μg/~m^3)),
                x = NULL,
                title = paste(
                    "PM2.5 Raw Egg Readings"
                ) ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5) ) +
            theme(plot.subtitle = element_text(hjust = 0.5) )
        }
        else if (input$highlight == 2) {
            
            data_1() %>%
                pivot_longer(starts_with("PM"), names_to = "Household", values_to = "observation") %>%
                ggplot(aes(x = YMD, y = observation, color = `Household`) ) +
                geom_point() +
                gghighlight(
                    outdoorburn.house2 > 0, use_direct_label = FALSE
                ) +
                labs(
                    y = expression(Mass - (μg/~m^3)),
                    x = NULL,
                    title = paste(
                        "PM2.5 Raw Egg Readings"
                    ) ) +
                theme_classic() +
                theme(plot.title = element_text(hjust = 0.5) ) +
                theme(plot.subtitle = element_text(hjust = 0.5) )
            
        }
        else if (input$highlight == 3) {
            
            data_1() %>%
                pivot_longer(starts_with("PM"), names_to = "Household", values_to = "observation") %>%
                ggplot(aes(x = YMD, y = observation, color = `Household`) ) +
                geom_point() + 
                gghighlight(
                    indoorburn.house1 > 0, use_direct_label = FALSE
                ) +
                labs(
                    y = expression(Mass - (μg/~m^3)),
                    x = NULL,
                    title = paste(
                        "PM2.5 Raw Egg Readings"
                    ) ) +
                theme_classic() +
                theme(plot.title = element_text(hjust = 0.5) ) +
                theme(plot.subtitle = element_text(hjust = 0.5) )
            
        }
        
        else if (input$highlight == 4) {
            
            data_1() %>%
                pivot_longer(starts_with("PM"), names_to = "Household", values_to = "observation") %>%
                ggplot(aes(x = YMD, y = observation, color = `Household`) ) +
                geom_point() + 
                gghighlight(
                    outdoorburn.house1 > 0, use_direct_label = FALSE
                ) +
                labs(
                    y = expression(Mass - (μg/~m^3)),
                    x = NULL,
                    title = paste(
                        "PM2.5 Raw Egg Readings"
                    ) ) +
                theme_classic() +
                theme(plot.title = element_text(hjust = 0.5) ) +
                theme(plot.subtitle = element_text(hjust = 0.5) )
            
        }
        
    }) # End of renderPlot
    # output$variableTest <- renderText(paste(input$variable, collapse = ", ") )
    
    
} # End of server

# Runs the application --------------------------------------------
shinyApp(ui = ui, server = server)