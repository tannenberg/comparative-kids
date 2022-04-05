
library(shiny)
library(tidyverse)
library(lubridate)
library(rio)
library(highcharter)
library(viridis)
library(shinythemes)
library(shinyWidgets)

#import data from shared google doc
df <- rio::import("https://docs.google.com/spreadsheets/d/1NSPYkxTzIE7USvVa7MmhcOQPSDgfao17-6QFbSlAM-Q/edit?usp=sharing") %>%
  mutate(date = as_date(as.character(date)),
         age_days = ifelse(kid=="Marcus", day(days(ymd(date)) - days(ymd("1987-08-30"))),
                    ifelse(kid=="Julius", day(days(ymd(date)) - days(ymd("2015-07-12"))),
                    ifelse(kid=="Mio", day(days(ymd(date)) - days(ymd("2018-10-22"))),
                    #ifelse(kid=="Philip", day(days(ymd(date)) - days(ymd("1985-05-12"))),
                    #ifelse(kid=="Hilma", day(days(ymd(date)) - days(ymd("2021-10-20"))),
                                                       NA)))) %>% 
  arrange(., age_days)

#my_xlim <- df %>% group_by(kid) %>%
#  summarise(max = max(age_days, na.rm=T) + 50)

start_value <-  df %>% 
  filter(kid=="Julius") %>%
  summarise(max(age_days, na.rm=T))

start_value <- start_value[1,1]
start_value <- plyr::round_any(start_value, 100, f=ceiling)

colors <- viridisLite::viridis(3)

slide_col <- viridisLite::viridis(6)[3]

slider <-  sliderInput(inputId = "xmax",
                       label = "Adjust timeline (x-axis)",
                       min = 100, max = 3000,
                       value = start_value, step = 100,
                       sep = "", 
                       width = "100%")


# UI for app that 
ui <- fluidPage(
  fluidPage(
    titlePanel("Comparative Kids"),
    
    p("This app compares my childhood weight and height measures to those of my kids"), #, my brother and my niece
    
    setSliderColor(c(slide_col), c(1)),
    
    theme = shinytheme("paper"),
    fluidRow(column(10, offset = 1, slider)),
    fluidRow(column(10, offset = 1, highchartOutput("plot_weight"))),
    fluidRow(column(10, offset = 1, highchartOutput("plot_height")))
    
    )
  )


# Define server 

server <- function(input, output) {

   output$plot_weight <- renderHighchart({
      #
      df %>%
        filter(!is.na(weight)) %>%
        filter(age_days < input$xmax + 50) %>%
       
       hchart(., "spline", hcaes(age_days, weight/1000, group=kid)) %>% 
       hc_colors(colors) %>% 
       hc_xAxis(title= list(text ="Age (days)")) %>% 
       hc_yAxis(title= list(text ="Weight (kg)"))
       
      
     })
   
   output$plot_height <- renderHighchart({
     #
     df %>%
       filter(!is.na(height)) %>%
       filter(age_days < input$xmax + 50) %>%
       
       hchart(., "line", hcaes(age_days, height, group=kid)) %>% 
       hc_colors(colors) %>% 
       hc_xAxis(title= list(text ="Age (days)")) %>% 
       hc_yAxis(title= list(text ="Height (cm)"))
     
     
   })


}

# Run the application
shinyApp(ui = ui, server = server)
