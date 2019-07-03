
library(shiny)
library(tidyverse)
library(lubridate)
library(rio)
library(hrbrthemes)

df <- rio::import("https://docs.google.com/spreadsheets/d/1NSPYkxTzIE7USvVa7MmhcOQPSDgfao17-6QFbSlAM-Q/edit?usp=sharing") %>%
  mutate(date = as_date(as.character(date)),
         age_days = ifelse(kid=="Marcus", day(days(ymd(date)) - days(ymd("1987-08-30"))),
                           ifelse(kid=="Julius", day(days(ymd(date)) - days(ymd("2015-07-12"))),
                                  ifelse(kid=="Mio", day(days(ymd(date)) - days(ymd("2018-10-22"))), NA))))

my_xlim <- df %>% group_by(kid) %>%
  summarise(max = max(age_days, na.rm=T) + 50)

start_value <-  df %>% 
  filter(kid=="Mio") %>%
  summarise(max(age_days, na.rm=T))

start_value <- start_value[1,1]
start_value <- plyr::round_any(start_value, 100, f=ceiling)


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Comparative Kids"),
  p("This app compares my childhood weight and height measures to those of my kids."),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      #selectInput(inputId = "xmax",
      #            label = "Adjust X-axis for last measurement of:",
      #            choices = c("Julius" = 1500,
      #                        "Mio" = 250,
      #                        "Marcus" = 2500))
      #

      sliderInput(inputId = "xmax",
                  label = "Adjust timeline (x-axis)",
                  min = 100, max = 2500,
                  value = start_value, step = 100,
                  sep = "" )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "plot_weight", width = "100%", height = "400px"),
      plotOutput(outputId = "plot_height", width = "100%", height = "400px")

    )
  )
)

# Define server logic required to draw a histogram

server <- function(input, output) {

   output$plot_weight <- renderPlot({
      #
      df %>%
        filter(!is.na(weight)) %>%
        filter(age_days < input$xmax + 50) %>%
        ggplot(aes(x=age_days, y = weight/1000, color=kid)) +
        geom_line(size=2, alpha=.7) +
        geom_point(size=2) +
        xlim(0, input$xmax) +
        #ylim(2.5, 16) +
        scale_color_viridis_d() +
        theme_ipsum(base_size = 14, axis_title_size = 16) +
        theme(legend.title=element_text(size=16),
             legend.text=element_text(size=16),
             legend.position = "top") +
        labs(x="Age (days)", y="Weight (kg)", color="")

     })

     output$plot_height <- renderPlot({
       #
       df %>%
         filter(!is.na(height)) %>%
         filter(age_days < input$xmax + 50) %>%
         ggplot(aes(x=age_days, y = height, color=kid)) +
         geom_line(size=2, alpha=.7) +
         geom_point(size=2) +
         xlim(-1, input$xmax) +
        #ylim(40, 110) +
         scale_color_viridis_d() +
         theme_ipsum(base_size = 14, axis_title_size = 16) +
         theme(legend.title=element_text(size=16),
               legend.text=element_text(size=16),
               legend.position = "none") +
         labs(x="Age (days)", y="Height (cm)", color="")

   })
}

# Run the application
shinyApp(ui = ui, server = server)
