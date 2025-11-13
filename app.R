library(shiny)
library(ggplot2)
library(scales)
source("power_fun.R")

ui <- fluidPage(
  sliderInput("Power_level", "Power Level", min = .05, max = .95, step = .05, value = c(.5, .95)),
  sliderInput("Cohens_d", "Cohen's D", min = .05, max = 1, step = .05, value = c(.5, 1)),
  numericInput("alpha", "Significance Level", min = .01, max = 1, step = .01, value = .05),
  plotOutput("Power_plot")
)

server <- function(input, output, session) {
  Power_data <- reactive({
    power_fun(x = input$Power_level[1],
              y = input$Power_level[2],
              w = input$Cohens_d[1],
              z = input$Cohens_d[2],
              a = input$alpha)
  })
  
  output$Power_plot <- renderPlot({ggplot(Power_data(), 
        aes(power, n, group = d, color = d)) + geom_line() + scale_y_continuous(n.breaks = 15) +
      scale_x_continuous(n.breaks = 10) + 
      labs(title = "Power~Sample Relation with Varying D values", x = "Power", y = "Sample Size")
  })
}

shinyApp(ui, server)

