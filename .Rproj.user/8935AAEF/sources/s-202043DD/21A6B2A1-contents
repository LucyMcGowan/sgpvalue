library(shiny)

ui <- fluidPage(

   titlePanel("Power Curves"),

   sidebarLayout(
      sidebarPanel(
         numericInput("n",
                     "N",
                     value = 30),
      sliderInput("delta",
                  "delta",
                  min = 0,
                  max = 3,
                  step = 0.01,
                  value = 0.4)
      ),

      mainPanel(
         plotOutput("null_plot"),
         plotOutput("alt_plot"),
         plotOutput("inc_plot")
      )
   )
)

server <- function(input, output) {

   output$null_plot <- renderPlot({
    power_curves(n = input$n, delta = input$delta, truth = "null")
     title("Null")
   })
   output$alt_plot <- renderPlot({
     power_curves(n = input$n, delta = input$delta, truth = "alternative")
     title("Alternative")
   })
   output$inc_plot <- renderPlot({
     power_curves(n = input$n, delta = input$delta, truth = "inconclusive")
     title("Inconclusive")
   })
}

shinyApp(ui = ui, server = server)

