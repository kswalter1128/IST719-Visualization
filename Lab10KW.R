#Lab 10 Kyle Walter

require(shiny)

server <- function(input, output){
  output$mypie <- renderPlot({
    pie(c(8,12,3), main = "Hello World")
  })
}

ui <- fluidPage(
  mainPanel(plotOutput("mypie"))
)

shinyApp(ui, server)