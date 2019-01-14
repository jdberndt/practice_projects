#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Playing with logs"),
   img(src="logs.png"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("base",
                     "base of log:",
                     min = 0,
                     max = round(exp(1),2),
                     value = round(exp(1),2),
                     step = 0.1),
         sliderInput("xvals",
                     "values of x",
                     min = 0,
                     max = 1000,
                     value = c(1, 100))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("scatterPlot", width = 800, brush = "plot_brush"),
                 
        # Output: Table summarizing the values entered ----
        DTOutput("values", width = 400)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    value <- reactive({
        base = input$base
        x <- input$xvals[1]:input$xvals[2]
        y <- log(x, base)
        data.frame(x,y,base)
    })    

   output$scatterPlot <- renderPlot(
      ggplot(value(), aes(x, y)) + 
                   geom_point()
   )   
      # Show the values in an HTML table ----
   output$values <- renderDT(
        brushedPoints(value(), input$plot_brush), 
        extensions = 'Buttons', 
        options = list(
                   dom = 'ltrfipB',
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
           )
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

