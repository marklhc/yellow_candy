#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Yellow Candy Example"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("priors", "Prior", 
                  c("Prior 1 (Flat)" = 1, 
                    "Prior 2 (Weak)" = 2, 
                    "Prior 3 (Strong)" = 3)), 
      actionButton("draw", "Draw a candy!"), 
      actionButton("draw2", "Draw 10 pieces of candy!"), 
      textOutput("newcandy"), 
      textOutput("total"), 
      textOutput("yellowcandy")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("priorPlot", height = "300px"), 
      plotOutput("likliPlot", height = "300px"), 
      plotOutput("posteriorPlot", height = "300px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  v <- reactiveValues(data = NULL)
  a <- reactive({
    switch(input$priors, 
           "1" = 1, 
           "2" = 1.1, 
           "3" = 5.1)
  })
  b <- reactive({
    switch(input$priors, 
           "1" = 1, 
           "2" = 2, 
           "3" = 8)
  })
  observeEvent(input$draw, {
    v$data <- c(v$data, sample(c("red", "blue", "green", "black", "yellow"), 1, 
                               prob = c(.3, .2, .28, .15, .07)))
  })
  observeEvent(input$draw2, {
    v$data <- c(v$data, sample(c("red", "blue", "green", "black", "yellow"), 10, 
                               prob = c(.3, .2, .28, .15, .07), 
                               replace = TRUE))
  })
  observeEvent(input$priors, {
    v$data <- NULL
  })
  output$newcandy <- renderText(paste0("Last Candy: ", v$data[length(v$data)]))
  output$total <- renderText(paste0("Total # of Candy: ", length(v$data)))
  output$yellowcandy <- renderText(paste0("# of 'Yellow' Candy: ", 
                                          sum(v$data == "yellow")))
  output$priorPlot <- renderPlot({
    # generate plot for the prior
    par(mar = c(4, 4, 2, 0) + 0.1)
    curve(dbeta(x, a(), b()), from = 0, to = 1, 
          xlab = expression(theta), ylab = "", lwd = 2, lty = "dashed", 
          ylim = c(0, 4.5), col = "green3", n = 237, 
          main = "Prior")
    abline(v = 0.2, lty = "dotted")
  })
  
  output$likliPlot <- renderPlot({
    # generate plot for the likelihood
    par(mar = c(4, 4, 2, 0) + 0.1)
    curve(dbinom(sum(v$data == "yellow"), length(v$data), x) / 
            integrate(function(x) dbinom(1, 10, x), 0, 1)$value, 
          from = 0, to = 1, lwd = 2, lty = "twodash", col = "red", 
          ylab = "", xlab = expression(theta), 
          main = "Likelihood")
  })
  
  output$posteriorPlot <- renderPlot({
    # generate plot for the likelihood
    par(mar = c(4, 4, 2, 0) + 0.1)
    curve(dbeta(x, a() + sum(v$data == "yellow"), 
                b() + sum(v$data != "yellow")), from = 0, to = 1, 
          lwd = 2, col = "blue", ylab = "", xlab = expression(theta), 
          main = "Posterior")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

