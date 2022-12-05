library(tidyverse)
library(shiny)
library(scales)
library(knitr)

getwd()
setwd("C:/Users/17372/Desktop/Files/SDS 313/Week 14")
data <- read_csv("movieData.csv")

ui <- fluidPage(
  titlePanel("Project 3"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("selectvar", label = h3("Choose a variable"), choices = list("Genres" = 1, "Adult Titles" = 2, "Ratings" = 3, "Format Types" = 4, "Release Years" = 5), selected = 1),
      p("Slider only works for ratings and release years"),
      sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 50, value = 30),
      p("Slider only works for ratings"),
      sliderInput(inputId = "range", label = "Ratings Slider:", min = 0, max = 10, value = c(0, 10)),
      p("Slider only works for release years"),
      sliderInput(inputId = "range2", label = "Release Years Slider", min = 1893, max = 2022, value = c(1893, 2022)),
      checkboxInput("checkbox", label = "Display descriptive statistics", value = FALSE),
      radioButtons("selectcolor", label = h3("Choose a color"), choices = list("Gray", "Red", "Blue", "Green", "Yellow", "Orange", "Purple"))
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot"),
      hr(),
      div(imageOutput("image"), style = "margin-left: 300px; position: absolute; border-style: solid; border-width: 10px; border-radius: 10px"),
      p("Descriptive statistics:"),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      fluidRow(column(5, verbatimTextOutput("sd"))),
      fluidRow(column(5, verbatimTextOutput("fivenum"))),
      fluidRow(column(5, verbatimTextOutput("table"))),
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    if(input$selectvar == 1) {
      ggplot(data) + 
        geom_bar(aes(x = Genre), fill = input$selectcolor) + 
        scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) + 
        labs(title = "Genres for Movie Titles", x = "Genres", y = "Titles") + 
        theme_classic()
    }
    else if(input$selectvar == 2) {
      ggplot(data) + 
        geom_bar(aes(x = `Adult Title`), fill = input$selectcolor) + 
        scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) + 
        labs(title = "Adult Ratings for Movie Titles", x = "Adult Ratings", y = "Titles") + 
        theme_classic()
    }
    else if(input$selectvar == 3) {
      ggplot(data) + 
        geom_histogram(aes(x = Rating), bins = input$bins, fill = input$selectcolor) + 
        scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) + 
        xlim(input$range) + 
        labs(title = "Ratings for Movie Titles", x = "Ratings", y = "Titles") + 
        theme_classic()
    }
    else if(input$selectvar == 4) {
      ggplot(data) + 
        geom_bar(aes(x = Type), fill = input$selectcolor) + 
        scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) + 
        labs(title = "Format Types for Movie Titles", x = "Format Types", y = "Titles") + 
        theme_classic()
    }
    else {
      ggplot(data) + 
        geom_histogram(aes(x = `Release Year`), bins = input$bins, fill = input$selectcolor) +
        scale_y_continuous(labels = comma_format(big.mark = ",", decimal.mark = ".")) +  
        xlim(input$range2) + 
        labs(title = "Release Years for Movie Titles", x = "Release Years", y = "Titles") + 
        theme_classic()
    }
  })
  
  output$mean <- renderPrint({
    if(input$checkbox == TRUE && input$selectvar == 3) {
      print("Mean")
      mean(data$Rating)
    }
    else if(input$checkbox == TRUE && input$selectvar == 5) {
      print("Mean")
      mean(data$`Release Year`, na.rm = TRUE)
    }
  })
  
  output$sd <- renderPrint({
    if(input$checkbox == TRUE && input$selectvar == 3) {
      print("Standard Deviation")
      sd(data$Rating)
    }
    else if(input$checkbox == TRUE && input$selectvar == 5) {
      print("Standard Deviation")
      sd(data$`Release Year`, na.rm = TRUE)
    }
  })
  
  output$fivenum <- renderPrint({
    if(input$checkbox == TRUE && input$selectvar == 3) {
      print("Five Number Summary")
      fivenum(data$Rating)
    }
    else if(input$checkbox == TRUE && input$selectvar == 5) {
      print("Five Number Summary")
      fivenum(data$`Release Year`, na.rm = TRUE)
    }
  })
  
  output$table <- renderPrint({
    if(input$checkbox == TRUE && input$selectvar == 1) {
      print("Frequency Table")
      kable(table(data$Genre), col.names = c("Genre", "Frequency"))
    }
    else if(input$checkbox == TRUE && input$selectvar == 2) {
      print("Frequency Table")
      kable(table(data$`Adult Title`), col.names = c("Adult Rating", "Frequency"))
    }
    else if(input$checkbox == TRUE && input$selectvar == 4) {
      print("Frequency Table")
      kable(table(data$Type), col.names = c("Format Type", "Frequency"))
    }
  })
  
  output$image <- renderImage ({
    list(src = "movie_image.png", height = "400px", width = "400px")
  })
}

shinyApp(ui = ui, server = server)
