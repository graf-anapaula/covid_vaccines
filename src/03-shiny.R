# Library ====
library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)

# Lectura de datos ====
data <- read_csv("data/interim/maps.csv")
colnames <- data %>% select(percent_vaccinated, percent_fully_vaccinated, gdp:percent_deaths) %>%
  colnames() 
# Shiny ====
# Make a toy dataset

x <- c(1,2,3,4,5)
y <- c(52,49,19,15,31)
grouping1 <- as.factor(c(1,1,1,2,2))
grouping2 <- as.factor(c(1,2,3,4,5))
toydataset <- data.frame(x,y,grouping1,grouping2)

# Make palettes to apply to each grouping

palette1 <- c("blue","red")
palette2 <- c("orange","yellow","green","blue","purple")

# the UI bit:
ui <- fluidPage(
  titlePanel("My question"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "selectedvariable",
                  label = "Select a variable", 
                  choices = colnames),
    ),
    textOutput("result")
    # mainPanel(
    #   plotOutput("myplot")
    )
  )


# the server bit:
server <- function(input, output) {
  
  output$result <- renderText({
    paste("You chose", input$selectedvariable)
  })
  
  currentvariable <- reactive({input$selectedvariable})
  output$myplot <- renderPlot({
    ggplot(data, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = input$selectedvariable), color = "white") +
      geom_text(stat='count', aes(x = input$selectedvariable, label = ..count..), vjust = -1)
      # scale_fill_viridis_c(option = "D")
  })
}

# Run it 
shinyApp(ui = ui, server = server)
