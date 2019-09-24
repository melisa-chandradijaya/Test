
library(shiny)
library(ggplot2)
library(dplyr)


setwd("C:/Users/mchan/Desktop/Practical Data Science/DataSet")

income <- read.csv("Week 10 income.csv")


ui <- fluidPage(
  titlePanel("Module 10 Assignment"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "subset_income", label = "select income:", 
                  choices = unique(income$income)),
      selectInput(inputId = "set_yaxis", label = "Set Y_axis",
                  choices = c("capital_loss", "hours_per_week")),
      checkboxGroupInput(inputId = "subset_occupation", label = "Select Occupation:",
                         choices = unique(income$occupation),
                         selected = unique(income$occupation))),
    mainPanel(plotOutput(outputId = "my_figure"))
  )
)
server <- function(input, output) {
  create_subset <- reactive(income %>%
                              filter(capital_loss > 0 &
                                       income == input$subset_income &
                                       occupation %in% input$subset_occupation))
  
  output$my_figure <- renderPlot({ggplot(create_subset())+
                                   geom_boxplot(aes_string(x = "occupation", y = input$set_yaxis)) +
                                   theme_bw(18)+
                                   theme(axis.text.x = element_text(angle = 90, hjust = 1))})
}

shinyApp(ui, server)


  
  