# Libraries ####
library(shiny)
library(tidyverse)
library(shinyjs)
library(DT)
library(shinydashboard)
library(leaflet)
library(mongolite)

# Load Files ####
source("functionsApp.R")

# Load Data Frames ####
ven_02 <- read.csv("RESULTADOS_2024_CSV_V2.csv", header = TRUE)
binder <- read.csv("binder.csv", header = TRUE)
clave_candidato <- read.csv("clave_candidato.csv", header = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Venezuela, Elecciones 2024", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      selectInput('estados', 'Estado', names_unique(ven_02$EDO))
    )
  ),
  dashboardBody(
    plotOutput("the_plot")
  )
)

# Server ####
server <- function(input, output, session){
 selected_state <- reactive({
   input$estados
 })
 
 
 
 output$the_plot <- renderPlot({
   this_title <- paste0("Resultados en ", selected_state())
   each_state <- ven_02 %>% 
     filter(EDO == selected_state()) %>% 
     summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)
   
   x <- percentage_function(each_state)
   g <- the_graphic(x, this_title)
   g
   
 })
}
shinyApp(ui = ui, server = server)
