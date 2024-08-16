# Libraries ####
library(shiny)
library(tidyverse)
#library(shinyjs)
#library(DT)
library(shinydashboard)
library(leaflet)
library(mongolite)

# Load Files ####
source("functionsApp.R")

# Load Data Frames ####
ven_02 <- read.csv("RESULTADOS_2024_CSV_V2.csv", header = TRUE, encoding = "UTF-8") %>% 
  mutate_if(is.character, utf8::utf8_encode)
print(unique(ven_02$MUN))
binder <- read.csv("binder.csv", header = TRUE)
#clave_candidato <- read.csv("clave_candidato.csv", header = TRUE)


ui <- dashboardPage(
  dashboardHeader(title = "Venezuela, Elecciones 2024", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estado", tabName = "estado", icon = icon("dashboard")),
      menuItem("Municipio", tabName = "municipio", icon = icon("city"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "estado",
              fluidRow(selectInput('estados', 'Estado', names_unique(ven_02$EDO))),
              fluidRow(plotOutput("the_plot"))
      ),
      tabItem(tabName = "municipio",
              fluidRow(selectInput('estados2', 'Estado', names_unique(ven_02$EDO)),
                       selectInput("municipio", "Municipio", choices = NULL)
                       # uiOutput("secondSelection")
              ),
              fluidRow(plotOutput("the_plot2"))
      )
    )
  )
)

# Server ####
server <- function(input, output, session){

  # General 
  # output$secondSelection <- renderUI({
  #   selectInput("municipio", "Municipio", choices = unique(ven_02[ven_02$EDO==input$estados2,"MUN"]))
  # })
  
  # estado
 selected_state <- reactive({
   input$estados
 })
 
 output$the_plot <- renderPlot({
   this_title <- paste0("Porcentaje de votos en ", selected_state())
   each_state <- ven_02 %>% 
     filter(EDO == selected_state()) %>% 
     summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)
   
   x <- percentage_function(each_state)
   g <- the_graphic(x, this_title)
   g
   
 })
 
 # municipio
 selected_state2 <- reactive({
   filter(ven_02, EDO == input$estados2)
 })
 
 observeEvent(selected_state2(), {
   choices <- unique(selected_state2()$MUN)
   updateSelectInput(inputId = "municipio", choices = choices) 
 })
 
 selected_municipality2 <- reactive({
   req(input$municipio)
   filter(selected_state2(), MUN == input$municipio)
 })
 
 
 output$the_plot2 <- renderPlot({
   this_title2 <- paste0("Porcentaje de votos")
   each_municipality <-  selected_municipality2() %>%
     summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)

   x <- percentage_function(each_municipality)
   g <- the_graphic(x, this_title2)
   g

 })
 
 
 
 
 
}
shinyApp(ui = ui, server = server)
