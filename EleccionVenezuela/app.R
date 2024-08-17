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
binder <- read.csv("binder.csv", header = TRUE)
#clave_candidato <- read.csv("clave_candidato.csv", header = TRUE)


ui <- dashboardPage(
  dashboardHeader(title = "Venezuela, Elecciones 2024", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estado", tabName = "estado", icon = icon("dashboard")),
      menuItem("Municipio", tabName = "municipio", icon = icon("city")),
      menuItem("Parroquia", tabName = "parroquia", icon = icon("tree-city"))
    ),
    fluidRow(
      style='padding-left:20px; padding-right:0px; padding-top:10px; padding-bottom:5px',
      div(
        h4("Con datos de", align = "left"),
        a("Resultados con Venezuela", href="https://www.resultadosconvzla.com/")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "estado",
        fluidRow(
          column(
            1
          ),
          column(
            2,
            fluidRow(
              selectInput('estados', 'Estado', names_unique(ven_02$EDO)))
            )
          ),
        fluidRow(
          column(
            6,
            plotOutput("the_plot")
          )
        )
      ),
      tabItem(
        tabName = "municipio",
        fluidRow(
          column(
            1
          ),
          column(
            2,
            fluidRow(
              selectInput('estados2', 'Estado', names_unique(ven_02$EDO))
            )
          ),
          column(
            2,
            fluidRow(
              selectInput("municipio", "Municipio", choices = NULL)
            )
          )
        ),
        fluidRow(
          column(
            6,
            plotOutput("the_plot2")
          )
        )
      ),
      tabItem(
        tabName = "parroquia",
        fluidRow(
          column(
            1
          ),
          column(
            2,
            fluidRow(
              selectInput("estados3", "Estado", names_unique(ven_02$EDO))
            )
          ),
          column(
            2,
            fluidRow(
              selectInput("municipio3", "Municipio", choices = NULL)
            )
          ),
          column(
            2,
            fluidRow(
              selectInput("parroquia3", "Parroquia", choices = NULL)
            )
          )
        ),
        fluidRow(
          column(
            6,
            plotOutput("the_plot3")
          )
        )
      )
    )
  )
)

# Server ####
server <- function(input, output, session){
  
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
 
 # parroquia
 selected_state3 <- reactive({
   filter(ven_02, EDO == input$estados3)
 })
 
 observeEvent(selected_state3(), {
   choices <- unique(selected_state3()$MUN)
   updateSelectInput(inputId = "municipio3", choices = choices) 
 })
 
 selected_municipality3 <- reactive({
   req(input$municipio3)
   filter(selected_state3(), MUN == input$municipio3)
 })
 
 observeEvent(selected_municipality3(), {
   choices <- unique(selected_municipality3()$PAR)
   updateSelectInput(inputId = "parroquia3", choices = choices) 
 })
 
 selected_parroquia3 <- reactive({
   req(input$parroquia3)
   filter(selected_municipality3(), PAR == input$parroquia3)
 })
 
 output$the_plot3 <- renderPlot({
   this_title3 <- paste0("Porcentaje de votos")
   each_parrish <-  selected_parroquia3() %>%
     summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)
   
   x <- percentage_function(each_parrish)
   g <- the_graphic(x, this_title3)
   g
 })
}
shinyApp(ui = ui, server = server)
