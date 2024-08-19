# Libraries ####
library(shiny)
library(tidyverse)
#library(shinyjs)
library(DT)
library(shinydashboard)
library(leaflet)
library(mongolite)

# Load Files ####
source("functionsApp.R")

# Load Data Frames ####
ven_02 <- read.csv("RESULTADOS_2024_CSV_V2.csv", header = TRUE, encoding = "latin1")
#%>% 
 # mutate_if(is.character, utf8::utf8_encode)
binder <- read.csv("binder.csv", header = TRUE)
#clave_candidato <- read.csv("clave_candidato.csv", header = TRUE)


ui <- dashboardPage(
  dashboardHeader(title = "Venezuela, Elecciones 2024", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estado", tabName = "estado", icon = icon("dashboard")),
      menuItem("Municipio", tabName = "municipio", icon = icon("city")),
      menuItem("Parroquia", tabName = "parroquia", icon = icon("tree-city")),
      menuItem("Acta", tabName = "acta", icon = icon("receipt"))
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
            width = 1
          ),
          column(
            width = 2,
            fluidRow(
              selectInput('estados', 'Estado', names_unique(ven_02$EDO))
            )
          ),
          valueBoxOutput("votosValidos")
        ),
        fluidRow(
          column(
            6,
            plotOutput("the_plot")
          ),
          column(
            6,
            DTOutput("the_table")
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
          ),
          valueBoxOutput("votosValidos2")
        ),
        fluidRow(
          column(
            6,
            plotOutput("the_plot2")
          ),
          column(
            6,
            DTOutput("the_table2")
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
          ),
          valueBoxOutput("votosValidos3")
        ),
        fluidRow(
          column(
            6,
            plotOutput("the_plot3")
          ),
          column(
            6,
            DTOutput("the_table3")
          )
        )
      ),
      tabItem(
        tabName = "acta",
        fluidRow(
          column(
            1
          ),
          column(
            2,
            fluidRow(
              selectInput("estados5", "Estado", names_unique(ven_02$EDO))
            )
          ),
          column(
            2,
            fluidRow(
              selectInput("municipio5", "Municipio", choices = NULL)
            )
          ),
          column(
            2,
            fluidRow(
              selectInput("parroquia5", "Parroquia", choices = NULL)
            )
          ),
          column(
            2,
            fluidRow(
              selectInput("centro5", "Centro", choices = NULL)
            )
          ),
          column(
            2,
            fluidRow(
              selectInput("mesa5", "Mesa", choices = NULL)
            )
          )
        ),
        h2("Acta de Votación"),
        DTOutput("tablaXX"),
        uiOutput("img")
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
 
 each_state <- reactive({
   ven_02 %>% 
     filter(EDO == selected_state()) %>% 
     summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)
 })
 
 output$votosValidos <- renderValueBox({
   los_votos_validos <- each_state()[1,1]
   valueBox(
     los_votos_validos,
     "Votos Válidos",
     icon = icon("vote-yea"),
     color = "olive",
   )
 })
 
 output$the_plot <- renderPlot({
   this_title <- paste0("Porcentaje de votos")
   x <- percentage_function(each_state())
   g <- the_graphic(x, this_title)
   g
 })
 
 output$the_table <- renderDT({
   x <- percentage_function(each_state())
   x
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
 
 
 each_municipality <-  reactive({
   selected_municipality2() %>%
     summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)
 })
 
 output$votosValidos2 <- renderValueBox({
   los_votos_validos <- each_municipality()[1,1]
   valueBox(
     los_votos_validos,
     "Votos Válidos",
     icon = icon("vote-yea"),
     color = "olive",
   )
 })
 
 output$the_plot2 <- renderPlot({
   this_title2 <- paste0("Porcentaje de votos")
   x <- percentage_function(each_municipality())
   g <- the_graphic(x, this_title2)
   g
 })
 
 output$the_table2 <- renderDT({
   x <- percentage_function(each_municipality())
   x
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
 
 each_parish <-  reactive({
   selected_parroquia3() %>%
     summarise_at(vars(VOTOS_VALIDOS:BERA), sum, na.rm = TRUE)
 })
 
 output$votosValidos3 <- renderValueBox({
   los_votos_validos <- each_parish()[1,1]
   valueBox(
     los_votos_validos,
     "Votos Válidos",
     icon = icon("vote-yea"),
     color = "olive",
   )
 })
   
 output$the_plot3 <- renderPlot({
   this_title3 <- paste0("Porcentaje de votos")
   x <- percentage_function(each_parish())
   g <- the_graphic(x, this_title3)
   g
 })
 
 output$the_table3 <- renderDT({
   x <- percentage_function(each_parish())
   x
 })
 
 # Acta ####
 
 selected_state5 <- reactive({
   filter(ven_02, EDO == input$estados5)
 })
 
 observeEvent(selected_state5(), {
   choices <- unique(selected_state5()$MUN)
   updateSelectInput(inputId = "municipio5", choices = choices) 
 })
 
 selected_municipality5 <- reactive({
   req(input$municipio5)
   filter(selected_state5(), MUN == input$municipio5)
 })
 
 observeEvent(selected_municipality5(), {
   choices <- unique(selected_municipality5()$PAR)
   updateSelectInput(inputId = "parroquia5", choices = choices) 
 })
 
 selected_parroquia5 <- reactive({
   req(input$parroquia5)
   filter(selected_municipality5(), PAR == input$parroquia5)
 })
 
 observeEvent(selected_parroquia5(), {
   choices <- unique(selected_parroquia5()$CENTRO)
   updateSelectInput(inputId = "centro5", choices = choices) 
 })
 
 selected_centro5 <- reactive({
   req(input$centro5)
   filter(selected_parroquia5(), CENTRO == input$centro5)
 })
 
 observeEvent(selected_centro5(), {
   choices <- unique(selected_centro5()$MESA)
   updateSelectInput(inputId = "mesa5", choices = choices) 
 })
 
 selected_mesa5 <- reactive({
   req(input$mesa5)
   filter(selected_centro5(), MESA == input$mesa5)
 })
 
 
 output$img <- renderUI({
   acta <- as.character(selected_mesa5()$URL)
   tags$img(src = acta)
 })
 
}
shinyApp(ui = ui, server = server)
