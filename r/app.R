library(shiny)
library(shinydashboard)
library(data.table)
library(fasttime)
library(lubridate)
library(openxlsx)
library(tidyverse)
library(readxl)
library(tidyxl)
library(unpivotr)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Zaalschema's 2023-2024"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    fileInput("csvs",
              label="Upload hockeyweerelt here",
              multiple = FALSE)
  ),
  dashboardBody(DT::dataTableOutput("endotable"))
  )
server <- function(input, output, session) {
  Data1 <- reactiveVal(NULL)
  Data2 <- reactiveVal(NULL)
  
  observe({
    inFile <- input$csvs
    if (!is.null(inFile)) {   
      dataFile <- read_excel(inFile$datapath, sheet=1)
      dat <- dataFile
      Data1(dat)
      Data2(dat)
    }
  })
  
  output$endotable = DT::renderDT({
    Data2()
  },options = list(scrollX = TRUE))
  

}
shinyApp(ui, server)