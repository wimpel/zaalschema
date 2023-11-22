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

# inladen poule en klasse indeling district NO
indeling <- readRDS("./poule_indeling.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Zaalschema's 2023-2024"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    fileInput("zaalschemaHockeyweerelt",
              label="Upload export wedstrijden hockeyweerelt",
              multiple = FALSE)
    
  ),
  dashboardBody(
    tabBox(width = "100%",
           tabPanel("Zaaldienst", DT::dataTableOutput("zaaldienstTabel")),
           tabPanel("Wedstrijdtafel", DT::dataTableOutput("teamdienstTabel")),
           tabPanel("Export Hockeyweerelt", DT::dataTableOutput("endotable")),
           tabPanel("Club", verbatimTextOutput(outputId = "outText"))
  )
  )
)
server <- function(input, output, session) {
  zaalschema <- reactiveVal(NULL)
  zaaldienst <- reactiveVal(NULL)
  teamdienstoutput <- reactiveVal(NULL)
  zaaldienstoutput <- reactiveVal(NULL)
  clubnaam <- reactiveVal(NULL)
  
  observe({
    inFile <- input$zaalschemaHockeyweerelt
    if (!is.null(inFile)) {   
      dataFile <- read_excel(inFile$datapath, sheet=1)
      zaalschema(dataFile)
      clubnaam(names(which.max(table(zaalschema()$zaalleiding))))
      ##
      # maak werkkopie
      DT <- data.table::setDT(data.table::copy(zaalschema()))
      # tijdstip goedzetten qua tijdzone (CET = -1 uur tov UTC, wintertijd)
      DT[, speeltijd := fasttime::fastPOSIXct(datum) %m-% hours(1)]
      # splits uit en thuisteams
      DT[, c("thuis", "uit") := data.table::tstrsplit(wedstrijd, " - ")]
      # start wedstrijd
      DT[, start := speeltijd]
      # einde wedstrijd (+45 minuten)
      DT[, eind := speeltijd %m+% minutes(45)]
      # geef een id op van de hoeveelste wedsrtijd op deze accommodatie op deze dag
      DT[, dag_id := data.table::rowid(data.table::as.IDate(speeltijd), accommodatie)]
      # stel datumin as iDate
      DT[, speeldatum := data.table::as.IDate(speeltijd)]

      # wat zijn de regels van de bond?
      # senioren >>
      #   - Eerstgenoemde team bemant wedstrijdtafel
      # junioren >>
      #   - topklasse >> Eerstgenoemde team bemant wedstrijdtafel
      #   - overig    >> Organiserende vereniging levert bemanning wedstrijdtafel
      # jongste jeugd >> Eerstgenoemde team bemant wedstrijdtafel

      # we splitsen dus op naar deze 4 categorieen
      # junioren
      DT[grepl("MO|JO", wedstrijd), categorie := "junioren"]
      # jongste jeugd (subset van junioren!!!)
      DT[grepl("O10|O9|O8", wedstrijd), categorie := "jongste jeugd"]
      # overig == senioren
      DT[is.na(categorie), categorie := "senioren"]
      # voeg de klasse toe
      DT[indeling, klasse := i.klasse, on = .(thuis = team)]

      # dienst wedsrtijdtafel per team
      teamdienst <- data.table::copy(DT)
      data.table::setkey(teamdienst, speeltijd, accommodatie, pl_id)
      # thuispelend team heeft dienst aan de wedsrtijdtafel
      teamdienst[categorie %in% c("senioren", "jongste jeugd") |
                   (categorie %in% c("junioren") & grepl("^Topklasse", klasse)),
                 wedstrijdtafel := thuis]
      # eerst spelende thuisteam heeft dienst aan de wedstrijdtafel voor het hele drieluik
      teamdienst[zaalleiding == clubnaam() & categorie %in% c("junioren") & !grepl("^Topklasse", klasse),
                 wedstrijdtafel := thuis[1], by = .(speeldatum, accommodatie, pl_id)]
      #overzicht per team
      teamdienst <- teamdienst[grepl(clubnaam(), wedstrijdtafel),
                               .(zaalleiding, speeltijd, accommodatie, veld, wedstrijd, klasse),
                               by = .(wedstrijdtafel)]
      data.table::setkey(teamdienst, wedstrijdtafel, speeltijd)
      taemdienst_ruw <- data.table::copy(teamdienst)
      teamdienst[, speeltijd := format(speeltijd, "%d-%m-%Y %H:%M")]
      teamdienstoutput(teamdienst)

      # zaalleiding per dag
      zaaldienst <- DT[zaalleiding == clubnaam(), ]
      zaaldienst[taemdienst_ruw, wedstrijdtafel := i.wedstrijdtafel, on = .(speeltijd, wedstrijd)]
      zaaldienst[is.na(wedstrijdtafel) & categorie %in% c("senioren", "jongste jeugd"), wedstrijdtafel := thuis]
      zaaldienst[is.na(wedstrijdtafel) &
                   (categorie %in% c("senioren", "jongste jeugd") |
                      (categorie %in% c("junioren") & grepl("^Topklasse", klasse))), wedstrijdtafel := thuis]
      zaaldienst <- zaaldienst[, .(wedstrijd, veld, klasse, wedstrijdtafel), keyby = .(speeldatum, accommodatie, tijdstip)]
      zaaldienst[, speeldatum := format(speeldatum, "%d-%m-%Y")]
      zaaldienstoutput(zaaldienst)
      ###

    }
    
  })

  
  
    
  output$endotable = DT::renderDT({
    zaalschema()
  })
  
  output$teamdienstTabel = DT::renderDT({
    teamdienstoutput()
  },extensions = 'Buttons', server = FALSE,
  options = list(dom = 'Bfrtip',
                 exportOptions = list(header = ""),
                 buttons = c('copy', 'csv', 'excel', 'pdf')))
  
  output$zaaldienstTabel = DT::renderDT({
    zaaldienstoutput()
  },extensions = 'Buttons', server = FALSE,
  options = list(dom = 'Bfrtip',
                 exportOptions = list(header = ""),
                 buttons = c('copy', 'csv', 'excel', 'pdf')))
  
  output$outText <- renderText(clubnaam())
  

}
shinyApp(ui, server)