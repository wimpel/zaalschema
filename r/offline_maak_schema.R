# geef clubnaam op
clubnaam <- "Doetinchem"

# laden libraries
library(data.table)
library(fasttime)
library(lubridate)
library(openxlsx)
library(tidyverse)

# als de pouleindeling nog nite is aangemaakt >> aanmaken
if (!file.exists("./data/poule_indeling.rds")) source("./r/offline_poule_en_klasse.R")
# pouleindeling inlezen
indeling <- readRDS("./data/poule_indeling.rds")

# inlezen export Hockeyweerelt
# dit bevat 
#  - alle zaalwedstrijden van de opgegeven club
#  - alle zaalwedstrijden waar de opgegeven club zaaldienst heeft
zaalschema <- readxl::read_excel(paste0("./data/test/zaal_", clubnaam, ".xlsx"))
# de meest frequente string van zaalleiding is de club waarvopor het schema gemaakt moet worden
clubnaam2 <- names(which.max(table(zaalschema$zaalleiding)))
# maak werkkopie
DT <- data.table::setDT(data.table::copy(zaalschema))
# tijdstip goedzetten qua tijdzone (CET = -1 uur tov UTC, wintertijd)
DT[, speeltijd := fasttime::fastPOSIXct(zaalschema$datum) %m-% hours(1)]
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
teamdienst[zaalleiding == clubnaam & categorie %in% c("junioren") & !grepl("^Topklasse", klasse), 
           wedstrijdtafel := thuis[1], by = .(speeldatum, accommodatie, pl_id)]
#overzicht per team
teamdienst <- teamdienst[grepl(clubnaam, wedstrijdtafel), 
                         .(zaalleiding, speeltijd, accommodatie, veld, wedstrijd, klasse), 
                         by = .(wedstrijdtafel)]
data.table::setkey(teamdienst, wedstrijdtafel, speeltijd)

# zaalleiding per dag
zaaldienst <- DT[zaalleiding == clubnaam, ]
zaaldienst[teamdienst, wedstrijdtafel := i.wedstrijdtafel, on = .(speeltijd, wedstrijd)]
zaaldienst[is.na(wedstrijdtafel) & categorie %in% c("senioren", "jongste jeugd"), wedstrijdtafel := thuis]
zaaldienst[is.na(wedstrijdtafel) & 
             (categorie %in% c("senioren", "jongste jeugd") | 
                (categorie %in% c("junioren") & grepl("^Topklasse", klasse))), wedstrijdtafel := thuis]
zaaldienst <- zaaldienst[, .(wedstrijd, veld, klasse, wedstrijdtafel), keyby = .(speeldatum, accommodatie, tijdstip)]
zaaldienst[, speeldatum := format(speeldatum, "%d-%m-%Y")]

# maak output

wb <- openxlsx::createWorkbook()

sheetnaam <- "zaalleiding"
openxlsx::addWorksheet(wb, sheetnaam)
openxlsx::setColWidths(wb, sheetnaam, cols = 1, widths = 15)
openxlsx::setColWidths(wb, sheetnaam, cols = 2:ncol(zaaldienst), widths = "auto")
openxlsx::writeData(wb, sheetnaam, zaaldienst)

sheetnaam <- "wedstrijdtafel per team"
openxlsx::addWorksheet(wb, sheetnaam)
openxlsx::setColWidths(wb, sheetnaam, cols = 1:ncol(teamdienst), widths = "auto")
openxlsx::setColWidths(wb, sheetnaam, cols = 3, widths = 20)
openxlsx::writeData(wb, sheetnaam, teamdienst)
openxlsx::saveWorkbook(wb, paste0("./output/zaal_", clubnaam, ".xlsx"), overwrite = TRUE)

