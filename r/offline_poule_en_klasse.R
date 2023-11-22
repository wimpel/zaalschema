#####################################################################################################################
# haal de puiledelindeling / klasseindeling op uit de drie aangeleverde
# bestanden van het district (on de ./input folder)
# inlezen poule-indelingen van de 3 bestanden van de bond (senioren / jongste jeugd / junioren)
#####################################################################################################################
library(data.table)
library(tidyxl)
library(unpivotr)
library(tidyverse)

indeling_compleet <-
  data.table::rbindlist(
    lapply(
      # loop door de (drie) bestanden van de districtsindeling heen
      list.files("./data", pattern = "^Definitieve indelingZAAL.*\\.xlsx", full.names = TRUE)
      , function(bestand) {
        data.table::rbindlist(
          # loop per bestand door de werkbladen heen
          lapply(readxl::excel_sheets(bestand), 
                 function(werkblad) {
                   # haal de data uit de cellen per werkblad op
                   cells <- tidyxl::xlsx_cells(bestand, sheet = werkblad)
                   # identificeer de opgegeven klasses/niveaus per werkblad
                   klasse <- dplyr::filter(cells, grepl("klasse|niveau", character, ignore.case = TRUE)) %>% 
                     # selecteer rij en kllom met de tekst per klasse/niveau
                     dplyr::select(row, col) %>% 
                     # join
                     dplyr::inner_join(cells, by = c("row", "col"))
                   # maak een unpivot functie
                   unpivot <- function(cells) {
                     cells %>%
                       unpivotr::behead("up-left", "klasse") %>% 
                       unpivotr::behead("up", "poule") %>% 
                       dplyr::mutate(indeling = werkblad) %>% 
                       dplyr::arrange(klasse, poule, character) %>% 
                       dplyr::select(indeling, klasse, poule, character) %>% 
                       dplyr::rename(team = character) %>% 
                       dplyr::filter(!is.na(team))
                   }
                   # partitioneer werkblad per klasse, haal de teams/poules er uit
                   unpivotr::partition(cells, klasse) %>% 
                     dplyr::mutate(cells = purrr::map(cells, unpivot)) %>% 
                     tidyr::unnest(cols = cells) %>% 
                     dplyr::select(indeling, klasse, poule, team)
                 }))
      }), fill = TRUE, use.names = TRUE)
# sla op in 
saveRDS(indeling_compleet, "./data/poule_indeling.rds")