library(httr)
library(XML)
library(xlsx)
library(jsonlite)
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(DatawRappr)

#Working Directory
setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")

#Main Data URL from BFS
BFS_API_URL <-
  "https://ckan.ogdch-abnahme.clients.liip.ch/api/3/action/package_show?id=eidg-wahlen-2023"
#BFS_API_URL <- "https://ckan.opendata.swiss/api/3/action/package_show?id=eidg-wahlen-2023"

#Datawrapper Auth
datawrapper_auth(Sys.getenv("DW_KEY"), overwrite = TRUE)

#Functions
setwd("./Functions")
source("functions_storyfinder.R")
source("functions_storybuilder.R")
source("functions_replace_variables_cleanup.R")
source("functions_winners_losers.R")
source("functions_voted_out_candidates.R")
source("function_text_charts.R")
source("functions_create_tables_NR.R")
source("functions_create_tables_overview.R")
source("function_create_table_communities.R")
setwd("..")
source("./tools/Funktionen/Utils.R")

#Texts
texts_spreadsheet_NR_results <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Sitzverteilung")
texts_spreadsheet_NR_candidates <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Gewaehlte")

#####START LOOP#####

#Load Databases
source("load_databases.R")

###Get BFS Data and update DB
source("get_data_results_NR.R")
source("get_data_candidates_NR.R")

###NATIONALRAT###

##Check: Canton completed?
#Get counted cantons
counted_cantons <- election_metadata %>%
  filter(council == "NR",
         date == "2023-10-22",
         grepl("finished",status) == FALSE
         )

#Merge with area, text and output overview
counted_cantons <- counted_cantons  %>%
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton")

###TO DO###

for (c in 1:nrow(counted_cantons)) {

##Text Results##
if (counted_cantons$texts_results[c] == "pending") {
#Generate Output
source("text_results_NR.R")
source("mars_meldung_results_NR_DE.R")
source("mars_meldung_results_NR_FR.R")
source("mars_meldung_results_NR_IT.R")
}
  
##Text Candidates##
if (counted_cantons$texts_candidates[c] == "pending") {
#Generate Output
source("text_candidates_NR.R")
source("mars_meldung_candidates_NR_DE.R")
source("mars_meldung_candidates_NR_FR.R")
source("mars_meldung_candidates_NR_IT.R")
}
  
##Charts Results##
if (counted_cantons$charts_results[c] == "pending") {
#Generate Output
source("prepare_results_charts_NR.R")
source("publish_results_charts_NR.R")
}
##Charts Results History##
if (counted_cantons$charts_history[c] == "pending") {
source("prepare_results_charts_history_NR.R")
source("publish_results_charts_history_NR.R")
}
  
##Charts Candidates##
if (counted_cantons$charts_candidates[c] == "pending") {
#Generate Output
source("publish_candidates_charts_NR.R")
}

##Analytics##
if (counted_cantons$analytics[c] == "pending") {
#Generate Output
###TO DO###
}
  
}

##Chart Overall##
source("prepare_overview_cantons.R")
source("publish_overview_charts.R")
source("create_output_candidates_flourish.R")

###STAENDERAT###
##Text Candidates##

##Charts Candidates##

##Charts Overall##

###COMMUNITIES###
##Output tables and texts##
source("live_data_communities.R")