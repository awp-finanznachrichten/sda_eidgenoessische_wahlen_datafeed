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
source("function_create_table_communities.R")
setwd("..")
source("./tools/Funktionen/Utils.R")

#Databases
source("load_databases.R")

#Texts
texts_spreadsheet <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Sitzverteilung")

#####START LOOP#####

###Get Data and update DB
source("get_data_bfs_feed.R")
#source("update_DB_NR_cantons_parties.R")
#source("update_DB_NR_cantons_candidates.R")

###NATIONALRAT###

##Check: Canton completed?
#Get counted cantons
counted_cantons <- election_metadata %>%
  filter(council == "NR",
         date == "2023-10-22",
         grepl("finished",status) == FALSE
         )

#Merge with area and text data
counted_cantons <- counted_cantons  %>%
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  filter(area_type == "canton")

#Merge with output overview
###TO DO###

for (c in 1:nrow(counted_cantons)) {

##Text Results##
#Check output overview 
###TO DO### 

#Generate Output
source("text_results_NR.R")
source("mars_meldung_results_NR_DE.R")
source("mars_meldung_results_NR_FR.R")
source("mars_meldung_results_NR_IT.R")

##Text Candidates##
#Check output overview
###TO DO###

#Generate Output
source("text_candidates_NR.R")
source("mars_meldung_results_NR_DE.R")
source("mars_meldung_results_NR_FR.R")
source("mars_meldung_results_NR_IT.R")

##Charts Results##
#Check output overview
###TO DO###
  
#Generate Output
source("prepare_results_charts_NR.R")
source("publish_results_charts_NR.R")

  
##Charts Candidates##
#Check output overview
###TO DO###
  
#Generate Output
###TO DO###

##Analytics##

#Check output overview
###TO DO###
  
#Generate Output
###TO DO###

}

##Chart Overall##
###TO DO###


###STAENDERAT###
##Text Candidates##

##Charts Candidates##

##Charts Overall##

###COMMUNITIES###
##Output tables and texts##
source("live_data_communities.R")