library(git2r)
library(httr)
library(XML)
library(xlsx)
library(jsonlite)
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(DatawRappr)
library(lubridate)
library(rsvg)
library(magick)
library(zip)
library(RCurl)


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
source("storyfinder.R")
source("storyfinder_urlena.R")
source("storybuilder.R")
source("add_texts.R")
source("replace_variables_urlena.R")
source("replace_variables_cleanup.R")
source("replace_variables_cleanup_SR.R")
source("winners_losers.R")
source("voted_out_candidates.R")
source("text_charts.R")
source("create_tables_NR.R")
source("create_tables_SR.R")
source("create_tables_overview.R")
source("create_table_communities.R")
source("create_bilddaten.R")
source("create_visual_data.R")
source("function_reports.R")
source("github.R")
setwd("..")
source("./tools/Funktionen/Utils.R")

#Texts
texts_spreadsheet_NR_results <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Sitzverteilung")
texts_spreadsheet_NR_candidates <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Gewaehlte")
texts_spreadsheet_SR_candidates <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "SR_Resultate")
texts_spreadsheet_NR_intermediate <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Zwischenstand")
texts_spreadsheet_UrLena <- read.xlsx("./Texte/LENA Textbausteine Eidgenössische Wahlen 2023_ Gemeindeebene.xlsx", sheetName = "Textbausteine")
texts_spreadsheet_UrLena <- texts_spreadsheet_UrLena %>%
  filter(is.na(Text_d) == FALSE)

#Metadata Communities
meta_gmd_kt <- read_csv("Data/MASTERFILE_GDE.csv")

#####START LOOP#####

#Flags
NR_new_results <- FALSE
NR_new_elected <- FALSE
SR_new_elected <- FALSE

#Load Databases
source("load_databases.R")

###Get BFS Data and update DB
source("NR_get_data_results.R")
source("NR_get_data_candidates.R")
source("SR_get_data_candidates.R")

#Load Databases again
source("load_databases.R")

##Check: Canton completed?
#Get counted cantons
counted_cantons_all <- election_metadata %>%
  filter(date == "2023-10-22",
         grepl("finished",status) == TRUE #CHANGE TO TRUE
         )

#Merge with area, text and output overview
counted_cantons_all <- counted_cantons_all  %>%
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton")

###NATIONALRAT###
counted_cantons <- counted_cantons_all %>%
  filter(council == "NR")

###TO DO###

for (c in 1:nrow(counted_cantons)) {

##Text Results##
if (counted_cantons$texts_results[c] == "pending") {
#Generate Output
source("NR_text_results.R")
source("NR_mars_meldung_results_DE.R")
source("NR_mars_meldung_results_FR.R")
source("NR_mars_meldung_results_IT.R")
}

##Text Candidates##
if (counted_cantons$texts_candidates[c] == "pending") {
#Generate Output
source("NR_text_candidates.R")
source("NR_mars_meldung_candidates_DE.R")
source("NR_mars_meldung_candidates_FR.R")
source("NR_mars_meldung_candidates_IT.R")
}
  
##Charts Results##
if (counted_cantons$charts_results[c] == "pending") {
#Generate Output
source("NR_prepare_results_charts.R")
source("NR_publish_results_charts.R")
}
##Charts Results History##
if (counted_cantons$charts_history[c] == "pending") {
source("NR_prepare_results_charts_history.R")
source("NR_publish_results_charts_history.R")
}
  
##Charts Candidates##
if (counted_cantons$charts_candidates[c] == "pending") {
#Generate Output
source("NR_publish_candidates_charts.R")
}

##Analytics##
if (counted_cantons$analytics[c] == "pending") {
#Generate Output
email_elected_report_nr(counted_cantons$area_ID[c])
}
  
}

###STAENDERAT###
#Get counted cantons SR
counted_cantons_SR <- counted_cantons_all %>%
  filter(council == "SR") 

for (c in 1:nrow(counted_cantons_SR)) {

##Text Candidates##
if (counted_cantons_SR$texts_candidates[c] == "pending") {
source("SR_text_candidates.R") #TO DO
#source("SR_mars_meldung_candidates_DE.R") #TO DO
#source("SR_mars_meldung_candidates_FR.R") #TO DO
#source("SR_mars_meldung_candidates_IT.R") #TO DO
}
  
if (counted_cantons_SR$charts_candidates[c] == "pending") {
##Chart Candidates##
#source("SR_publish_candidates_charts.R")
}
}
###GESAMTERGEBNIS###
if (NR_new_results == TRUE || NR_new_elected == TRUE) {
source("All_prepare_results.R")
source("All_publish_charts.R")
source("All_create_output_parliament_flourish.R")
source("All_create_output_candidates_flourish.R")
  
###ZWISCHENSTAND (jeweils um x.35 Uhr)
source("NR_text_intermediate.R")
source("NR_mars_meldung_intermediate_DE.R")
source("NR_mars_meldung_intermediate_FR.R")
source("NR_mars_meldung_intermediate_IT.R")  
  
}

###ELECTION FINISHED###
#TO DO#

###COMMUNITIES###
##Output tables and texts##
if (NR_new_results == TRUE) {
source("Communities_live_data.R")
source("Communities_publish_charts.R")
}

###COMMIT###
git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
token <- read.csv("C:/Users/sw/OneDrive/Github_Token/token.txt",header=FALSE)[1,1]
git2r::cred_token(token)
gitadd()
gitcommit()
gitpush()

###CREATE VISUAL###
source("NR_create_visual_data.R")

