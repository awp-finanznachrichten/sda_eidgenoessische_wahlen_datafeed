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
MAIN_PATH <- "C:/Users/sw/OneDrive/"
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))

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
source("send_mail.R")
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

#Adapt Metadata

#####START LOOP#####

#Flags
NR_new_results <- FALSE
NR_new_elected <- FALSE
SR_new_elected <- FALSE
intermediate_done <- TRUE 
if (minute(Sys.time()) < 35) {
intermediate_done <- FALSE 
}  

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
         grepl("finished",status) == TRUE
         )

#Merge with area, text and output overview
counted_cantons_all <- counted_cantons_all  %>%
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton")

#Get counted cantons NR and SR
counted_cantons <- counted_cantons_all %>%
  filter(council == "NR")
counted_cantons_SR <- counted_cantons_all %>%
  filter(council == "SR") 

###NATIONALRAT###
for (c in 1:nrow(counted_cantons)) {
if (counted_cantons$status[c] != "candidates finished") {
##Text Results##
if (counted_cantons$texts_results[c] == "pending") {
#Generate Output
source("NR_text_results.R")
source("NR_mars_meldung_results_DE.R")
source("NR_mars_meldung_results_FR.R")
source("NR_mars_meldung_results_IT.R")
#Set Status Done
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE output_overview SET texts_results = 'done' WHERE election_ID = '",counted_cantons$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll()  
#Send Mail
send_mail(type="NR_Results",
          recipients= "robot-notification@awp.ch,contentdevelopment@keystone-sda.ch")
}
##Charts Results##
if (counted_cantons$charts_results[c] == "pending") {
#Generate Output
source("NR_prepare_results_charts.R")
source("NR_publish_results_charts.R")
#Set Status Done
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE output_overview SET charts_results = 'done' WHERE election_ID = '",counted_cantons$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 
}
##Charts Results History##
if (counted_cantons$charts_history[c] == "pending") {
source("NR_prepare_results_charts_history.R")
source("NR_publish_results_charts_history.R")
#Set Status Done  
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE output_overview SET charts_history = 'done' WHERE election_ID = '",counted_cantons$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 
}
##Analytics##
if (counted_cantons$analytics[c] == "pending") {
    #Generate Output
    email_elected_report_nr(counted_cantons$area_ID[c],
                            recipients = "robot-notification@awp.ch,contentdevelopment@keystone-sda.ch")
    #Set Status Done
    mydb <- connectDB(db_name = "sda_elections")  
    sql_qry <- paste0("UPDATE output_overview SET analytics = 'done' WHERE election_ID = '",counted_cantons$election_ID[c],"'")
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll() 
}
  
}  
if (counted_cantons$status[c] != "parties finished") {
  ##Text Candidates##
  if (counted_cantons$texts_candidates[c] == "pending") {
    #Generate Output
    source("NR_text_candidates.R")
    source("NR_mars_meldung_candidates_DE.R")
    source("NR_mars_meldung_candidates_FR.R")
    source("NR_mars_meldung_candidates_IT.R")
    #Set Status Done
    mydb <- connectDB(db_name = "sda_elections")  
    sql_qry <- paste0("UPDATE output_overview SET texts_candidates = 'done' WHERE election_ID = '",counted_cantons$election_ID[c],"'")
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll() 
    #Send Mail
    send_mail(type="NR_Candidates",
              recipients= "robot-notification@awp.ch,contentdevelopment@keystone-sda.ch")
  }  
  
##Charts Candidates##
if (counted_cantons$charts_candidates[c] == "pending") {
#Generate Output
source("NR_publish_candidates_charts.R")
#Set Status Done
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE output_overview SET charts_candidates = 'done' WHERE election_ID = '",counted_cantons$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 
}
}
}

###STAENDERAT###
for (c in 1:nrow(counted_cantons_SR)) {

##Text Candidates##
if (counted_cantons_SR$texts_candidates[c] == "pending") {
source("SR_text_candidates.R")
source("SR_mars_meldung_candidates_DE.R") 
source("SR_mars_meldung_candidates_FR.R") 
source("SR_mars_meldung_candidates_IT.R") 
#Set Status Done
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE output_overview SET texts_candidates = 'done' WHERE election_ID = '",counted_cantons_SR$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll()
#Send Mail
send_mail(type="SR_Candidates",
          recipients= "robot-notification@awp.ch,contentdevelopment@keystone-sda.ch")
}
  
if (counted_cantons_SR$charts_candidates[c] == "pending") {
##Chart Candidates##
source("SR_publish_candidates_charts.R")
#Set Status Done
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE output_overview SET charts_candidates = 'done' WHERE election_ID = '",counted_cantons_SR$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 
}
}

###INTERMEDIATE RESULTS NATIONALRAT###
if ((minute(Sys.time()) >= 25) & (intermediate_done == FALSE)) {
  source("load_databases.R")
  source("All_prepare_results.R")
  source("NR_text_intermediate.R")
  source("NR_mars_meldung_intermediate_DE.R")
  source("NR_mars_meldung_intermediate_FR.R")
  source("NR_mars_meldung_intermediate_IT.R")
  #Send Mail
  send_mail(type="NR_Overview",
            recipients= "robot-notification@awp.ch,contentdevelopment@keystone-sda.ch")
  intermediate_done <- TRUE
}  

###OVERVIEW RESULTS###
if (NR_new_results == TRUE || NR_new_elected == TRUE || SR_new_elected == TRUE) {
source("load_databases.R")
source("All_prepare_results.R")
source("All_publish_charts.R")
source("All_create_output_parliament_flourish.R")
source("All_create_output_candidates_flourish.R")
}

###ELECTION FINISHED###
#TO DO#

###COMMUNITIES UR-LENA###
##Output tables and texts##
if (NR_new_results == TRUE) {
source("Communities_live_data.R")
source("Communities_publish_charts.R")
}

###COMMIT###
git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
token <- read.csv(paste0(MAIN_PATH,"Github_Token/token.txt"),header=FALSE)[1,1]
git2r::cred_token(token)
gitadd()
gitcommit()
gitpush()

###CREATE VISUAL###
counted_cantons <- counted_cantons_all %>%
  filter(council == "NR")
source("NR_create_visual_data.R")
source("SR_create_visual_data.R")

###ENTER ALL CANDIDATES RESULTS
##TO DO##
