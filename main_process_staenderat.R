#Working Directory
MAIN_PATH <- "C:/Automatisierungen/"
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))

#Get Libraries and needed Data
source("CONFIG.R")

#####START LOOP#####
repeat {

#Load Databases
source("load_databases.R")

###CANTON RESULTS###
##Check: Canton completed?
#Get counted cantons
source("get_counted_cantons_staenderat.R")

###STAENDERAT ENDRESULTAT###
if (nrow(counted_cantons_SR) > 0) {
for (c in 1:nrow(counted_cantons_SR)) {
##Chart Candidates##
if (counted_cantons_SR$charts_candidates[c] == "pending") {
source("SR_publish_candidates_charts_DE.R")
source("SR_publish_candidates_charts_FR.R")
source("SR_publish_candidates_charts_IT.R")
#Set Status Done
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE output_overview SET charts_candidates = 'done' WHERE election_ID = '",counted_cantons_SR$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 

#Send Mail
selected_charts <- datawrapper_codes %>%
  filter(election_ID == counted_cantons_SR$election_ID[c],
         chart_type == "majorz_votes")

Subject <- paste0("Kanton ",counted_cantons_SR$area_name_de[c],": Endergebnis Ständerat zweiter Wahlgang")
Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
               "Die Ständeratergebnisse des Kantons ",counted_cantons_SR$area_name_de[c]," sind bekannt. ",
               "Es wurde folgende Grafiken erstellt:\n",
               "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[1],"/\n",
               "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[2],"/\n",
               "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[3],"/\n\n",
               "Liebe Grüsse\n\nLENA")
send_notification(Subject,Body,"robot-notification@awp.ch") #DEFAULT_MAILS
}
}
}
###STAENDERAT ZWISCHENRESULTAT###
if (nrow(intermediate_cantons_SR) > 0){
for (c in 1:nrow(intermediate_cantons_SR)) {
print(paste0("New intermediate data for ",intermediate_cantons_SR$area_ID[c]," found!"))
source("SR_publish_candidates_charts_intermediate_DE.R")
source("SR_publish_candidates_charts_intermediate_FR.R")
source("SR_publish_candidates_charts_intermediate_IT.R")  
#Set Data published
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE elections_metadata SET remarks = 'intermediate data published' WHERE election_ID = '",intermediate_cantons_SR$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll()   
}  
}
Sys.sleep(10)
if (hour(Sys.time()) > 22) {
break
}  
}

