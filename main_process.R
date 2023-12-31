#Working Directory
MAIN_PATH <- "C:/Users/simon/OneDrive/"
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))

#Get Libraries and needed Data
source("CONFIG.R")

#####START LOOP#####
#repeat {
#Flags
NR_new_results <- FALSE
NR_new_elected <- FALSE
SR_new_elected <- FALSE
NR_results_finished <- FALSE
NR_candidates_finished <- FALSE
SR_candidates_finished <- FALSE

#Load Databases
source("load_databases.R")

###Get BFS Data and update DB
source("NR_get_data_results.R")
source("NR_get_data_candidates.R")
source("SR_get_data_candidates.R")

#Load Databases again
source("load_databases.R")

###OVERVIEW RESULTS###
#source("All_prepare_results.R")
#source("All_publish_charts.R")
#source("All_create_output_parliament_flourish.R")
#source("All_create_output_candidates_flourish.R")

###CANTON RESULTS###
##Check: Canton completed?
#Get counted cantons
source("get_counted_cantons.R")

###INTERMEDIATE RESULTS NATIONALRAT###
check_intermediate <- intermediate_timecheck %>%
  filter(hour == hour(Sys.time()))

# if (nrow(check_intermediate) > 0) {
# if ((minute(Sys.time()) >= 35) & (check_intermediate$status == "pending") ){
#   if (nrow(counted_cantons) > 0) {
#   source("load_databases.R")
#   source("All_prepare_results.R")
#   source("NR_text_intermediate.R")
#   source("NR_mars_meldung_intermediate_DE.R")
#   source("NR_mars_meldung_intermediate_FR.R")
#   source("NR_mars_meldung_intermediate_IT.R")
#   #Send Mail
#   send_mail(type="NR_Overview",
#             recipients= paste0(DEFAULT_EMAILS,",inland@keystone-ats.ch,suisse@keystone-ats.ch"))
#   }
#   #Set Intermediate news done
#   mydb <- connectDB(db_name = "sda_elections")  
#   sql_qry <- paste0("UPDATE intermediate_timecheck SET status = 'done' WHERE hour = '",hour(Sys.time()),"'")
#   rs <- dbSendQuery(mydb, sql_qry)
#   dbDisconnectAll() 
# }  
# }

###NATIONALRAT###
if (nrow(counted_cantons) > 0) {
for (c in 1:nrow(counted_cantons)) {
##PARTIES RESULTS HERE##
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
          recipients= paste0(DEFAULT_EMAILS,",",counted_cantons$mail_KeySDA[c]))
}
##Charts Results##
if (counted_cantons$charts_results[c] == "pending") {
#Generate Output
source("NR_prepare_results_charts.R")
source("NR_publish_results_charts_DE.R")
source("NR_publish_results_charts_FR.R")
source("NR_publish_results_charts_IT.R")
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
                            recipients= paste0(DEFAULT_EMAILS,",",counted_cantons$mail_KeySDA[c]))
    #Set Status Done
    mydb <- connectDB(db_name = "sda_elections")  
    sql_qry <- paste0("UPDATE output_overview SET analytics = 'done' WHERE election_ID = '",counted_cantons$election_ID[c],"'")
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll() 
}
}  
##CANDIDATES RESULTS HERE##
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
              recipients= paste0(DEFAULT_EMAILS,",",counted_cantons$mail_KeySDA[c]))
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
##Alerts## 
if (counted_cantons$alerts[c] == "pending") {
    #Send VIP-Mail
    vip_alert(counted_cantons$area_ID[c],
              "NR",
              recipients= paste0(DEFAULT_EMAILS,",",counted_cantons$mail_KeySDA[c]))
    #Set Status Done
    mydb <- connectDB(db_name = "sda_elections")  
    sql_qry <- paste0("UPDATE output_overview SET alerts = 'done' WHERE election_ID = '",counted_cantons$election_ID[c],"'")
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll() 
}    
}
}
}

###STAENDERAT ENDRESULTAT###
if (nrow(counted_cantons_SR) > 0) {
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
          recipients= paste0(DEFAULT_EMAILS,",",counted_cantons_SR$mail_KeySDA[c]))
}
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
}
##Alerts## 
if (counted_cantons_SR$alerts[c] == "pending") {
#Send VIP-Mail
vip_alert(counted_cantons_SR$area_ID[c],
          "SR",
          recipients= paste0(DEFAULT_EMAILS,",",counted_cantons_SR$mail_KeySDA[c]))
#Set Status Done
mydb <- connectDB(db_name = "sda_elections")  
sql_qry <- paste0("UPDATE output_overview SET alerts = 'done' WHERE election_ID = '",counted_cantons_SR$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)
dbDisconnectAll() 
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

###ELECTION FINISHED###
if (NR_results_finished == TRUE) {
print("Create overall results charts") 
  
source("load_databases.R")
  ch_metadata <- election_metadata %>%
    filter(date == "2023-10-22",
           council == "NR"
    )
  
  #Merge with area, text and output overview
  ch_metadata  <- ch_metadata   %>%
    left_join(areas_metadata) %>%
    left_join(status_texts) %>%
    left_join(output_overview) %>%
    filter(area_type == "nation")

if (ch_metadata$status != "finished") {
  source("All_get_data_results.R")
}  
  
##Charts Results##
if (ch_metadata$charts_results[1] == "pending") {
source("All_prepare_results_charts.R")
source("All_publish_results_charts_DE.R")
source("All_publish_results_charts_FR.R")
source("All_publish_results_charts_IT.R")
    #Set Status Done
    mydb <- connectDB(db_name = "sda_elections")  
    sql_qry <- paste0("UPDATE output_overview SET charts_results = 'done' WHERE election_ID = '2023-10-22_CH_NR'")
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll() 
  }
##Charts History##
if (ch_metadata$charts_history[1] == "pending") {
source("All_prepare_results_charts_history.R")
source("All_publish_results_charts_history.R")
    #Set Status Done
    mydb <- connectDB(db_name = "sda_elections")  
    sql_qry <- paste0("UPDATE output_overview SET charts_history = 'done' WHERE election_ID = '2023-10-22_CH_NR'")
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll() 
  }
##Analytics##
if ((ch_metadata$analytics[1] == "pending") & (NR_candidates_finished == TRUE)) {
  print("Create overall reports and VIP alerts of candidates") 
    #Generate Output
    email_elected_report_nr(recipients = paste0(DEFAULT_EMAILS,",",ch_metadata$mail_KeySDA))
    #Set Status Done
    mydb <- connectDB(db_name = "sda_elections")  
    sql_qry <- paste0("UPDATE output_overview SET analytics = 'done' WHERE election_ID = '2023-10-22_CH_NR'")
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll() 
}
##Alerts##
if ((ch_metadata$alerts[1] == "pending") & (NR_candidates_finished == TRUE)) {
    #Send VIP-Mail
    vip_alert(council = "NR",
              recipients= paste0(DEFAULT_EMAILS,",",ch_metadata$mail_KeySDA))
    #Set Status Done
    mydb <- connectDB(db_name = "sda_elections")  
    sql_qry <- paste0("UPDATE output_overview SET alerts = 'done' WHERE election_ID = '2023-10-22_CH_NR'")
    rs <- dbSendQuery(mydb, sql_qry)
    dbDisconnectAll() 
  }  
  
##Create Visuals##
counted_cantons <- ch_metadata
source("NR_create_visual_data.R")
}
  
###COMMUNITIES UR-LENA###
##Output tables and texts##
if (NR_new_results == TRUE) {
source("Communities_live_data.R")

#Commit to Github#
library(git2r)
git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
token <- read.csv(paste0(MAIN_PATH,"Github_Token/token.txt"),header=FALSE)[1,1]
git2r::cred_token(token)
gitadd()
gitcommit()
gitpush()
detach("package:git2r",unload=TRUE)
#Publish Charts  
source("Communities_publish_charts.R")
}    

###CREATE VISUAL DATA###
#source("produce_visual_data.R")
#}

