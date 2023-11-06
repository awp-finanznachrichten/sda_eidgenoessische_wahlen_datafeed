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
  #Get counted or partly counted cantons
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
                       "Es wurden folgende Grafiken erstellt:\n",
                       "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[1],"/\n",
                       "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[2],"/\n",
                       "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[3],"/\n\n",
                       "Liebe Grüsse\n\nLENA")
        send_notification(Subject,Body,DEFAULT_EMAILS) 
      } 
      if (counted_cantons_SR$texts_candidates[c] == "pending") {
        ###Text meldung
        source("SR_text_candidates.R")
        source("SR_mars_meldung_candidates_DE.R")
        source("SR_mars_meldung_candidates_FR.R")
        source("SR_mars_meldung_candidates_IT.R")
        #Set Status Done
        mydb <- connectDB(db_name = "sda_elections")  
        sql_qry <- paste0("UPDATE output_overview SET texts_candidates = 'done' WHERE election_ID = '",counted_cantons_SR$election_ID[c],"'")
        rs <- dbSendQuery(mydb, sql_qry)
        dbDisconnectAll() 
      }
      if (counted_cantons_SR$charts_results[c] == "pending") {
        ###Flourish-CSV-Files
        source("All_create_output_candidates_flourish.R")
        #Send Mail
        send_attachment("Aktualisierte CSV für Flourish-Grafik der Gewählten",
                        paste0("Liebes Keystone-SDA-Team,\n\n",
                               "Die aktualisierten CSVs für die Flourish-Grafik der Gewählten findet ihr im Anhang.\n\n",
                               "Der Ständerat ist in folgenden Kantonen ausgezählt: ",paste(counted_cantons_SR$area_name_de, collapse= ", "),"\n\n",
                               "Liebe Grüsse\n\nLENA"),
                        attachment = c(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed/Output/elected_candidates_overall_de.csv"),
                                       paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed/Output/elected_candidates_overall_fr.csv"),
                                       paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed/Output/elected_candidates_overall_it.csv")),
                        recipients = DEFAULT_EMAILS)
        #Set Status Done
        mydb <- connectDB(db_name = "sda_elections")  
        sql_qry <- paste0("UPDATE output_overview SET charts_results = 'done' WHERE election_ID = '",counted_cantons_SR$election_ID[c],"'")
        rs <- dbSendQuery(mydb, sql_qry)
        dbDisconnectAll() 
      }
    }
  }
  ###STAENDERAT ZWISCHENRESULTAT###
  if (nrow(intermediate_cantons_SR) > 0){
    for (c in 1:nrow(intermediate_cantons_SR)) {
      ###Datawrappr
      print(paste0("New intermediate data for ",intermediate_cantons_SR$area_ID[c]," found!"))
      source("SR_publish_candidates_charts_intermediate_DE.R")
      source("SR_publish_candidates_charts_intermediate_FR.R")
      source("SR_publish_candidates_charts_intermediate_IT.R")  
      #Set Data published
      mydb <- connectDB(db_name = "sda_elections")  
      sql_qry <- paste0("UPDATE elections_metadata SET remarks = 'intermediate data published' WHERE election_ID = '",intermediate_cantons_SR$election_ID[c],"'")
      rs <- dbSendQuery(mydb, sql_qry)
      dbDisconnectAll()   
      #Send Mail
      selected_charts <- datawrapper_codes %>%
        filter(election_ID == intermediate_cantons_SR$election_ID[c],
               chart_type == "majorz_votes")
      
      Subject <- paste0("Kanton ",intermediate_cantons_SR$area_name_de[c],": Zwischenergebnis Ständerat zweiter Wahlgang")
      Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                     "Ein Zwischenstand zur Ständeratswahl in ",intermediate_cantons_SR$area_name_de[c]," wurde erfasst. ",
                     "Die daten wurden in folgenden Grafiken eingelesen:\n",
                     "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[1],"/\n",
                     "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[2],"/\n",
                     "https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[3],"/\n\n",
                     "Liebe Grüsse\n\nLENA")
      send_notification(Subject,Body,DEFAULT_EMAILS) 
    }
    
  }  
  
  Sys.sleep(10)
  if (hour(Sys.time()) > 22) {
    break
  }  
}

