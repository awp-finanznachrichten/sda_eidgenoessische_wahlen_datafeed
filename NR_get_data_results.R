#Get URLs from API
response <-
  GET(BFS_API_URL)
content <- httr::content(response)$result$resources

###GET RESULTS DATA###
url_NR_results <-
  as.data.frame(do.call(rbind, content))$download_url[[1]]

#Get timestamp and compare with old one
timestamp_results <- headers(HEAD(url_NR_results))$`last-modified`
timestamp_results_old <-
  read.csv("./Timestamps/timestamp_results.txt", header = FALSE,sep = ";")[1, 1]

if (timestamp_results != timestamp_results_old) {
 
#Set Flag
NR_new_results <- TRUE  
  
  #Download data

  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten"))
  download.file(url_NR_results,
                destfile = "data_NR_results.json",
                method = "curl")
  data_NR_results <-
    fromJSON("data_NR_results.json", flatten = TRUE)
  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))
  print("new results NR data downloaded!")
  
  #Check: Election done?
  if (data_NR_results$stand$wahl_abgeschlossen == TRUE) {
  NR_finished <- TRUE
  }  
  
  #Timestamps
  stand_cantons_results <- data_NR_results$stand_kantone
  
  #Results
  results_NR_cantons <- data_NR_results$level_kantone

  #Merge with parties metadata
  results_NR_cantons <- results_NR_cantons %>%
    left_join(parties_metadata,
              by = join_by(partei_id == bfs_id))
  
#Replace NA with 0
results_NR_cantons[is.na(results_NR_cantons)] <- 0

###CHECK CORRECTIONS###
finished_cantons_NR <- election_metadata %>%
  filter(
    council == "NR",
    date == "2023-10-22",
    status == "parties finished" |
    status == "finished",
    source_update == "BFS"
  )

finished_cantons_NR <- finished_cantons_NR  %>%
  left_join(areas_metadata) %>%
  filter(area_type == "canton")
corrected_cantons_NR <- finished_cantons_NR %>%
  left_join(stand_cantons_results, join_by(bfs_ID == kanton_nummer)) %>%
  filter(kanton_abgeschlossen == FALSE)

if (nrow(corrected_cantons_NR) > 0) {

#Adapt Metadata
  #Status and Remarks
  if (corrected_cantons_NR$status[1] == "parties finished") {
    status <- "upcoming"
    remarks <- "BFS correction"
  } else {
    status <- "candidates finished"
    remarks <- "BFS correction"
  }
  
  #Adapt Metadata
  mydb <- connectDB(db_name = "sda_elections")
  sql_qry <- paste0(
    "UPDATE elections_metadata SET ",
    " status = '",
    status,
    "'",
    ", source_update = 'BFS'",
    ", remarks = '",
    remarks,
    "'",
    " WHERE election_ID = '",
    corrected_cantons_NR$election_ID[1],
    "'"
  )
  rs <- dbSendQuery(mydb, sql_qry)
  
  dbDisconnectAll()  
  
#Correction Alert
print(paste0("Achtung: Korrektur bei den Nationalrats-Resultaten des Kantons ",corrected_cantons_NR$area_name_de[1]," entdeckt!"))
Subject <- paste0("Achtung: Korrektur bei den Nationalrats-Resultaten des Kantons ",corrected_cantons_NR$area_name_de[1]," entdeckt!")
Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                 "Das BFS hat den bereits als ausgezählt gemeldeten Kanton ",corrected_cantons_NR$area_name_de[1],
               " wieder deaktiviert. Allenfalls müssen die Sitzverteilung oder die Wähleranteile korrigiert werden. Bitte direkt mit dem Kanton abklären, was genau korrigiert wurde.\n\n",
                 "Liebe Grüsse\n\nLENA")
recipients <- "robot-notification@awp.ch, contentdevelopment@keystone-sda.ch"
#send_notification(Subject,Body,recipients)  
}  


###UPDATE DATABASE###
  #Metadata NR
  ongoing_cantons_NR <- election_metadata %>%
    filter(
      council == "NR",
      date == "2023-10-22",
      status != "parties finished" | 
        source_update != "BFS" ,
      status != "finished" | 
        source_update != "BFS"
    ) %>%
    left_join(areas_metadata) %>%
    filter(area_type == "canton") %>%
    left_join(stand_cantons_results, join_by(bfs_ID == kanton_nummer))

  for (c in 1:nrow(ongoing_cantons_NR)) {
    if (ongoing_cantons_NR$kanton_abgeschlossen[c] == TRUE) {
      
      print(paste0("new party results for canton ",ongoing_cantons_NR$area_ID[c]," found!"))
      
      #Get party results from canton
      results_canton <- results_NR_cantons %>%
        filter(kanton_nummer == ongoing_cantons_NR$bfs_ID[c])

      #Make initial party entries
     #mydb <- connectDB(db_name="sda_elections")
     #sql_qry <- paste0("INSERT IGNORE INTO parties_results(election_ID,area_ID,party_ID) VALUES")
     #sql_qry <- paste0(sql_qry, paste(sprintf("('%s','%s','%s')",
    #                                           ongoing_cantons_NR$election_ID[c],
    #                                           ongoing_cantons_NR$area_ID[c],
    #                                           results_canton$id
    #                                           ), collapse = ","))
    #  rs <- dbSendQuery(mydb, sql_qry)
    #  dbDisconnectAll()
    
    #Check if amount of distributed seats is correct  
    seats_check <- sum(results_canton$anzahl_gewaehlte) == ongoing_cantons_NR$seats_available_NR[c]

    if (seats_check == TRUE) {
      #Update party results
      mydb <- connectDB(db_name = "sda_elections")
      for (p in 1:nrow(results_canton)) {
        sql_qry <- paste0(
          "UPDATE parties_results SET ",
          " seats = '",
          results_canton$anzahl_gewaehlte[p],
          "'",
          ", seats_change = '",
          results_canton$differenz_anzahl_gewaehlte[p],
          "'",
          ", voter_share = '",
          results_canton$partei_staerke[p],
          "'",
          ", voter_share_change = '",
          results_canton$differenz_partei_staerke[p],
          "'",
          ", final_results = '1'",
          ", source_update = 'BFS'",
          " WHERE election_ID = '",
          ongoing_cantons_NR$election_ID[c],
          "' AND area_ID = '",
          ongoing_cantons_NR$area_ID[c],
          "' AND party_ID = '",
          results_canton$id[p],
          "'"
        )
        rs <- dbSendQuery(mydb, sql_qry)
      }
      
      #Status and Remarks
      if (ongoing_cantons_NR$status[c] == "candidates finished") {
        status <- "finished"
        remarks <- "candidates and parties available"
      } else {
        status <- "parties finished"
        remarks <- "candidates not available yet"
      }
      
      #Adapt Metadata
      sql_qry <- paste0(
        "UPDATE elections_metadata SET ",
        " status = '",
        status,
        "'",
        ", source_update = 'BFS'",
        ", remarks = '",
        remarks,
        "'",
        " WHERE election_ID = '",
        ongoing_cantons_NR$election_ID[c],
        "'"
      )
      rs <- dbSendQuery(mydb, sql_qry)
      
      dbDisconnectAll()
    } else {
      print(paste0("WARNING: Amount of seats does not match overall seats available in ",ongoing_cantons_NR$area_ID[c],"! The data was not entered in DB!"))
    }  
    }
  }
  
  #Save Timestamp
  cat(timestamp_results, file = "./Timestamps/timestamp_results.txt")
  
  #Load Databases again
  source("load_databases.R")
  
} else {
  print("no new data for NR results found")
}

###GET VOTERTURNOUT DATA###
url_NR_voterturnout <-
  as.data.frame(do.call(rbind, content))$download_url[[4]]

#Get timestamp and compare with old one
timestamp_voterturnout <- headers(HEAD(url_NR_voterturnout))$`last-modified`
timestamp_voterturnout_old <-
  read.csv("./Timestamps/timestamp_voterturnout.txt", header = FALSE,sep = ";")[1, 1]

if (timestamp_voterturnout != timestamp_voterturnout_old) {
  print("new voterturnout NR data downloaded!")
  #Download data
  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten"))
  download.file(url_NR_voterturnout,
                destfile = "data_NR_voterturnout.json",
                method = "curl")
  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))
  
  #Save Timestamp
  cat(timestamp_voterturnout, file = "./Timestamps/timestamp_voterturnout.txt")
      
} else {
  print("no new data for NR voterturnout found")
}
