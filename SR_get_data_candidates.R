#Get URLs from API
response <-
  GET(BFS_API_URL)
content <- content(response)$result$resources

###GET CANDIDATES DATA###
#url_SR_candidates <-
#  as.data.frame(do.call(rbind, content))$download_url[[9]]
url_SR_candidates <- trimws(content[[7]]$download_url)

#Get timestamp and compare with old one
timestamp_SR_candidates <- headers(HEAD(url_SR_candidates))$`last-modified` 
timestamp_SR_candidates_old <-
  read.csv("./Timestamps/timestamp_SR_candidates.txt", header = FALSE,sep = ";")[1, 1]

if (timestamp_SR_candidates != timestamp_SR_candidates_old) {
#Set Flag
SR_new_elected <- TRUE

  #Download data
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten"))
  download.file(url_SR_candidates,
                destfile = "data_SR_candidates.json",
                method = "curl")
  data_SR_candidates <-
    fromJSON("data_SR_candidates.json", flatten = TRUE)
  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))
  print("new candidates SR data downloaded!")

  #Check: Election done?
  if (data_SR_candidates$stand$wahlgang_abgeschlossen == TRUE) {
    print("All SR candidate results are complete!")
    SR_candidates_finished <- TRUE
  }
  
  #Stand CH and Kantone
  stand_ch_candidates <- data_SR_candidates$stand
  stand_cantons_candidates <- data_SR_candidates$stand_kantone

  #Results
  results_SR_cantons_candidates <- data_SR_candidates$kandidierende %>%
    left_join(parties_metadata,
              by = join_by(kandidat_partei_id == bfs_id)) %>%
    left_join(areas_metadata, join_by(kanton_nummer == bfs_ID))
  
  #Create source ID
  results_SR_cantons_candidates$source_person_id  <- paste0(formatC(results_SR_cantons_candidates$kanton_nummer,width = 2,flag = "0"),       
                                                            results_SR_cantons_candidates$geburtsjahr,
                                                              str_replace_all(results_SR_cantons_candidates$name, " |-|'|/", ""))  
  
  #Replace NA with 0
  results_SR_cantons_candidates[is.na(results_SR_cantons_candidates)] <- 0
  
  ###CHECK CORRECTIONS###
  finished_cantons_SR <- election_metadata %>%
    filter(
      council == "SR",
      date == "2023-10-22",
        status == "finished",
      source_update == "BFS") %>%
    left_join(areas_metadata) %>%
    filter(area_type == "canton")
  
  corrected_cantons_SR <- finished_cantons_SR %>%
    left_join(stand_cantons_candidates, join_by(bfs_ID == kanton_nummer)) %>%
    filter(wahlgang_abgeschlossen == FALSE)
  
  if (nrow(corrected_cantons_SR) > 25) {
    
    #Adapt Metadata
    mydb <- connectDB(db_name = "sda_elections")
    sql_qry <- paste0(
      "UPDATE elections_metadata SET ",
      " status = 'upcoming'",
      ", source_update = 'BFS' WHERE election_ID = '",
      corrected_cantons_SR$election_ID[1],
      "'"
    )
    rs <- dbSendQuery(mydb, sql_qry)
    
    #Correction Alert
    print(paste0("Achtung: Korrektur bei den Ständerats-Resultaten des Kantons ",corrected_cantons_SR$area_name_de[1]," entdeckt!"))
    Subject <- paste0("Achtung: Korrektur bei den Ständerats-Resultaten des Kantons ",corrected_cantons_SR$area_name_de[1]," entdeckt!")
    Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                   "Das BFS hat den bereits als ausgezählt gemeldeten Kanton ",corrected_cantons_SR$area_name_de[1],
                   " wieder deaktiviert. Allenfalls müssen die Ständerats-Ergebnisse korrigiert werden. Bitte direkt mit dem Kanton abklären, was genau korrigiert wurde.\n\n",
                   "Liebe Grüsse\n\nLENA")
    recipients <- DEFAULT_EMAILS
    send_notification(Subject,Body,recipients)  
  }  
  

  ###UPDATE DATABASE###
  #Metadata NR
  ongoing_cantons_SR <- election_metadata %>%
    filter(
      council == "SR",
      date == "2023-10-22",
      status != "finished" |
        source_update != "BFS") %>%
    left_join(areas_metadata) %>%
    filter(area_type == "canton") %>%
    left_join(stand_cantons_candidates, join_by(bfs_ID == kanton_nummer))

  #Set variable elected or not
  results_SR_cantons_candidates$flag_gewaehlt <-
    ifelse(results_SR_cantons_candidates$flag_gewaehlt == TRUE, 1, 0)
  
  if (nrow(ongoing_cantons_SR) > 0) {
  for (c in 1:nrow(ongoing_cantons_SR)) {
    if (ongoing_cantons_SR$wahlgang_abgeschlossen[c] == TRUE) {
      
      print(paste0("new SR candidates results for canton ",ongoing_cantons_SR$area_ID[c]," found!"))
      
      #Get elected candidates results from canton
      results_canton <- results_SR_cantons_candidates %>%
        filter(kanton_nummer == ongoing_cantons_SR$bfs_ID[c])

      #Update candidates results
      mydb <- connectDB(db_name = "sda_elections")
      for (p in 1:nrow(results_canton)) {
        sql_qry <- paste0(
          "UPDATE candidates_results SET ",
          " elected = '",
          results_canton$flag_gewaehlt[p],
          "'",
          ", votes = '",
          results_canton$stimmen_kandidat[p],
          "'",
          ", source_update = 'BFS'",
          " WHERE source_person_id = '",
          results_canton$source_person_id[p],
          "' AND election_ID = '",
          ongoing_cantons_SR$election_ID[c],
          "'"
        )
        rs <- dbSendQuery(mydb, sql_qry)
      }
      
      other_election_needed <- ifelse(ongoing_cantons_SR$zweiter_wahlgang_noetig[c] == TRUE,"yes","no")
      other_election_needed <- ifelse(is.na(other_election_needed) == TRUE,"no",other_election_needed)

      #Adapt Metadata
      sql_qry <- paste0(
        "UPDATE elections_metadata SET ",
        " status = 'finished'",
        ", source_update = 'BFS'",
        ", absolute_majority = 0",
        ", other_election_needed = '",other_election_needed,
        "' WHERE election_ID = '",
        ongoing_cantons_SR$election_ID[c],
        "'"
      )
      rs <- dbSendQuery(mydb, sql_qry)
      
      dbDisconnectAll()

      if ((grepl("App",ongoing_cantons_SR$source_update[c]) == TRUE) & (ongoing_cantons_SR$status[c] == "finished")) {
        
        #Alert if Data from App gets overwritten
        print(paste0("Achtung: Manuelle Eingabe der SR-Endresultate im Kanton ",ongoing_cantons_SR$area_name_de[c]," durch BFS-Daten ersetzt!"))
        Subject <- paste0("Achtung: Manuelle Eingabe der SR-Endresultate im Kanton ",ongoing_cantons_SR$area_name_de[c]," durch BFS-Daten ersetzt!")
        Body <- paste0("Hallo,\n\n",
                       "Die Ständerats-Endresultate vom BFS zu dem bereits von euch manuell erfassten Kanton ",ongoing_cantons_SR$area_name_de[c],
                       " sind eingetroffen und wurden in die Datenbank geschrieben. Bitte kontrollieren, ob die folgenden Daten mit den bereits generierten Outputs übereinstimmen:\n\n",
                       "Resultate Ständerat:\n",
                       paste(paste0(results_canton$vorname," ",results_canton$name," (",results_canton$shortname_de,"): ",results_canton$stimmen_kandidat," Stimmen"),collapse = "\n"),
                       "\n\nLiebe Grüsse\n\nLENA")
        send_notification(Subject,Body,"robot-notification@awp.ch")  
      }
      
    }
  }
  }
#Save Timestamp
cat(timestamp_SR_candidates, file = "./Timestamps/timestamp_SR_candidates.txt")

} else {
  print("no new data for SR candidates found")
}

