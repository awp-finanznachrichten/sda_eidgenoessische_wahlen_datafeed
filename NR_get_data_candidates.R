#Get URLs from API
response <-
  GET(BFS_API_URL)
content <- content(response)$result$resources

###GET CANDIDATES DATA###
url_NR_candidates <-
  as.data.frame(do.call(rbind, content))$download_url[[2]]

#Get timestamp and compare with old one
timestamp_candidates <- headers(HEAD(url_NR_candidates))$`last-modified`
timestamp_candidates_old <-
  read.csv("./Timestamps/timestamp_candidates.txt", header = FALSE,sep = ";")[1, 1]

if (timestamp_candidates != timestamp_candidates_old) {
#Set Flag
NR_new_elected <- TRUE
  
  #Download data
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten"))
  download.file(url_NR_candidates,
                destfile = "data_NR_candidates.json",
                method = "curl")
  data_NR_candidates <-
    fromJSON("data_NR_candidates.json", flatten = TRUE)
  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))
  print("new candidates NR data downloaded!")
  
  #Stand CH and Kantone
  stand_ch_candidates <- data_NR_candidates$stand
  stand_cantons_candidates <- data_NR_candidates$stand_kantone


  #Results
  results_NR_cantons_candidates <- data_NR_candidates$level_kantone %>%
    left_join(parties_metadata,
              by = join_by(kandidat_partei_id == bfs_id)) %>%
    left_join(areas_metadata, join_by(kanton_nummer == bfs_ID))

  #Create source ID
  results_NR_cantons_candidates$source_person_id <-
    paste0(
      formatC(
        results_NR_cantons_candidates$kanton_nummer,
        width = 2,
        flag = "0"
      ),
      formatC(
        results_NR_cantons_candidates$liste_nummer_bfs,
        width = 2,
        flag = "0"
      ),
      formatC(
        results_NR_cantons_candidates$kandidat_nummer,
        width = 2,
        flag = "0"
      )
    )
  
  #Replace NA with 0
  results_NR_cantons_candidates[is.na(results_NR_cantons_candidates)] <- 0

  ###CHECK CORRECTIONS###
  finished_cantons_NR <- election_metadata %>%
    filter(
      council == "NR",
      date == "2023-10-22",
      status == "candidates finished" |
        status == "finished",
      source_update == "BFS") %>%
    left_join(areas_metadata) %>%
    filter(area_type == "canton")
  
  corrected_cantons_NR <- finished_cantons_NR %>%
    left_join(stand_cantons_candidates, join_by(bfs_ID == kanton_nummer)) %>%
    filter(kanton_abgeschlossen == FALSE)

  if (nrow(corrected_cantons_NR) > 0) {
    #Adapt Metadata
    #Status and Remarks
    if (corrected_cantons_NR$status[1] == "candidates finished") {
      status <- "upcoming"
      remarks <- "BFS correction"
    } else {
      status <- "parties finished"
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
    print(paste0("Achtung: Korrektur bei den Nationalrats-Kandidierenden des Kantons ",corrected_cantons_NR$area_name_de[1]," entdeckt!"))
    Subject <- paste0("Achtung: Korrektur bei den Nationalrats-Kandidierenden des Kantons ",corrected_cantons_NR$area_name_de[1]," entdeckt!")
    Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                   "Das BFS hat den bereits als ausgezählt gemeldeten Kanton ",corrected_cantons_NR$area_name_de[1],
                   " wieder deaktiviert. Allenfalls müssen die gewählten Nationalräte korrigiert werden. Bitte direkt mit dem Kanton abklären, was genau korrigiert wurde.\n\n",
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
      status != "candidates finished" |
        source_update != "BFS",
      status != "finished" |
        source_update != "BFS") %>% 
    left_join(areas_metadata) %>%
    filter(area_type == "canton") %>%
    left_join(stand_cantons_candidates, join_by(bfs_ID == kanton_nummer))

  #Set variable elected or not
  results_NR_cantons_candidates$flag_gewaehlt <-
    ifelse(results_NR_cantons_candidates$flag_gewaehlt == TRUE, 1, 0)

  for (c in 1:nrow(ongoing_cantons_NR)) {
    if (ongoing_cantons_NR$kanton_abgeschlossen[c] == TRUE) {
      
      print(paste0("new candidates results for canton ",ongoing_cantons_NR$area_ID[c]," found!"))
      
      #Get elected candidates results from canton
      results_canton <- results_NR_cantons_candidates %>%
        filter(kanton_nummer == ongoing_cantons_NR$bfs_ID[c])
               #,flag_gewaehlt == 1)

      #Check if amount of distributed seats is correct  
      seats_check <- nrow(results_canton) == ongoing_cantons_NR$seats_available_NR[c]
      
      if (seats_check == TRUE) {
      #Update candidates results
      print(paste0("Enter the results of ",nrow(results_canton)," candidates..."))  
        
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
          ongoing_cantons_NR$election_ID[c],
          "'"
        )
        rs <- dbSendQuery(mydb, sql_qry)
        print(p)
      }
      
      if (ongoing_cantons_NR$status[c] == "parties finished") {
        status <- "finished"
        remarks <- "candidates and parties available"
      } else {
        status <- "candidates finished"
        remarks <- "parties not available yet"
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
        print(paste0("WARNING: Amount of elected NR candidates does not match overall seats available in ",ongoing_cantons_NR$area_ID[c],"! The data was not entered in DB!"))   
      }  
    }
  }
  
#Save Timestamp
cat(timestamp_candidates, file = "./Timestamps/timestamp_candidates.txt") 
  
} else {
  print("no new data for NR candidates found")
}
