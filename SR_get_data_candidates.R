#Get URLs from API
response <-
  GET(BFS_API_URL)
content <- content(response)$result$resources

###GET CANDIDATES DATA###
url_SR_candidates <-
  as.data.frame(do.call(rbind, content))$download_url[[9]]

#Get timestamp
timestamp_candidates_SR <-
  toString(as.POSIXlt(HEAD(url_SR_candidates)$date))

#Compare with old timestamp
timestamp_candidates_SR_old <-
  read.csv("./Timestamps/timestamp_candidates_SR.txt", header = FALSE)[1, 1]

if (timestamp_candidates_SR != timestamp_candidates_SR_old) {
#Set Flag
SR_new_elected <- TRUE
  
  #Download data
  setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_daten")
  download.file(url_SR_candidates,
                destfile = "data_SR_candidates.json",
                method = "curl")
  data_SR_candidates <-
    fromJSON("data_SR_candidates.json", flatten = TRUE)
  setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")
  print("new candidates SR data downloaded!")
  
  #Stand CH and Kantone
  timestamp_SR_candidates <- data_SR_candidates$timestamp
  stand_ch_candidates <- data_SR_candidates$stand
  stand_cantons_candidates <- data_SR_candidates$stand_kantone

  #Results
  results_SR_cantons_candidates <- data_SR_candidates$kandidierende
  
  #Merge with parties metadata
  results_SR_cantons_candidates <- results_SR_cantons_candidates %>%
    left_join(parties_metadata,
              by = join_by(kandidat_partei_id == bfs_id))
  

  #Merge with location data
  results_SR_cantons_candidates <- results_SR_cantons_candidates %>%
    left_join(areas_metadata, join_by(kanton_nummer == bfs_ID))
  
  #Create source ID
  results_SR_cantons_candidates$source_person_id  <- paste0(formatC(results_SR_cantons_candidates$kanton_nummer,width = 2,flag = "0"),       
                                                            results_SR_cantons_candidates$geburtsjahr,
                                                              str_replace_all(results_SR_cantons_candidates$name, " |-|'|/", ""))  
  

  #Replace NA with 0
  results_SR_cantons_candidates[is.na(results_SR_cantons_candidates)] <- 0

  ###UPDATE DATABASE###
  #Metadata NR
  ongoing_cantons_SR <- election_metadata %>%
    filter(
      council == "SR",
      date == "2023-10-22",
      status != "finished"
    )
  
  #Merge with area data
  ongoing_cantons_SR  <- ongoing_cantons_SR  %>%
    left_join(areas_metadata) %>%
    filter(area_type == "canton")
  
  #Merge with status data
  ongoing_cantons_SR <- ongoing_cantons_SR %>%
    left_join(stand_cantons_candidates, join_by(bfs_ID == kanton_nummer))

  #Set variable elected or not
  results_SR_cantons_candidates$flag_gewaehlt <-
    ifelse(results_SR_cantons_candidates$flag_gewaehlt == TRUE, 1, 0)

  for (c in 1:nrow(ongoing_cantons_SR)) {
    if (ongoing_cantons_SR$wahlgang_abgeschlossen[c] == FALSE) {
      
      print(paste0("new candidates results for canton ",ongoing_cantons_SR$area_ID[c]," found!"))
      
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
      
      #Adapt Metadata
      sql_qry <- paste0(
        "UPDATE elections_metadata SET ",
        " status = 'finished'",
        ", source_update = 'BFS'",
        " WHERE election_ID = '",
        ongoing_cantons_SR$election_ID[c],
        "'"
      )
      rs <- dbSendQuery(mydb, sql_qry)
      
      dbDisconnectAll()
    }
  }
  
#Save Timestamp
cat(timestamp_candidates_SR, file = "./Timestamps/timestamp_candidates_SR.txt")  
  
} else {
  print("no new data for NR candidates found")
}

View(results_SR_cantons_candidates)
