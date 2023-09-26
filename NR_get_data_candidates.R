#Get URLs from API
response <-
  GET(BFS_API_URL)
content <- content(response)$result$resources

###GET CANDIDATES DATA###
url_NR_candidates <-
  as.data.frame(do.call(rbind, content))$download_url[[3]] #2

#Get timestamp
timestamp_candidates <-
  toString(as.POSIXlt(HEAD(url_NR_candidates)$date))

#Compare with old timestamp
timestamp_candidates_old <-
  read.csv("./Timestamps/timestamp_candidates.txt", header = FALSE)[1, 1]

if (timestamp_candidates != timestamp_candidates_old) {
#Set Flag
NR_new_elected <- TRUE
  
  #Download data
  setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_daten")
  download.file(url_NR_candidates,
                destfile = "data_NR_candidates.json",
                method = "curl")
  data_NR_candidates <-
    fromJSON("data_NR_candidates.json", flatten = TRUE)
  setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")
  print("new candidates NR data downloaded!")
  
  #Stand CH and Kantone
  timestamp_NR_candidates <- data_NR_candidates$timestamp
  stand_ch_candidates <- data_NR_candidates$stand
  stand_cantons_candidates <- data_NR_candidates$stand_kantone %>%
    rename(kanton_abgeschlossen_candidates = kanton_abgeschlossen)
  

  #Results
  results_NR_cantons_candidates <- data_NR_candidates$level_kantone
  
  #Merge with parties metadata
  results_NR_cantons_candidates <- results_NR_cantons_candidates %>%
    left_join(parties_metadata,
              by = join_by(kandidat_partei_id == bfs_id))
  
  #Merge with location data
  results_NR_cantons_candidates <- results_NR_cantons_candidates %>%
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
  
  ###UPDATE DATABASE###
  #Metadata NR
  ongoing_cantons_NR <- election_metadata %>%
    filter(
      council == "NR",
      date == "2023-10-22",
      status != "candidates finished",
      status != "finished"
    )
  
  #Merge with area data
  ongoing_cantons_NR  <- ongoing_cantons_NR  %>%
    left_join(areas_metadata) %>%
    filter(area_type == "canton")
  
  #Merge with status data
  ongoing_cantons_NR <- ongoing_cantons_NR %>%
    left_join(stand_cantons_candidates, join_by(bfs_ID == kanton_nummer))
  

  #Set variable elected or not
  results_NR_cantons_candidates$flag_gewaehlt <-
    ifelse(results_NR_cantons_candidates$flag_gewaehlt == TRUE, 1, 0)

  for (c in 1:nrow(ongoing_cantons_NR)) {
    if (ongoing_cantons_NR$kanton_abgeschlossen[c] == TRUE) {
      
      #Get candidates results from canton
      results_canton <- results_NR_cantons_candidates %>%
        filter(kanton_nummer == ongoing_cantons_NR$bfs_ID[c])
   
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
          ongoing_cantons_NR$election_ID[c],
          "'"
        )
        rs <- dbSendQuery(mydb, sql_qry)
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
    }
  }
  
#Save Timestamp
cat(timestamp_candidates, file = "./Timestamps/timestamp_candidates.txt")  
  
} else {
  print("no new data for NR candidates found")
}
