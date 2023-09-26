#Get URLs from API
response <-
  GET(BFS_API_URL)
content <- content(response)$result$resources

###GET RESULTS DATA###
url_NR_results <-
  as.data.frame(do.call(rbind, content))$download_url[[4]]

#Get timestamp
timestamp_results <-
  toString(as.POSIXlt(HEAD(url_NR_results)$date))

#Compare with old timestamp
timestamp_results_old <-
  read.csv("./Timestamps/timestamp_results.txt", header = FALSE)[1, 1]

if (timestamp_results != timestamp_results_old) {
  
#Set Flag
new_results <- TRUE  
  
  #Download data
  setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_daten")
  download.file(url_NR_results,
                destfile = "data_NR_results.json",
                method = "curl")
  data_NR_results <-
    fromJSON("data_NR_results.json", flatten = TRUE)
  setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")
  print("new results NR data downloaded!")
  #Timestamps
  stand_cantons_results <- data_NR_results$stand_kantone #%>%
    #rename(kanton_abgeschlossen_parties = kanton_abgeschlossen)
  
  #Results
  results_NR_cantons <- data_NR_results$level_kantone

  #Merge with parties metadata
  results_NR_cantons <- results_NR_cantons %>%
    left_join(parties_metadata,
              by = join_by(partei_id == bfs_id))
  
  ###UPDATE DATABASE###
  #Metadata NR
  ongoing_cantons_NR <- election_metadata %>%
    filter(
      council == "NR",
      date == "2023-10-22",
      status != "parties finished",
      status != "finished"
    )
  
  #Merge with area data
  ongoing_cantons_NR  <- ongoing_cantons_NR  %>%
    left_join(areas_metadata) %>%
    filter(area_type == "canton")
  
  #Merge with status data
  ongoing_cantons_NR <- ongoing_cantons_NR %>%
    left_join(stand_cantons_results, join_by(bfs_ID == kanton_nummer))
  

  for (c in 1:nrow(ongoing_cantons_NR)) {
    if (ongoing_cantons_NR$kanton_abgeschlossen[c] == TRUE) {
      #Get party results from canton
      results_canton <- results_NR_cantons %>%
        filter(kanton_nummer == ongoing_cantons_NR$bfs_ID[c])

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
  as.data.frame(do.call(rbind, content))$download_url[[2]]

#Get timestamp
timestamp_voterturnout <-
  toString(as.POSIXlt(HEAD(url_NR_voterturnout)$date))

#Compare with old timestamp
timestamp_voterturnout_old <-
  read.csv("./Timestamps/timestamp_voterturnout.txt", header = FALSE)[1, 1]

if (timestamp_voterturnout != timestamp_voterturnout_old) {
  print("new voterturnout NR data downloaded!")
  #Download data
  setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_daten")
  download.file(url_NR_results,
                destfile = "data_NR_voterturnout.json",
                method = "curl")
  setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")
  
} else {
  print("no new data for NR voterturnout found")
}
