#Get URLs from API
response <-
  GET(BFS_API_URL)
content <- httr::content(response)$result$resources

###GET RESULTS DATA###
url_NR_results <-
  as.data.frame(do.call(rbind, content))$download_url[[1]]

  #Download data

  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten"))
  download.file(url_NR_results,
                destfile = "data_NR_results.json",
                method = "curl")
  data_NR_results <-
    fromJSON("data_NR_results.json", flatten = TRUE)
  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))
  print("new results NR data downloaded!")
  

  #Results
  results_NR_ch <- data_NR_results$level_ch

  #Merge with parties metadata
  results_NR_ch <- results_NR_ch %>%
    left_join(parties_metadata,
              by = join_by(partei_id == bfs_id))
  
#Replace NA with 0
results_NR_ch[is.na(results_NR_ch)] <- 0

      print(paste0("final election results found!"))

      #Update party results
      mydb <- connectDB(db_name = "sda_elections")
      for (p in 1:nrow(results_NR_ch)) {
        sql_qry <- paste0(
          "UPDATE parties_results SET ",
          " seats = '",
          results_NR_ch$anzahl_gewaehlte[p],
          "'",
          ", seats_change = '",
          results_NR_ch$differenz_anzahl_gewaehlte[p],
          "'",
          ", voter_share = '",
          results_NR_ch$partei_staerke[p],
          "'",
          ", voter_share_change = '",
          results_NR_ch$differenz_partei_staerke[p],
          "'",
          ", final_results = '1'",
          ", source_update = 'BFS'",
          " WHERE election_ID = '2023-10-22_CH_NR' AND area_ID = 'CH' AND party_ID = '",
          results_NR_ch$id[p],
          "'"
        )
        rs <- dbSendQuery(mydb, sql_qry)
      }

      #Adapt Metadata
      sql_qry <- paste0(
        "UPDATE elections_metadata SET ",
        " status = 'finished'",
        ", source_update = 'BFS'",
        " WHERE election_ID = '2023-10-22_CH_NR'"
      )
      rs <- dbSendQuery(mydb, sql_qry)
      
      dbDisconnectAll()
      
  #Load Databases again
  source("load_databases.R")

