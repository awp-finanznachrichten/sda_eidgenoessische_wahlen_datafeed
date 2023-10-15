 #Get candidates data
  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten"))
  data_NR_candidates <-
    fromJSON("data_NR_candidates.json", flatten = TRUE)
  setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))
  
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
  
  #Replace NA with 0
  results_NR_cantons_candidates[is.na(results_NR_cantons_candidates)] <- 0
  
  ###UPDATE DATABASE###
    results_NR_cantons_candidates$flag_gewaehlt <-
    ifelse(results_NR_cantons_candidates$flag_gewaehlt == TRUE, 1, 0)

      print(paste0("Enter complete results for canton ",counted_cantons$area_ID[c],"!"))
      
      #Get not elected candidates results from canton
      results_canton <- results_NR_cantons_candidates %>%
        filter(kanton_nummer == counted_cantons$bfs_ID[c],
               flag_gewaehlt == 0)
      
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
          counted_cantons$election_ID[c],
          "'"
        )
        rs <- dbSendQuery(mydb, sql_qry)
      }
      dbDisconnectAll()

