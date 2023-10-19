#####STAENDERAT######

###GET ALL SR CANDIDATES
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM candidates_results WHERE date = '2023-10-22' AND council = 'SR'"
    )
  )
candidates_overall_SR <- fetch(rs, n = -1)
dbDisconnectAll()

#Download data
#Get URLs from API
response <-
  GET(BFS_API_URL)
content <- content(response)$result$resources
url_SR_candidates <- trimws(content[[7]]$download_url)
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten"))
data_SR_candidates <-
  fromJSON("data_SR_candidates.json", flatten = TRUE)
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))
print("new candidates SR data downloaded!")

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

non_matches_SR <- candidates_overall_SR %>%
  full_join(results_SR_cantons_candidates) %>%
  filter(is.na(kanton_nummer) |
           is.na(person_id))
print(non_matches_SR)
#####NATIONALRAT######
###GET ALL NR CANDIDATES
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM candidates_results WHERE date = '2023-10-22' AND council = 'NR'"
    )
  )
candidates_overall_NR <- fetch(rs, n = -1)
dbDisconnectAll()


response <-
  GET(BFS_API_URL)
content <- content(response)$result$resources
url_NR_candidates <- trimws(content[[5]]$download_url)
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten"))
data_NR_candidates <-
  fromJSON("data_NR_candidates.json", flatten = TRUE)
setwd(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed"))
print("new candidates NR data downloaded!")

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

non_matches_NR <- candidates_overall_NR %>%
  full_join(results_NR_cantons_candidates) %>%
  filter(is.na(kanton_nummer) |
           is.na(person_id))
print(non_matches_NR)
