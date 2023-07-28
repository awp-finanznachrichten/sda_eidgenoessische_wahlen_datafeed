#Metadata NR
ongoing_cantons_NR <- election_metadata %>%
  filter(council == "NR",
         date == "2023-10-22",
         status != "candidates finished",
         status != "finished")

#Merge with area data
ongoing_cantons_NR  <- ongoing_cantons_NR  %>%
  left_join(areas_metadata) %>%
  filter(area_type == "canton")

#Merge with status data
ongoing_cantons_NR <- ongoing_cantons_NR %>% 
  left_join(stand_cantons_candidates, join_by(bfs_ID==kanton_nummer))

#Give candidates ID
results_NR_cantons_candidates$person_id <- c(90000:(89999+nrow(results_NR_cantons_candidates))) 
results_NR_cantons_candidates$flag_gewaehlt <- ifelse(results_NR_cantons_candidates$flag_gewaehlt == TRUE,1,0)

for (c in 1:nrow(ongoing_cantons_NR)) {
if (ongoing_cantons_NR$kanton_abgeschlossen[c] == TRUE) {  

#Get candidates results from canton
results_canton <- results_NR_cantons_candidates %>%
  filter(kanton_nummer == ongoing_cantons_NR$bfs_ID[c])

#Make initial candidate entries
#mydb <- connectDB(db_name="sda_elections")
#sql_qry <- paste0("INSERT IGNORE INTO candidates_results(person_id,election_id,year,council,date,canton,area_id,party_id,list_id,status,source_update,source_person_id) VALUES")
#sql_qry <- paste0(sql_qry, paste(sprintf("('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')",
#                                         results_canton$person_id,
#                                         ongoing_cantons_NR$election_ID[c],
#                                         "2023",
#                                         "NR",
#                                         "2023-10-22",
#                                         ongoing_cantons_NR$area_ID[c],
#                                         ongoing_cantons_NR$area_ID[c],
#                                         results_canton$id,
#                                         results_canton$liste_nummer_bfs,
#                                         results_canton$kandidat_status_id,
#                                         "BFS Testdata",
#                                         results_canton$source_person_id
#                                         ), collapse = ","))
#rs <- dbSendQuery(mydb, sql_qry)
#dbDisconnectAll()

#Update party results
mydb <- connectDB(db_name="sda_elections")
for (p in 1:nrow(results_canton)) {

sql_qry <-paste0("UPDATE candidates_results SET ",
                 " elected = '",results_canton$flag_gewaehlt[p],"'",
                 ", votes = '",results_canton$stimmen_kandidat[p],"'",
                 ", source_update = 'BFS Testdata'",
                 " WHERE person_id = '",results_canton$person_id[p],
                 "' AND election_ID = '",ongoing_cantons_NR$election_ID[c],"'")
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
sql_qry <-paste0("UPDATE elections_metadata SET ",
                 " status = '",status,"'",
                 ", source_update = 'BFS Testdata'",
                 ", remarks = '",remarks,"'",
                 " WHERE election_ID = '",ongoing_cantons_NR$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)

dbDisconnectAll()
}  
}

