#Metadata NR
ongoing_cantons_NR <- election_metadata %>%
  filter(council == "NR",
         date == "2023-10-22",
         status != "parties finished",
         status != "finished")

#Merge with area data
ongoing_cantons_NR  <- ongoing_cantons_NR  %>%
  left_join(areas_metadata) %>%
  filter(area_type == "canton")

#Merge with status data
ongoing_cantons_NR <- ongoing_cantons_NR %>% 
  left_join(stand_cantons, join_by(bfs_ID==kanton_nummer))

for (c in 1:nrow(ongoing_cantons_NR)) {
if (ongoing_cantons_NR$kanton_abgeschlossen[c] == TRUE) {  

#Get party results from canton
results_canton <- results_NR_cantons %>%
  filter(kanton_nummer == ongoing_cantons_NR$bfs_ID[c])

#Make initial party entries
#mydb <- connectDB(db_name="sda_elections")
#sql_qry <- paste0("INSERT IGNORE INTO parties_results(election_ID,area_ID,party_ID) VALUES")
#sql_qry <- paste0(sql_qry, paste(sprintf("('%s','%s','%s')",
#                                         ongoing_cantons_NR$election_ID[c],
#                                         ongoing_cantons_NR$area_ID[c],
#                                         results_canton$id
#                                         ), collapse = ","))
#rs <- dbSendQuery(mydb, sql_qry)
#dbDisconnectAll()


#Update party results
mydb <- connectDB(db_name="sda_elections")
for (p in 1:nrow(results_canton)) {

sql_qry <-paste0("UPDATE parties_results SET ",
                 " seats = '",results_canton$anzahl_gewaehlte[p],"'",
                 ", seats_change = '",results_canton$differenz_anzahl_gewaehlte[p],"'",
                 ", voter_share = '",results_canton$partei_staerke[p],"'",
                 ", voter_share_change = '",results_canton$differenz_partei_staerke[p],"'",
                 ", final_results = '1'",
                 ", source_update = 'BFS Testdata'",
                 " WHERE election_ID = '",ongoing_cantons_NR$election_ID[c],
                 "' AND area_ID = '",ongoing_cantons_NR$area_ID[c],
                 "' AND party_ID = '",results_canton$id[p],"'")
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
sql_qry <-paste0("UPDATE elections_metadata SET ",
                 " status = '",status,"'",
                 ", source_update = 'BFS Testdata'",
                 ", remarks = '",remarks,"'",
                 " WHERE election_ID = '",ongoing_cantons_NR$election_ID[c],"'")
rs <- dbSendQuery(mydb, sql_qry)

dbDisconnectAll()
}  
}

