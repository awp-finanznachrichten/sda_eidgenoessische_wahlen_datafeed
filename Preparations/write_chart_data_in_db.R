grafiken_uebersicht <- readRDS("grafiken_uebersicht.RDS")
grafiken_uebersicht_history <- readRDS("grafiken_uebersicht_history.RDS")
grafiken_uebersicht_elected <- readRDS("grafiken_uebersicht_elected.RDS")

grafiken_uebersicht <- grafiken_uebersicht[-1,]
grafiken_uebersicht_history <- grafiken_uebersicht_history[-1,]
grafiken_uebersicht_elected <- grafiken_uebersicht_elected[-1,]

grafiken_uebersicht_all <- rbind(grafiken_uebersicht,grafiken_uebersicht_history,grafiken_uebersicht_elected)

#saveRDS(grafiken_uebersicht_all,"grafiken_uebersicht_all.RDS")
#write.xlsx(grafiken_uebersicht_all,"grafiken_uebersicht_all.xlsx",row.names = FALSE)

types <- unique(grafiken_uebersicht_all$Typ)

grafiken_uebersicht_all$Typ <- gsub(types[1],"proporz_overview",grafiken_uebersicht_all$Typ)
grafiken_uebersicht_all$Typ <- gsub(types[2],"proporz_votes",grafiken_uebersicht_all$Typ)
grafiken_uebersicht_all$Typ <- gsub(types[3],"proporz_seats",grafiken_uebersicht_all$Typ)
grafiken_uebersicht_all$Typ <- gsub(types[4],"proporz_history",grafiken_uebersicht_all$Typ)
grafiken_uebersicht_all$Typ <- gsub(types[5],"proporz_elected",grafiken_uebersicht_all$Typ)
grafiken_uebersicht_all$Sprache <- "de"

mydb <- connectDB(db_name="sda_elections")

for (i in 1:nrow(grafiken_uebersicht_all)) {
sql_qry <- paste0("INSERT IGNORE INTO datawrapper_codes(election_ID,chart_type,language,datawrapper_ID) VALUES")
sql_qry <- paste0(sql_qry, paste(sprintf("('%s','%s','%s','%s')",
                                         paste0("2023-10-22_",grafiken_uebersicht_all$Gebiet[i],"_NR"),
                                         grafiken_uebersicht_all$Typ[i],
                                         grafiken_uebersicht_all$Sprache[i],
                                         grafiken_uebersicht_all$ID[i]
                                           ), collapse = ","))
  rs <- dbSendQuery(mydb, sql_qry)
}
  
  
dbDisconnectAll()