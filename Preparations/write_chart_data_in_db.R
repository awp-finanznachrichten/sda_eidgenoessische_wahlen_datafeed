grafiken_uebersicht <- readRDS("./Preparations/grafiken_uebersicht_it.RDS")
grafiken_uebersicht_history <- readRDS("./Preparations/grafiken_uebersicht_history_it.RDS")
grafiken_uebersicht_elected <- readRDS("./Preparations/grafiken_uebersicht_elected_it.RDS")
grafiken_uebersicht_candidates_SR <- readRDS("./Preparations/grafiken_uebersicht_it_candidates_SR.RDS")

grafiken_uebersicht <- grafiken_uebersicht[-1,]
grafiken_uebersicht_fr <- grafiken_uebersicht_fr[-1,]
grafiken_uebersicht_it <- grafiken_uebersicht_it[-1,]

grafiken_uebersicht_all <- rbind(grafiken_uebersicht,grafiken_uebersicht_fr,grafiken_uebersicht_it)


grafiken_uebersicht_all$Typ <- "majorz_votes"


mydb <- connectDB(db_name="sda_elections")

for (i in 1:nrow(grafiken_uebersicht_all)) {
sql_qry <- paste0("INSERT IGNORE INTO datawrapper_codes(election_ID,chart_type,language,datawrapper_ID) VALUES")
sql_qry <- paste0(sql_qry, paste(sprintf("('%s','%s','%s','%s')",
                                         ifelse(grafiken_uebersicht_all$Typ[i] == "majorz_votes",
                                                paste0("2023-11-19_",grafiken_uebersicht_all$Gebiet[i],"_SR"),
                                                paste0("2023-10-22_",grafiken_uebersicht_all$Gebiet[i],"_NR")),
                                         grafiken_uebersicht_all$Typ[i],
                                         substr(grafiken_uebersicht_all$Sprache[i],1,2),
                                         grafiken_uebersicht_all$ID[i]
                                           ), collapse = ","))
  rs <- dbSendQuery(mydb, sql_qry)
}
  
dbDisconnectAll()


grafiken_uebersicht_all <- dw_list_charts()

