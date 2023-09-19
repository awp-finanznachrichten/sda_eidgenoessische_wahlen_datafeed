#Get parties results from Canton
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    "SELECT * FROM parties_results")
results_parties <- fetch(rs, n = -1)
dbDisconnectAll()


#Merge with metadata, location metadata, party metadata and make selections
results_parties <- results_parties %>%
  select(election_ID,party_ID,seats,seats_change,voter_share,voter_share_change,final_results) %>%
  left_join(election_metadata,
            ) %>%
  left_join(areas_metadata) %>%
  filter(area_type == "canton") %>%
  left_join(parties_metadata,
            by = join_by(party_ID == id)) %>%
  filter(date == "2023-10-22",
         council == "NR"
         )

###OVERVIEW CH
overview_ch <- results_parties %>%
  #filter(status == "finished" |
  #         status == "parties finished") %>%
  group_by(party_ID) %>%
  summarise(shortname_de = max(shortname_de),
            shortname_fr = max(shortname_fr),
            shortname_it = max(shortname_it),
    seats_2023 = sum(seats,na.rm = TRUE),
    seats_2019 = sum(seats,na.rm = TRUE) - sum(seats_change,na.rm = TRUE),
    seats_change = sum(seats_change)) %>%
  filter(seats_2023 != 0 |
           seats_change != 0 ) %>%
  arrange(desc(seats_change))

###OVERVIEW CANTONS
overview_cantons <- data.frame("area_ID","status","title_de","content_de","title_fr","content_fr","title_it","content_it")
colnames(overview_cantons) <- c("area_ID","status","title_de","content_de","title_fr","content_fr","title_it","content_it")

cantons <- unique(results_parties$area_ID)

for (canton in cantons) {

#Results Nationalrat
data_canton_NR <- results_parties %>%
  filter(area_ID == canton,
         voter_share > 0,
         seats != 0 |
           seats_change != 0 |
           voter_share >= 3) %>%
  arrange(desc(seats),
          desc(voter_share))

content_de <- paste0("<b>Nationalrat (",data_canton_NR$seats_available_NR[1]," Sitze)</b><br><br>")
content_fr <- ""
content_it <- ""

#Results Ständerat
###TO DO###

check_NR <- FALSE
check_SR <- FALSE
if (data_canton_NR$status[1] != "finished" | data_canton_NR$status[1] != "parties finished") { #ADAPT
check_NR <- TRUE
tabelle_de <- create_table_overview(data_canton_NR,"de")
content_de <- paste0(content_de,tabelle_de,"<br>")
} else {
content_de  <- paste0(content_de,"<br>Es sind noch keine Resultate vorhanden.<br>")
}  

#Results Ständerat
content_de <- paste0(content_de,"<b>Ständerat (",data_canton_NR$seats_available_SR[1]," Sitze)</b><br>")

#if (data_canton_SR$status[1] == "finished") {
#check_SR <- TRUE
###TO DO###
#} else {
content_de  <- paste0(content_de,"<br>Es sind noch keine Resultate vorhanden.<br><br>")
#}  

#Status
if ((check_NR == TRUE) & (check_SR == TRUE)) {
status <- "all_counted"  
} else if  ((check_NR == TRUE) & (check_SR == FALSE)) {
status <- "NR_counted"  
} else if  ((check_NR == FALSE) & (check_SR == TRUE)) {
status <- "SR_counted"  
} else {
status <- "no_data"  
}  

#Simulation Status
status_all <- c("all_counted","NR_counted","SR_counted","no_data")
status <- status_all[sample(1:4,1)]


new_entry <- data.frame(canton,
                        status,
                        data_canton_NR$area_name_de[1],
                        content_de,
                        data_canton_NR$area_name_fr[1],
                        content_fr,
                        data_canton_NR$area_name_it[1],
                        content_it)
colnames(new_entry) <- c("area_ID","status","title_de","content_de","title_fr","content_fr","title_it","content_it")
overview_cantons <- rbind(overview_cantons,new_entry)
}    
overview_cantons <- overview_cantons[-1,]
overview_cantons$content_de <- gsub("1 Sitze","1 Sitz",overview_cantons$content_de)
overview_cantons$content_fr <- gsub("1 sièges","1 siège",overview_cantons$content_fr)
overview_cantons$content_it <- gsub("1 seggi","1 seggio",overview_cantons$content_it)

overview_cantons$content_de <- gsub("[<]","$",overview_cantons$content_de)
overview_cantons$content_de <- gsub("[>]","£",overview_cantons$content_de)
overview_cantons$content_de <- gsub("[;]","¢",overview_cantons$content_de)
overview_cantons$content_fr <- gsub("[<]","$",overview_cantons$content_fr)
overview_cantons$content_fr <- gsub("[>]","£",overview_cantons$content_fr)
overview_cantons$content_fr <- gsub("[;]","¢",overview_cantons$content_fr)
overview_cantons$content_it <- gsub("[<]","$",overview_cantons$content_it)
overview_cantons$content_it <- gsub("[>]","£",overview_cantons$content_it)
overview_cantons$content_it <- gsub("[;]","¢",overview_cantons$content_it)

write.csv(overview_cantons,file="./Output/ergebnisse_kantone_uebersicht.csv",row.names = FALSE)