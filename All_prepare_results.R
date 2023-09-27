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
  filter(status == "finished" |
           status == "parties finished") %>%
  group_by(party_ID) %>%
  summarise(shortname_de = max(shortname_de,na.rm = TRUE),
            shortname_fr = max(shortname_fr,na.rm = TRUE),
            shortname_it = max(shortname_it,na.rm = TRUE),
    seats_2023 = sum(seats,na.rm = TRUE),
    seats_2019 = sum(seats,na.rm = TRUE) - sum(seats_change,na.rm = TRUE),
    seats_change = sum(seats_change),
    position_parliament = max(position_parliament,na.rm=TRUE)) %>%
  filter(seats_2023 != 0 |
           seats_change != 0 ) %>%
  arrange(desc(seats_change))

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

#Get candidates SR
results_candidates_SR <- candidates_overall_SR %>%
  mutate(status_text = ifelse(status == 2,"bisher","neu"),
         status_text_fr = ifelse(status == 2,"sortant", "nouveau"),
         status_text_it = ifelse(status == 2,"uscente", "nuovo")) %>%
  select(person_id,election_id,area_id,party_id,votes,elected,status_text,status_text_fr,status_text_it) %>%
  left_join(election_metadata, join_by(election_id == election_ID)) %>%
  left_join(people_metadata, join_by(person_id == id)) %>%
  left_join(parties_metadata, join_by (party_id == id)) %>%
  left_join(areas_metadata, join_by (area_id == area_ID)) %>%
  arrange(desc(votes),
          lastname)

###OVERVIEW CANTONS
overview_cantons <- data.frame("area_ID","status","title_de","content_de","title_fr","content_fr","title_it","content_it")
colnames(overview_cantons) <- c("area_ID","status","title_de","content_de","title_fr","content_fr","title_it","content_it")

cantons <- unique(results_parties$area_ID)

for (canton in cantons) {

data_canton_all <- results_parties %>%
    filter(area_ID == canton)  

#Results Nationalrat
data_canton_NR <- results_parties %>%
  filter(area_ID == canton,
         voter_share > 0,
         seats != 0 |
           seats_change != 0 |
           voter_share >= 3) %>%
  arrange(desc(seats),
          desc(voter_share))

content_de <- paste0("<b>Nationalrat (",data_canton_all$seats_available_NR[1]," Sitze)</b><br>")
content_fr <- ""
content_it <- ""

#Results Ständerat
data_canton_all_SR <- results_candidates_SR %>%
  filter(area_ID == canton)  

elected_candidates_SR <- results_candidates_SR %>%
  filter(area_id == canton,
         elected == 1)

check_NR <- FALSE
check_SR <- FALSE
if (data_canton_all$status[1] == "finished" | data_canton_all$status[1] == "parties finished") { 
check_NR <- TRUE
tabelle_de <- create_table_overview(data_canton_NR,"de")
content_de <- paste0(content_de,tabelle_de,"<br>")
} else {
content_de  <- paste0(content_de,"Es sind noch keine Resultate vorhanden.<br><br>")
}  

#Results Ständerat
content_de <- paste0(content_de,"<b>Ständerat (",data_canton_all$seats_available_SR[1]," Sitze)</b><br>")

if (data_canton_all_SR$status[1] == "finished") {
check_SR <- TRUE

if (nrow(elected_candidates_SR) == 2) {
content_de <- paste0(content_de,"Gewählt sind:<br>",
                       elected_candidates_SR$firstname[1]," ",elected_candidates_SR$lastname[1],
                       " (",elected_candidates_SR$shortname_de[1],", ",elected_candidates_SR$status_text[1],")<br>",
                     elected_candidates_SR$firstname[2]," ",elected_candidates_SR$lastname[2],
                     " (",elected_candidates_SR$shortname_de[2],", ",elected_candidates_SR$status_text[2],")")  
} else if (nrow(elected_candidates_SR) == 1) {
content_de <- paste0(content_de,"Gewählt ist ",
                     elected_candidates_SR$firstname[1]," ",elected_candidates_SR$lastname[1],
                     " (",elected_candidates_SR$shortname_de[1],", ",elected_candidates_SR$status_text[1],")")
if (elected_candidates_SR$seats_available_SR[1] != 1) {
content_de <- paste0(content_de,"<br><br>Es ist ein zweiter Wahlgang nötig.")  
}  
} else {
content_de <- paste0(content_de,"Im ersten Wahlgang wurde niemand in den Ständerat gewählt. Es ist ein zweiter Wahlgang nötig.")
}  
} else {
content_de  <- paste0(content_de,"Es sind noch keine Resultate vorhanden.")
}  

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
#status_all <- c("all_counted","NR_counted","SR_counted","no_data")
#status <- status_all[sample(1:4,1)]

new_entry <- data.frame(canton,
                        status,
                        data_canton_all$area_name_de[1],
                        content_de,
                        data_canton_all$area_name_fr[1],
                        content_fr,
                        data_canton_all$area_name_it[1],
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
