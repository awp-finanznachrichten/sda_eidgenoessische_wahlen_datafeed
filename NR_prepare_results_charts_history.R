#Get parties results from Canton
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM parties_results WHERE election_id = '",
      counted_cantons$election_ID[c],
      "'"
    )
  )
results_parties <- fetch(rs, n = -1)
dbDisconnectAll()

texts_chart <- get_text_charts(language="de",
                               elections_metadata = counted_cantons[c,])

#Merge with party_metadata
results_parties <- results_parties %>%
  left_join(parties_metadata,
            by = join_by(party_ID == id))

results_parties <- results_parties %>%
  filter(voter_share > 0,
         seats != 0 |
           seats_change != 0 |
           voter_share >= 3) %>%
  arrange(desc(seats),
          desc(voter_share)) %>%
  select(shortname_de,
         shortname_fr,
         shortname_it,
         voter_share,
         party_color)

###GET HISTORY DATA###
response <-
  GET(BFS_API_URL)
content <- content(response)$result$resources

url_NR_history <-
  as.data.frame(do.call(rbind, content))$download_url[[1]]

#Download data
setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_daten")
download.file(url_NR_history ,
              destfile = "data_NR_history.json",
              method = "curl")
results_parties_history <-
  fromJSON("data_NR_history.json", flatten = TRUE)
setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")
  
results_parties_history_cantons <- results_parties_history$level_kantone

#Adapt CVP/Mitte
results_parties_history_cantons$partei_id <- as.numeric(ifelse(results_parties_history_cantons$partei_id == 2,34,results_parties_history_cantons$partei_id))
  
#Merge with parties metadata
results_parties_history_cantons <- results_parties_history_cantons %>%
  left_join(parties_metadata,
            by = join_by(partei_id == bfs_id))


#Merge with history data
parties_history <- results_parties_history_cantons %>%
  filter(kanton_nummer == counted_cantons$bfs_ID[c],
         wahl_jahr != 2023)

years <- c(seq(1991,2023,4))
results_history <- data.frame(years)

for (p in 1:nrow(results_parties)) {

party_values <- parties_history %>%
  filter(shortname_de == results_parties$shortname_de[p]) %>%
  select(wahl_jahr,partei_staerke) %>%
  add_row(wahl_jahr = 2023,partei_staerke = results_parties$voter_share[p])

colnames(party_values) <- c("years",results_parties$shortname_de[p])

results_history <- results_history %>%
  left_join(party_values)
} 
