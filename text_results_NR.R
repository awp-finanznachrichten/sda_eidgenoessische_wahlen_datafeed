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
          desc(voter_share))

###Get Storyboard
storyboard_parties <- get_story_NR_parties(results_parties,
                                           counted_cantons$seats_available_NR[c],
                                           counted_cantons$area_ID[c])

###Get Story pieces
texts_parties <- get_texts(storyboard_parties,
                           texts_spreadsheet_NR_results,
                           "de")

texts_parties_fr <- get_texts(storyboard_parties,
                              texts_spreadsheet_NR_results,
                              "fr")

texts_parties_it <- get_texts(storyboard_parties,
                              texts_spreadsheet_NR_results,
                              "it")

#Get winners and losers
texts_parties <- get_winners_losers(texts_parties,
                                    results_parties,
                                    "de")
texts_parties_fr <- get_winners_losers(texts_parties_fr,
                                       results_parties,
                                       "fr")
texts_parties_it <- get_winners_losers(texts_parties_it,
                                       results_parties,
                                       "it")

#Replace Variables and cleanup
texts_parties <- replace_variables_cleanup(texts_parties,
                                           counted_cantons,
                                           "de")
texts_parties_fr <- replace_variables_cleanup(texts_parties_fr,
                                              counted_cantons,
                                              "fr")
texts_parties_it <- replace_variables_cleanup(texts_parties_it,
                                              counted_cantons,
                                              "it")

#Create tables
tabelle <- create_table_NR_results(results_parties,
                                   "de")
tabelle_fr <- create_table_NR_results(results_parties,
                                      "fr")
tabelle_it <- create_table_NR_results(results_parties,
                                      "it")

print(texts_parties)
print(tabelle)
print(texts_parties_fr)
print(tabelle_fr)
print(texts_parties_it)
print(tabelle_it)
