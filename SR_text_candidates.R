#Get candidates results from Canton
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM candidates_results WHERE election_id = '",
      counted_cantons_SR$election_ID[c], 
      "'"
    )
  )
results_candidates <- fetch(rs, n = -1)
dbDisconnectAll()

results_candidates <- results_candidates %>%
  left_join(people_metadata, join_by(person_id == id)) %>%
  left_join(parties_metadata, join_by (party_id == id)) %>%
  arrange(desc(votes),
          lastname) %>%
  mutate(status_text = ifelse(status == 2,"bisher","neu"),
         status_text_fr = ifelse(status == 2,"sortant", "nouveau"),
         status_text_it = ifelse(status == 2,"uscente", "nuovo"),
         lastname = ifelse(grepl("Vereinzelte",lastname),"Vereinzelte",lastname),
         shortname_de = ifelse(grepl("Vereinzelte",lastname),"-",shortname_de),
         shortname_fr = ifelse(grepl("Vereinzelte",lastname),"-",shortname_fr),
         shortname_it = ifelse(grepl("Vereinzelte",lastname),"-",shortname_it),
         status_text = ifelse(grepl("Vereinzelte",lastname),"-",status_text),
         status_text_fr = ifelse(grepl("Vereinzelte",lastname),"-",status_text_fr),
         status_text_it = ifelse(grepl("Vereinzelte",lastname),"-",status_text_it)
         )

#Elected candidates
elected_candidates <- results_candidates %>%
  filter(elected == 1)

#Voted out candidates
voted_out_candidates <- results_candidates %>%
  filter(elected == 0,
         status == 2) %>%
  arrange(desc(votes))

###Get Storyboard
storyboard_candidates <-
  get_story_SR_candidates(
    counted_cantons_SR$seats_available_SR[c],
    elected_candidates,
    voted_out_candidates
  )


###Get Story pieces
texts_candidates <- get_texts(storyboard_candidates,
                              texts_spreadsheet_SR_candidates,
                              "de")
texts_candidates_fr <- get_texts(storyboard_candidates,
                                 texts_spreadsheet_SR_candidates,
                                 "fr")
texts_candidates_it <- get_texts(storyboard_candidates,
                                 texts_spreadsheet_SR_candidates,
                                 "it")

#Replace Variables and cleanup
texts_candidates <- replace_variables_cleanup_SR(texts_candidates,
                                              counted_cantons_SR,
                                              elected_candidates,
                                              "de")
texts_candidates_fr <-
  replace_variables_cleanup_SR(texts_candidates_fr,
                            counted_cantons_SR,
                            elected_candidates,
                            "fr")
texts_candidates_it <-
  replace_variables_cleanup_SR(texts_candidates_it,
                            counted_cantons_SR,
                            elected_candidates,
                            "it")

#Create tables
tabelle <- create_table_SR_candidates(results_candidates,
                                      "de")
tabelle_fr <- create_table_SR_candidates(results_candidates,
                                         "fr")
tabelle_it <- create_table_SR_candidates(results_candidates,
                                         "it")

print(texts_candidates)
print(tabelle)
print(texts_candidates_fr)
print(tabelle_fr)
print(texts_candidates_it)
print(tabelle_it)

