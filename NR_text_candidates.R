#Get candidates results from Canton
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM candidates_results WHERE election_id = '",
      counted_cantons$election_ID[c],
      "'"
    )
  )
results_candidates <- fetch(rs, n = -1)
dbDisconnectAll()


#Merge with candidates metadata (to adapt!)
#people_metadata <- results_NR_cantons_candidates %>%
#  .[, c(38, 10, 9, 11)]

#people_metadata$source_person_id <-
#  as.numeric(people_metadata$source_person_id)
#results_candidates$source_person_id <-
#  as.numeric(results_candidates$source_person_id)

#Merge with people data
results_candidates <- results_candidates %>%
  left_join(people_metadata,
            by = join_by(person_id==id))

#Merge with party data
results_candidates <- results_candidates %>%
  left_join(parties_metadata,
            by = join_by(party_id == id))

#Elected candidates
elected_candidates <- results_candidates %>%
  filter(elected == 1) %>%
  group_by(party_id) %>%
  mutate(amount_elected = n()) %>%
  ungroup() %>%
  arrange(desc(amount_elected),
          shortname_de,
    desc(votes))

elected_candidates$status_text <-
  ifelse(elected_candidates$status == 2, "bisher", "neu")
elected_candidates$status_text_fr = ifelse(elected_candidates$status == 2,
                ifelse(elected_candidates$gender == "m","sortant","sortante"),
                ifelse(elected_candidates$gender == "m","nouveau","nouvelle"))
elected_candidates$status_text_it <-
  ifelse(elected_candidates$status == 2, "uscente", "nuovo")

#Voted out candidates
voted_out_candidates <- results_candidates %>%
  filter(elected == 0,
         status == 2) %>%
  arrange(desc(votes))

###Get Storyboard
storyboard_candidates <-
  get_story_NR_candidates(
    counted_cantons$seats_available_NR[c],
    elected_candidates,
    voted_out_candidates
  )

###Get Story pieces
texts_candidates <- get_texts(storyboard_candidates,
                              texts_spreadsheet_NR_candidates,
                              "de")
texts_candidates_fr <- get_texts(storyboard_candidates,
                                 texts_spreadsheet_NR_candidates,
                                 "fr")
texts_candidates_it <- get_texts(storyboard_candidates,
                                 texts_spreadsheet_NR_candidates,
                                 "it")

#Get voted out candidates
texts_candidates <- get_voted_out_candidates(texts_candidates,
                                             voted_out_candidates,
                                             "de")
texts_candidates_fr <-
  get_voted_out_candidates(texts_candidates_fr,
                           voted_out_candidates,
                           "fr")
texts_candidates_it <-
  get_voted_out_candidates(texts_candidates_it,
                           voted_out_candidates,
                           "it")

#Replace Variables and cleanup
texts_candidates <- replace_variables_cleanup(texts_candidates,
                                              counted_cantons,
                                              "de")
texts_candidates_fr <-
  replace_variables_cleanup(texts_candidates_fr,
                            counted_cantons,
                            "fr")
texts_candidates_it <-
  replace_variables_cleanup(texts_candidates_it,
                            counted_cantons,
                            "it")

#Create tables
tabelle <- create_table_NR_candidates(elected_candidates,
                                      "de")
tabelle_fr <- create_table_NR_candidates(elected_candidates,
                                         "fr")
tabelle_it <- create_table_NR_candidates(elected_candidates,
                                         "it")

print(texts_candidates)
print(tabelle)
print(texts_candidates_fr)
print(tabelle_fr)
print(texts_candidates_it)
print(tabelle_it)

