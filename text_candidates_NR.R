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
people_metadata <- results_NR_cantons_candidates %>%
  .[, c(38, 10, 9, 11)]

people_metadata$source_person_id <-
  as.numeric(people_metadata$source_person_id)
results_candidates$source_person_id <-
  as.numeric(results_candidates$source_person_id)

results_candidates <- results_candidates %>%
  left_join(people_metadata)

#Merge with party data
results_candidates <- results_candidates %>%
  left_join(parties_metadata,
            by = join_by(party_id == id))

#Elected candidates
elected_candidates <- results_candidates %>%
  filter(elected == 1) %>%
  arrange(desc(votes))

elected_candidates$status_text <-
  ifelse(elected_candidates$status == 2, "bisher", "neu")
elected_candidates$status_text_fr <-
  ifelse(elected_candidates$status == 2, "sortant", "nouveau")

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
                              texts_spreadsheet,
                              "de")
texts_candidates_fr <- get_texts(storyboard_candidates,
                                 texts_spreadsheet,
                                 "fr")


#Get voted out candidates
texts_candidates <- get_voted_out_candidates(texts_candidates,
                                             voted_out_candidates,
                                             "de")
texts_candidates_fr <-
  get_voted_out_candidates(texts_candidates_fr,
                           voted_out_candidates,
                           "fr")

#Replace Variables and cleanup
texts_candidates <- replace_variables_cleanup(texts_candidates,
                                              counted_cantons,
                                              "de")
texts_candidates_fr <-
  replace_variables_cleanup(texts_candidates_fr,
                            counted_cantons,
                            "fr")

#Create tables
tabelle <- create_table_NR_candidates(elected_candidates,
                                      "de")
tabelle_fr <- create_table_NR_candidates(elected_candidates,
                                         "fr")

new_entry <- data.frame(
  counted_cantons$area_ID[c],
  toString(storyboard_candidates),
  paste0(
    "<b>",
    texts_candidates[1],
    "</b>\n",
    texts_candidates[2],
    "\n\n",
    texts_candidates[3],
    tabelle,
    "\n\n",
    texts_candidates[4],
    "\n",
    texts_candidates[5],
    "\n",
    texts_candidates[6],
    "\n\n\n"
  ),
  paste0(
    "<b>",
    texts_candidates_fr[1],
    "</b>\n",
    texts_candidates_fr[2],
    "\n\n",
    texts_candidates_fr[3],
    tabelle_fr,
    "\n\n",
    texts_candidates_fr[4],
    "\n",
    texts_candidates_fr[5],
    "\n",
    texts_candidates_fr[6],
    "\n\n\n"
  )
)


print(texts_candidates)
print(tabelle)
print(texts_candidates_fr)
print(tabelle_fr)
