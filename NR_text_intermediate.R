###Get Storyboard
storyboard_intermediate <- get_story_NR_intermediate(overview_ch)


###Get Story pieces
texts_intermediate <- get_texts(storyboard_intermediate,
                           texts_spreadsheet_NR_intermediate,
                           "de")

texts_intermediate_fr <- get_texts(storyboard_intermediate,
                              texts_spreadsheet_NR_intermediate,
                              "fr")

texts_intermediate_it <- get_texts(storyboard_intermediate,
                              texts_spreadsheet_NR_intermediate,
                              "it")


#Get winners and losers
texts_intermediate <- get_winners_losers(texts_intermediate,
                                    overview_ch,
                                    "de")
texts_intermediate_fr <- get_winners_losers(texts_intermediate_fr,
                                       overview_ch,
                                       "fr")
texts_intermediate_it <- get_winners_losers(texts_intermediate_it,
                                       overview_ch,
                                       "it")

#Replace Variables and cleanup
texts_intermediate <- replace_variables_cleanup(texts_intermediate,
                                           counted_cantons,
                                           "de",
                                           type = "overview")
texts_intermediate_fr <- replace_variables_cleanup(texts_intermediate_fr,
                                              counted_cantons,
                                              "fr",
                                              type = "overview")
texts_intermediate_it <- replace_variables_cleanup(texts_intermediate_it,
                                              counted_cantons,
                                              "it",
                                              type = "overview")

#Create tables
tabelle <- create_table_NR_overview(overview_ch,
                                   "de")
tabelle_fr <- create_table_NR_overview(overview_ch,
                                      "fr")
tabelle_it <- create_table_NR_overview(overview_ch,
                                      "it")

print(texts_intermediate)
print(tabelle)
print(texts_intermediate_fr)
print(tabelle_fr)
print(texts_intermediate_it)
print(tabelle_it)
