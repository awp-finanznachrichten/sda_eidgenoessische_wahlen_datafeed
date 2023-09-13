source("get_testdata_2023.R")

library(stringi)
library(stringr)
all_texts <- ""
texts_spreadsheet <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Sitzverteilung")

setwd("./Functions")
source("functions_storyfinder.R")
source("functions_storybuilder.R")
source("functions_replace_variables_cleanup.R")
source("functions_winners_losers.R")
source("functions_create_tables_NR.R")
setwd("..")

#Metadata NR
elections_metadata_selection <- election_metadata %>%
  filter(council == "NR",
         date == "2023-10-22")
#status != "finished")

#Merge with area data
elections_metadata_selection <- elections_metadata_selection  %>%
  left_join(areas_metadata) %>%
  filter(area_type == "canton")

#Merge with status data
elections_metadata_selection <- elections_metadata_selection %>% 
  left_join(stand_cantons, join_by(bfs_ID==kanton_nummer))

overview_texts <- data.frame("Kanton","Storyboard","Text","Text_fr","Text_it")
colnames(overview_texts) <- c("Kanton","Storyboard","Text","Text_fr","Text_it")

for (c in 1:nrow(elections_metadata_selection)) {

#Get parties results from Canton
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, paste0("SELECT * FROM parties_results WHERE election_id = '",elections_metadata_selection$election_ID[c],"'"))
results_parties <- fetch(rs,n=-1)
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
                                           elections_metadata_selection$seats_available_NR[c],
                                           elections_metadata_selection$area_ID[c])

###Get Story pieces
texts_parties <- get_texts(storyboard_parties,
                           texts_spreadsheet,
                           "de")

texts_parties_fr <- get_texts(storyboard_parties,
                           texts_spreadsheet,
                           "fr")

texts_parties_it <- get_texts(storyboard_parties,
                              texts_spreadsheet,
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
                                   elections_metadata_selection,
                                   "de")
texts_parties_fr <- replace_variables_cleanup(texts_parties_fr,
                                   elections_metadata_selection,
                                   "fr")
texts_parties_it <- replace_variables_cleanup(texts_parties_it,
                                              elections_metadata_selection,
                                              "it")


#Create tables
tabelle <- create_table_NR_results(results_parties,
                                   "de")
tabelle_fr <- create_table_NR_results(results_parties,
                                   "fr")
tabelle_it <- create_table_NR_results(results_parties,
                                      "it")

new_entry <- data.frame(elections_metadata_selection$area_ID[c],
                        toString(storyboard_parties),
                        paste0("<b>",texts_parties[1],"</b>\n",texts_parties[2],"\n\n",
                               texts_parties[3]," ",texts_parties[4],"\n\n",
                               texts_parties[5],
                               tabelle,"\n\n",
                               texts_parties[6]," ",texts_parties[7],"\n\n",
                               texts_parties[8],"\n\n",
                               texts_parties[9],"\n\n\n"
                               ),
                        paste0("<b>",texts_parties_fr[1],"</b>\n",texts_parties_fr[2],"\n\n",
                               texts_parties_fr[3]," ",texts_parties_fr[4],"\n\n",
                               texts_parties_fr[5],
                               tabelle_fr,"\n\n",
                               texts_parties_fr[6]," ",texts_parties_fr[7],"\n\n",
                               texts_parties_fr[8],"\n\n",
                               texts_parties_fr[9],"\n\n\n"
                        ),
                        paste0("<b>",texts_parties_it[1],"</b>\n",texts_parties_it[2],"\n\n",
                               texts_parties_it[3]," ",texts_parties_it[4],"\n\n",
                               texts_parties_it[5],
                               tabelle_it,"\n\n",
                               texts_parties_it[6]," ",texts_parties_it[7],"\n\n",
                               texts_parties_it[8],"\n\n",
                               texts_parties_it[9],"\n\n\n"
                        )
                        )
colnames(new_entry) <- c("Kanton","Storyboard","Text","Text_fr","Text_it")
overview_texts <- rbind(overview_texts,new_entry)

cat(overview_texts$Text[c+1])
#source("mars_meldung_results_NR_IT.R",encoding = "UTF-8")

}

overview_texts <- overview_texts[-1,]
#write.xlsx(overview_texts,"texte_results_NR.xlsx",row.names = FALSE)

#HTML Output
html_output <- gsub("\n","<br>",overview_texts$Text)
cat(html_output)
