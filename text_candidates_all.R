source("get_testdata_2023.R")

library(stringi)
library(stringr)
texts_spreadsheet <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Gewaehlte")

setwd("./Functions")
source("functions_storyfinder.R")
source("functions_storybuilder.R")
source("functions_replace_variables_cleanup.R")
source("functions_voted_out_candidates.R")
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

  #Get candidates results from Canton
  mydb <- connectDB(db_name="sda_elections")
  rs <- dbSendQuery(mydb, paste0("SELECT * FROM candidates_results WHERE election_id = '",elections_metadata_selection$election_ID[c],"'"))
  results_candidates <- fetch(rs,n=-1)
  dbDisconnectAll()


  #Merge with candidates metadata (to adapt!)
  people_metadata <- results_NR_cantons_candidates %>%
    .[,c(38,10,9,11)]

  people_metadata$source_person_id <- as.numeric(people_metadata$source_person_id)
  results_candidates$source_person_id <- as.numeric(results_candidates$source_person_id)
  
  results_candidates <- results_candidates %>%
    left_join(people_metadata) %>%
    rename(firstname = vorname,
           lastname = name,
           gender = geschlecht)

  #Merge with party data
  results_candidates <- results_candidates %>%
    left_join(parties_metadata,
              by = join_by(party_id == id))

  #Elected candidates
  elected_candidates <- results_candidates %>%
    filter(elected == 1) %>%
    arrange(desc(votes))
  
  elected_candidates$status_text <- ifelse(elected_candidates$status == 2,"bisher","neu")
  elected_candidates$status_text_fr <- ifelse(elected_candidates$status == 2,"sortant","nouveau")
  
  #Voted out candidates
  voted_out_candidates <- results_candidates %>%
    filter(elected == 0,
           status == 2) %>%
    arrange(desc(votes))

  ###Get Storyboard
  storyboard_candidates <- get_story_NR_candidates(elections_metadata_selection$seats_available_NR[c],
                                                   elected_candidates,
                                                   voted_out_candidates)

  ###Get Story pieces
  texts_candidates <- get_texts(storyboard_candidates,
                             texts_spreadsheet,
                             "de")
  texts_candidates_fr <- get_texts(storyboard_candidates,
                                texts_spreadsheet,
                                "fr")
  texts_candidates_it <- get_texts(storyboard_candidates,
                                   texts_spreadsheet,
                                   "it")
  

  
  #Get voted out candidates
  texts_candidates <- get_voted_out_candidates(texts_candidates,
                                         voted_out_candidates,
                                         "de")
  texts_candidates_fr <- get_voted_out_candidates(texts_candidates_fr,
                                                  voted_out_candidates,
                                                   "fr")
  texts_candidates_it <- get_voted_out_candidates(texts_candidates_it,
                                                  voted_out_candidates,
                                                  "it")
  
  
  #Replace Variables and cleanup
  texts_candidates <- replace_variables_cleanup(texts_candidates,
                                     elections_metadata_selection,
                                     "de")
  texts_candidates_fr <- replace_variables_cleanup(texts_candidates_fr,
                                        elections_metadata_selection,
                                        "fr")
  texts_candidates_it <- replace_variables_cleanup(texts_candidates_it,
                                                   elections_metadata_selection,
                                                   "it")

#Create tables
tabelle <- create_table_NR_candidates(elected_candidates,
                                     "de")
tabelle_fr <- create_table_NR_candidates(elected_candidates,
                                        "fr")
tabelle_it <- create_table_NR_candidates(elected_candidates,
                                         "it")

new_entry <- data.frame(elections_metadata_selection$area_ID[c],
                        toString(storyboard_candidates),
                        paste0("<b>",texts_candidates[1],"</b>\n",texts_candidates[2],"\n\n",
                               texts_candidates[3],
                               tabelle,"\n\n",
                               texts_candidates[4],"\n",
                               texts_candidates[5],"\n",
                               texts_candidates[6],"\n\n\n"
                        ),
                        paste0("<b>",texts_candidates_fr[1],"</b>\n",texts_candidates_fr[2],"\n\n",
                               texts_candidates_fr[3],
                               tabelle_fr,"\n\n",
                               texts_candidates_fr[4],"\n",
                               texts_candidates_fr[5],"\n",
                               texts_candidates_fr[6],"\n\n\n"
                        ),
                        paste0("<b>",texts_candidates_it[1],"</b>\n",texts_candidates_it[2],"\n\n",
                               texts_candidates_it[3],
                               tabelle_it,"\n\n",
                               texts_candidates_it[4],"\n",
                               texts_candidates_it[5],"\n",
                               texts_candidates_it[6],"\n\n\n"
                        )                
                        )


colnames(new_entry) <- c("Kanton","Storyboard","Text","Text_fr","Text_it")
overview_texts <- rbind(overview_texts,new_entry)

cat(overview_texts$Text_it[c+1])

#source("create_mars_meldung_candidates_NR_FR.R",encoding = "UTF-8")
}

overview_texts <- overview_texts[-1,]
#write.xlsx(overview_texts,"texte_candidates_NR.xlsx",row.names = FALSE)

#HTML Output
html_output <- gsub("\n","<br>",overview_texts$Text_it)
cat(html_output)