source("get_data_2023.R")

library(stringi)
library(stringr)
texts_spreadsheet <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Gewaehlte")

source("functions_storyfinder.R")
source("functions_storybuilder.R")
source("functions_replace_variables.R")

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

overview_texts <- data.frame("Kanton","Storyboard","Text","Tabelle")
colnames(overview_texts) <- c("Kanton","Storyboard","Text","Tabelle")

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
    left_join(people_metadata)

  #Merge with party data
  results_candidates <- results_candidates %>%
    left_join(parties_metadata,
              by = join_by(party_id == id))
  
  #Elected candidates
  elected_candidates <- results_candidates %>%
    filter(elected == 1) %>%
    arrange(desc(votes))
  
  elected_candidates$status_text <- ifelse(elected_candidates$status == 2,"bisher","neu")
  
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

  #Replace Variables
  texts_candidates <- replace_variables(texts_candidates,
                                     elections_metadata_selection,
                                     voted_out_candidates)

#Table elected candidates

tabelle <- paste0("<table><tbody><tr><td></td>",
"<td></td>",
"<td></td>",
"<td>Anzahl Stimmen</td></tr>")

for (e in 1:nrow(elected_candidates)) {
tabelle <- paste0(tabelle,
                  "<tr>",
                  "<td>",elected_candidates$vorname[e]," ",elected_candidates$name[e],"</td>",
                  "<td>",elected_candidates$shortname_de[e],"</td>",
                  "<td>",elected_candidates$status_text[e],"</td>",
                  "<td>",format(elected_candidates$votes[e],big.mark = "'"),"</td>",
                  "</tr>")
         
}                    
tabelle <- paste0(tabelle,"</tbody></table>")

new_entry <- data.frame(elections_metadata_selection$area_ID[c],
                        toString(storyboard_candidates),
                        paste0(texts_candidates[1],"\n",texts_candidates[2],"\n\n",
                               texts_candidates[3],"\n((Tabelle))\n",
                               texts_candidates[4],"\n",
                               texts_candidates[5],"\n",
                               texts_candidates[6]
                        ),
                        tabelle)


colnames(new_entry) <- c("Kanton","Storyboard","Text","Tabelle")
overview_texts <- rbind(overview_texts,new_entry)

print(texts_candidates)
cat(tabelle)

#source("create_mars_meldung_candidates_NR.R",encoding = "UTF-8")
}

overview_texts <- overview_texts[-1,]
#write.xlsx(overview_texts,"texte_candidates_NR.xlsx",row.names = FALSE)
