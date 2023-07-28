source("get_data_2023.R")

library(stringi)
all_texts <- ""
texts_spreadsheet <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Sitzverteilung")
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

overview_texts <- data.frame("Kanton","Storyboard","Text")
colnames(overview_texts) <- c("Kanton","Storyboard","Text")

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
###Get Story pieces¨
texts_parties <- get_texts(storyboard_parties,
                           texts_spreadsheet,
                           "de")

#Replace Variables
texts_parties <- replace_variables(texts_parties,
                                   elections_metadata_selection,
                                   results_parties)

##Create Table
tabelle <- paste0("<table><tbody><tr>",
"<td></td>",
"<td>Sitze</td>",
"<td>Veränderung zu 2019</td>",
"<td>Wähleranteil</td>",
"<td>Veränderung zu 2019</td>",
"</tr>")

for (p in 1:nrow(results_parties)) {
tabelle <- paste0(tabelle,
                  "<tr>",
                  "<td>",results_parties$shortname_de[p],"</td>",
                  "<td>",results_parties$seats[p],"</td>",
                  "<td>+",results_parties$seats_change[p],"</td>",
                  "<td>",gsub("[.]",",",format(round2(results_parties$voter_share[p],1),nsmall=1)),"%</td>",
                  "<td>+",gsub("[.]",",",format(round2(results_parties$voter_share_change[p],1),nsmall=1)),"</td>", #%P
                  "</tr>")
         
}                    
tabelle <- paste0(tabelle,"</tbody></table>")
tabelle <- gsub("[+]-","-",tabelle)
tabelle <- gsub("[+]0[<]","-<",tabelle)
tabelle <- gsub("[+]0,0","-",tabelle) #[+]0.0[%]P

print(texts_parties)
print(tabelle)

new_entry <- data.frame(elections_metadata_selection$area_ID[c],
                        toString(storyboard_parties),
                        paste0("<b>",texts_parties[1],"</b>\n",texts_parties[2],"\n\n",
                               texts_parties[3]," ",texts_parties[4],"\n\n",
                               texts_parties[5],
                               tabelle,"\n\n",
                               texts_parties[6]," ",texts_parties[7],"\n\n",
                               texts_parties[8],"\n\n",
                               texts_parties[9],"\n\n\n"
                               ))
colnames(new_entry) <- c("Kanton","Storyboard","Text")
overview_texts <- rbind(overview_texts,new_entry)
print(storyboard_parties)

#source("create_mars_meldung_results_NR.R",encoding = "UTF-8")

}

overview_texts <- overview_texts[-1,]
write.xlsx(overview_texts,"texte_results_NR.xlsx",row.names = FALSE)

#HTML Output
html_output <- gsub("\n","<br>",overview_texts$Text)
cat(html_output)
