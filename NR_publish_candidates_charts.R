#Get parties results from Canton
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


#Get elected candidates
elected_candidates <- results_candidates %>%
  left_join(people_metadata, join_by(person_id == id)) %>%
  left_join(parties_metadata, join_by (party_id == id)) %>%
  filter(elected == 1) %>%
  mutate(picture = ifelse(is.na(picture) == FALSE,picture,"Replacement.jpg"))

#Frequency of party occurence for sorting 
elected_candidates$frequency_party <- 0
for (e in 1:nrow(elected_candidates)) {
elected_candidates$frequency_party[e] <- sum(grepl(elected_candidates$shortname_de[e],elected_candidates$shortname_de)) 
}  

elected_candidates <- elected_candidates %>%
  arrange(desc(frequency_party),
          lastname)

elected_candidates$status_de <- ifelse(elected_candidates$status == 2,"bisher","neu")
elected_candidates$image_link <- paste0("![](https://164.ch/grafiken_wahlen2023/Nationalrat/",elected_candidates$picture,")")
elected_candidates$text_de <- paste0("<b>",elected_candidates$firstname,"<br>",elected_candidates$lastname,"</b><br>",
                                     elected_candidates$shortname_de,", ",elected_candidates$status_de)

elected_candidates_images <- data.frame("1","2","3","4")
colnames(elected_candidates_images) <- data.frame("col_1","col_2","col_3","col_4")

for (e in seq(1,nrow(elected_candidates),4)) {
  new_entry_pictures <- data.frame(elected_candidates$image_link[e],
                                   elected_candidates$image_link[e+1],
                                   elected_candidates$image_link[e+2],
                                   elected_candidates$image_link[e+3])
  colnames(new_entry_pictures) <- data.frame("col_1","col_2","col_3","col_4")
  elected_candidates_images <- rbind(elected_candidates_images,new_entry_pictures)
  
  
  new_entry_text <- data.frame(elected_candidates$text_de[e],
                               elected_candidates$text_de[e+1],
                               elected_candidates$text_de[e+2],
                               elected_candidates$text_de[e+3])
  colnames(new_entry_text) <- data.frame("col_1","col_2","col_3","col_4")
  elected_candidates_images <- rbind(elected_candidates_images,new_entry_text)
}  
elected_candidates_images <- elected_candidates_images[-1,]
elected_candidates_images[is.na(elected_candidates_images)] <- "&nbsp;"


##Chart Candidates DE
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_elected",
         language == "de") %>%
  .[,4]
dw_data_to_chart(elected_candidates_images,chart_id)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")


