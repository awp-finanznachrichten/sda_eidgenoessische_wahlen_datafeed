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


elected_candidates <- elected_candidates %>%
  mutate(image_link = paste0("![](https://164.ch/grafiken_wahlen2023/Parlament/",picture,")"),
         status_de = ifelse(status == 2,"bisher","neu"),
         text_de = paste0("<b>",firstname,"<br>",lastname,"</b><br>",
                           shortname_de,", ",status_de),
         status_fr = ifelse(status == 2,
                         ifelse(gender == "m","sortant","sortante"),
                         ifelse(gender == "m","nouveau","nouvelle")),
         text_fr = paste0("<b>",firstname,"<br>",lastname,"</b><br>",
                          shortname_fr,", ",status_fr),
         status_it = ifelse(status == 2,"uscenti","nuovi"),
         text_it = paste0("<b>",firstname,"<br>",lastname,"</b><br>",
                          shortname_it,", ",status_it)
         
  )

###DE###
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


###FR###
elected_candidates_images_fr <- data.frame("1","2","3","4")
colnames(elected_candidates_images_fr) <- data.frame("col_1","col_2","col_3","col_4")

for (e in seq(1,nrow(elected_candidates),4)) {
  new_entry_pictures <- data.frame(elected_candidates$image_link[e],
                                   elected_candidates$image_link[e+1],
                                   elected_candidates$image_link[e+2],
                                   elected_candidates$image_link[e+3])
  colnames(new_entry_pictures) <- data.frame("col_1","col_2","col_3","col_4")
  elected_candidates_images_fr <- rbind(elected_candidates_images_fr,new_entry_pictures)
  
  
  new_entry_text <- data.frame(elected_candidates$text_fr[e],
                               elected_candidates$text_fr[e+1],
                               elected_candidates$text_fr[e+2],
                               elected_candidates$text_fr[e+3])
  colnames(new_entry_text) <- data.frame("col_1","col_2","col_3","col_4")
  elected_candidates_images_fr <- rbind(elected_candidates_images_fr,new_entry_text)
}  
elected_candidates_images_fr <- elected_candidates_images_fr[-1,]
elected_candidates_images_fr[is.na(elected_candidates_images_fr)] <- "&nbsp;"

###IT###
elected_candidates_images_it <- data.frame("1","2","3","4")
colnames(elected_candidates_images_it) <- data.frame("col_1","col_2","col_3","col_4")

for (e in seq(1,nrow(elected_candidates),4)) {
  new_entry_pictures <- data.frame(elected_candidates$image_link[e],
                                   elected_candidates$image_link[e+1],
                                   elected_candidates$image_link[e+2],
                                   elected_candidates$image_link[e+3])
  colnames(new_entry_pictures) <- data.frame("col_1","col_2","col_3","col_4")
  elected_candidates_images_it <- rbind(elected_candidates_images_it,new_entry_pictures)
  
  
  new_entry_text <- data.frame(elected_candidates$text_it[e],
                               elected_candidates$text_it[e+1],
                               elected_candidates$text_it[e+2],
                               elected_candidates$text_it[e+3])
  colnames(new_entry_text) <- data.frame("col_1","col_2","col_3","col_4")
  elected_candidates_images_it <- rbind(elected_candidates_images_it,new_entry_text)
}  
elected_candidates_images_it <- elected_candidates_images_it[-1,]
elected_candidates_images_it[is.na(elected_candidates_images_it)] <- "&nbsp;"


texts_chart <- get_text_charts(language="de",
                               elections_metadata = counted_cantons[c,])
texts_chart_fr <- get_text_charts(language="fr",
                                  elections_metadata = counted_cantons[c,])
texts_chart_it <- get_text_charts(language="it",
                                  elections_metadata = counted_cantons[c,])

##Chart Candidates DE
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_elected",
         language == "de") %>%
  .[,4]
dw_data_to_chart(elected_candidates_images,chart_id)
dw_edit_chart(chart_id,
              intro = texts_chart[1])
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

##Chart Candidates FR
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_elected",
         language == "fr") %>%
  .[,4]
dw_data_to_chart(elected_candidates_images_fr,chart_id)
dw_edit_chart(chart_id,
              intro = texts_chart_fr[1])
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

##Chart Candidates IT
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_elected",
         language == "it") %>%
  .[,4]
dw_data_to_chart(elected_candidates_images_it,chart_id)
dw_edit_chart(chart_id,
              intro = texts_chart_it[1])
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

