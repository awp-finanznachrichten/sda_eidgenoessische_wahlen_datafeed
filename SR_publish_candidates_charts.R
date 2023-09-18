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

#Get SR candidates
SR_results <- results_candidates %>%
  filter(is.na(source_update)) %>% #REMOVE!
  left_join(people_metadata, join_by(person_id == id)) %>%
  left_join(parties_metadata, join_by (party_id == id)) %>%
  filter(is.na(picture) == FALSE) %>% #REMOVE!
  .[1:10,] %>%  #REMOVE!
  mutate(votes = sample(1:10000,10)) #REMOVE!

SR_results <- SR_results %>%
  arrange(desc(votes),
          lastname)

SR_results$elected[1:2] <- 1 #REMOVE!

SR_results <- SR_results %>%
  mutate(image_link = paste0("![](https://164.ch/grafiken_wahlen2023/Nationalrat/",picture,")"),
         status = ifelse(status == 2,"bisher","neu"),
         name_text = paste0(firstname," ",lastname,
                            "<br>(",status,")"),
         elected = ifelse(elected == 1,"&#x2714;&#xFE0F;","")) %>%
  select(image_link,name_text,shortname_de,votes,elected)

###Publish Chart DE###
intro_text <- paste0("Das absolute Mehr liegt bei <b>",format(counted_cantons$absolute_majority[c],big.mark ="'")," Stimmen</b>. ")

if (counted_cantons$other_election_needed[c] == "no") {
  intro_text <- paste0(intro_text,"Es ist kein zweiter Wahlgang nötig.")  
} else {
  intro_text <- paste0(intro_text,"Es ist ein zweiter Wahlgang nötig.")  
}  

chart_ID <- "gmJb8"
dw_data_to_chart(SR_results,chart_ID)

chart_metadata <- dw_retrieve_chart_metadata(chart_ID)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

#Make rows Green and Bold of elected candidates
for (i in 1:nrow(SR_results)) {
 if (SR_results$elected[i] == "&#x2714;&#xFE0F;") {
  adapted_list$rows[[i]]$style$bold <- TRUE
  adapted_list$rows[[i]]$style$background <- "#d1eec9"
 }
}

dw_edit_chart(chart_ID,
              intro = intro_text,
              annotate = paste0("Endresultat vom ",format(Sys.Date(),"%d.%m.%Y")," ",format(Sys.time(),"%H:%M")," Uhr"),
              visualize = adapted_list)
dw_publish_chart(chart_ID)
print("Datawrapper-Chart updated")


