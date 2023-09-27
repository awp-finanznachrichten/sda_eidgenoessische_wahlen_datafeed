#Get parties results from Canton
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM candidates_results WHERE election_id = '",
      counted_cantons_SR$election_ID[c],
      "'"
    )
  )
results_candidates <- fetch(rs, n = -1)
dbDisconnectAll()

#Get SR candidates
SR_results <- results_candidates %>%
  left_join(people_metadata, join_by(person_id == id)) %>%
  left_join(parties_metadata, join_by (party_id == id)) %>%
  arrange(desc(votes),
          lastname) %>%
  mutate(picture = ifelse(is.na(picture) == FALSE,picture,"Replacement.jpg"),
         image_link = ifelse(grepl("NR",picture),
                             paste0("![](https://164.ch/grafiken_wahlen2023/Nationalrat/",picture,")"),
                             paste0("![](https://164.ch/grafiken_wahlen2023/Staenderat/",picture,")")),
         status = ifelse(status == 2,"bisher","neu"),
         name_text = paste0(firstname," ",lastname,
                            "<br>(",status,")"),
         elected = ifelse(elected == 1,"&#x2714;&#xFE0F;",""),
         name_text = ifelse(grepl("Vereinzelte",name_text),"Vereinzelte",name_text),
         shortname_de = ifelse(grepl("Vereinzelte",name_text),"-",shortname_de)) %>%
  select(image_link,name_text,shortname_de,votes,elected)


###Publish Chart DE###
intro_text <- ""

if (is.na(counted_cantons_SR$absolute_majority[c]) == FALSE) {
intro_text <- paste0("Das absolute Mehr liegt bei <b>",format(counted_cantons_SR$absolute_majority[c],big.mark ="'")," Stimmen</b>. ")
}

if (is.na(counted_cantons_SR$other_election_needed[c]) == FALSE) {
if (counted_cantons_SR$other_election_needed[c] == "no") {
  intro_text <- paste0(intro_text,"Es ist kein zweiter Wahlgang nötig.")  
} else {
  intro_text <- paste0(intro_text,"Es ist ein zweiter Wahlgang nötig.")  
}  
}
  
chart_ID <- datawrapper_codes %>%
  filter(election_ID == counted_cantons_SR$election_ID[c],
         chart_type == "majorz_votes",
         language == "de") %>%
  .[,4]
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


