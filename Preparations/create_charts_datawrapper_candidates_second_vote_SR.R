###CANDIDATES
#folders_SR <- readRDS("./Preparations/folders_SR.RDS")

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

counted_cantons_all <- election_metadata %>%
  filter(date == "2023-10-22",
         other_election_needed == "yes")

#Merge with area, text and output overview
counted_cantons_all <- counted_cantons_all  %>%
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton")

counted_cantons_SR <- counted_cantons_all %>%
  filter(council == "SR")


for (c in 1:nrow(counted_cantons_SR)) {
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
    arrange(party_id,
            candidate_id
            ) %>%
    mutate(picture = ifelse(is.na(picture) == FALSE,picture,"Replacement.jpg"),
           image_link =paste0("![](https://164.ch/grafiken_wahlen2023/Parlament/",picture,")"),
           status = ifelse(status == 2,"bisher","neu"),
           name_text = paste0(firstname," ",lastname,
                              "<br>(",status,")"),
           elected = ifelse(elected == 1,"&#x2714;&#xFE0F;",""),
           name_text = ifelse(grepl("Vereinzelte",name_text),"Vereinzelte",name_text),
           shortname_de = ifelse(grepl("Vereinzelte",name_text),"-",shortname_de)) %>%
    select(image_link,name_text,shortname_de,votes,elected)

  SR_results$votes <- 0
  SR_results$elected <- ""

  ##Chart Candidates DE
  data_chart <- dw_copy_chart("gmJb8")
  chart_id <- data_chart$id
  
  #Get id 
  # chart_id <- datawrapper_codes %>%
  #   filter(election_ID == counted_cantons_SR$election_ID[c],
  #          chart_type == "majorz_votes",
  #          language == "de") %>%
  #   .[,4]
  

  dw_edit_chart(chart_id ,
                title=paste0("Wahlen 2023: Ergebnis zweiter Wahlgang Ständerat Kanton ",counted_cantons_SR$area_name_de[c]),
                intro = "&nbsp;"
                )
                #folderId = folders_SR[c])
  dw_data_to_chart(SR_results,chart_id)
  dw_publish_chart(chart_id)
  print("Datawrapper-Chart updated")
  
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
new_entry <- data.frame("Ständerat Ergebnis",
                         counted_cantons_SR$area_ID[c],
                         metadata_chart$content$title,
                         metadata_chart$content$language,
                         metadata_chart$id,
                         metadata_chart$content$publicUrl,
                         metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                         metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
 colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
 grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}



saveRDS(grafiken_uebersicht,"grafiken_uebersicht_candidates_SR_second_turn.RDS")
write.xlsx(grafiken_uebersicht,"grafiken_uebersicht_candidates_SR_second_turn.xlsx",row.names = FALSE)
#grafiken_uebersicht_old <- readRDS("grafiken_uebersicht.RDS")
#folders_NR <- readRDS("folders_NR.RDS")


