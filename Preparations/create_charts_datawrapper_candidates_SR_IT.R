###CANDIDATES
folders_SR <- readRDS("./Preparations/folders_SR_IT.RDS")


###Grafiken erstellen und Daten speichern
grafiken_uebersicht_it <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht_it) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

#counted_cantons_SR <- counted_cantons_SR %>%
#  filter(area_ID != "AI",
#         area_ID != "OW")

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
    arrange(desc(votes),
            lastname) %>%
    mutate(picture = ifelse(is.na(picture) == FALSE,picture,"Replacement.jpg"),
           image_link = ifelse(grepl("NR",picture),
                               paste0("![](https://164.ch/grafiken_wahlen2023/Nationalrat/",picture,")"),
                                      paste0("![](https://164.ch/grafiken_wahlen2023/Staenderat/",picture,")")),
           status = ifelse(status == 2,"uscenti","nuovi"),
           name_text = paste0(firstname," ",lastname,
                              "<br>(",status,")"),
           elected = ifelse(elected == 1,"&#x2714;&#xFE0F;",""),
           name_text = ifelse(grepl("Altri",name_text),"Altri",name_text),
           shortname_it = ifelse(grepl("Altri",name_text),"-",shortname_it)) %>%
    select(image_link,name_text,shortname_it,votes,elected)

  SR_results$votes <-0
  SR_results$elected <- ""

  ##Chart Candidates FR
  data_chart <- dw_copy_chart("0JfDv")
  chart_id <- data_chart$id
  
  #Get id 
  #chart_id <- datawrapper_codes %>%
  #  filter(election_ID == counted_cantons_SR$election_ID[c],
  #         chart_type == "majorz_votes",
  #         language == "de") %>%
  #  .[,4]
  
  headline <- paste0("Federali 2023: risultati dell'elezione per il Consiglio degli Stati nel canton ",counted_cantons_SR$area_name_it[c])
  
  
  dw_edit_chart(chart_id ,
                title=headline,
                intro = "&nbsp;",
                annotate = "&nbsp;",
                folderId = folders_SR[c]
                )
  dw_data_to_chart(SR_results,chart_id)
  dw_publish_chart(chart_id)
  print("Datawrapper-Chart updated")
  
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
new_entry <- data.frame("Ständerat Ergebnis IT",
                          counted_cantons_SR$area_ID[c],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht_it <- rbind(grafiken_uebersicht_it,new_entry)
}


saveRDS(grafiken_uebersicht_it,"grafiken_uebersicht_it_candidates_SR.RDS")
write.xlsx(grafiken_uebersicht_it,"grafiken_uebersicht_it_candidates_SR.xlsx",row.names = FALSE)



