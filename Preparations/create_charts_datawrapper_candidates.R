###CANDIDATES

###Grafiken erstellen und Daten speichern
#grafiken_uebersicht <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
#colnames(grafiken_uebersicht) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(counted_cantons)) {
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
    .[1:counted_cantons$seats_available_NR[c],] #REMOVE!
  
  texts_chart <- get_text_charts(language="de",
                                 elections_metadata = counted_cantons[c,])

  elected_candidates$image_link <- paste0("![](https://164.ch/grafiken_wahlen2023/Parlament/Replacement.jpg)")
  elected_candidates$text_de <- paste0("<b>Noch nicht bekannt</b><br>")
  
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
  #data_chart <- dw_copy_chart("YS5Cf")
  #chart_id <- data_chart$id
  
  #Get id 
  chart_id <- datawrapper_codes %>%
    filter(election_ID == counted_cantons$election_ID[c],
           chart_type == "proporz_elected",
           language == "de") %>%
    .[,4]

  dw_edit_chart(chart_id,
                title=paste0("Wahlen 2023: Die gewählten Nationalratsmitglieder im Kanton ",counted_cantons$area_name_de[c]),
                intro = texts_chart[1],
                annotate = texts_chart[3]
                )
                #folderId = folders_NR[c])
  dw_data_to_chart(elected_candidates_images,chart_id)
  dw_publish_chart(chart_id)
  print("Datawrapper-Chart updated")
  
  metadata_chart <- dw_retrieve_chart_metadata(chart_id)
  
#new_entry <- data.frame("Nationalrat gewählte Nationalratsmitglieder",
#                          counted_cantons$area_ID[c],
#                          metadata_chart$content$title,
#                          metadata_chart$content$language,
#                          chart_id,
#                          metadata_chart$content$publicUrl,
#                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
#                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
#  colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
#  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}

#saveRDS(grafiken_uebersicht,"grafiken_uebersicht_elected.RDS")
#write.xlsx(grafiken_uebersicht,"grafiken_uebersicht_elected.xlsx",row.names = FALSE)
#grafiken_uebersicht_old <- readRDS("grafiken_uebersicht.RDS")
#folders_NR <- readRDS("folders_NR.RDS")


