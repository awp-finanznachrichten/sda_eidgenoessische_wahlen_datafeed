counted_cantons_all <- election_metadata %>%
  filter(date == "2023-10-22")

#Merge with area, text and output overview
counted_cantons_all <- counted_cantons_all  %>%
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton")

###NATIONALRAT###
counted_cantons <- counted_cantons_all %>%
  filter(council == "NR")

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(counted_cantons)) {

##Chart Overview FR
#Get parties results from Canton
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM parties_results WHERE election_id = '",
      counted_cantons$election_ID[c],
      "'"
    )
  )
results_parties <- fetch(rs, n = -1)
dbDisconnectAll()

#Merge with party_metadata
results_parties <- results_parties %>%
  left_join(parties_metadata,
            by = join_by(party_ID == id))

results_parties <- results_parties %>%
  arrange(desc(seats),
          desc(voter_share)) %>%
  select(shortname_de,
         shortname_fr,
         shortname_it,
         voter_share,
         voter_share_change,
         seats,
         seats_change,
         party_color)

results_parties$voter_share <- 0
results_parties$seats <- 0
results_parties$voter_share_change <- 0
results_parties$seats_change <- 0

results_parties$text_votes_fr <- paste0("<b>",results_parties$shortname_fr,"<br>",
                                        trimws(format(round2(results_parties$voter_share,1),nsmall=1)),"%",
                                        "<br>(+",trimws(format(round2(results_parties$voter_share_change,1),nsmall=1)),"%)")
results_parties$text_votes_fr <- gsub("[+]-","-",results_parties$text_votes_fr)
results_parties$text_votes_fr <- gsub("[+]0[.]0%","-",results_parties$text_votes_fr)  

results_parties$text_seats_fr <- paste0("<b>",results_parties$shortname_fr,"<br>",
                                        results_parties$seats,
                                        "<br>(+",results_parties$seats_change,")")
results_parties$text_seats_fr <- gsub("[+]-","-",results_parties$text_seats_fr)
results_parties$text_seats_fr <- gsub("[+]0","-",results_parties$text_seats_fr)

results_parties$shortname_fr <- paste0("<br>",results_parties$shortname_fr)

texts_chart <- get_text_charts(language="fr",
                               elections_metadata = counted_cantons[c,])

##Chart Overview FR
#data_chart <- dw_copy_chart("YHhsv")
#chart_id <- data_chart$id

#Get id 
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_overview",
         language == "fr") %>%
  .[,4]


headline <- paste0("Fédérales 2023: résultat de l'élection pour le Conseil national dans le canton de ",counted_cantons$area_name_fr[c])
headline <- str_replace_all(headline,"canton de Jura","canton du Jura")
headline <- str_replace_all(headline,"canton de Tessin","canton du Tessin")
headline <- str_replace_all(headline,"dans le canton de Valais","en Valais")
headline <- str_replace_all(headline,"canton de Argovie","canton d'Argovie")
headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
headline <- str_replace_all(headline,"canton de Grisons","canton des Grisons")
headline <- str_replace_all(headline,"canton de Obwald","canton d'Obwald")
headline <- str_replace_all(headline,"canton de Uri","canton d'Uri")

dw_edit_chart(chart_id,
              title=headline,
              folderId = folders_NR_FR[c])
dw_data_to_chart(results_parties[c(2,4:7)],chart_id)
dw_publish_chart(chart_id)
metadata_chart <- dw_retrieve_chart_metadata(chart_id)

#Farben anpassen
adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  adapted_list$`custom-colors`[results_parties$shortname_fr[i]] <- results_parties$party_color[i]
  adapted_list$`highlighted-series`[[i]] <- results_parties$shortname_fr[i]
}


dw_edit_chart(chart_id,
              visualize = adapted_list,
              intro = texts_chart[1]
              #folderId = folders_NR_FR[c]
)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

new_entry <- data.frame("Nationalrat Übersicht FR",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        chart_id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)


##Chart Votes FR
#data_chart <- dw_copy_chart("q0ap1")
#chart_id <- data_chart$id

#Get id 
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_votes",
         language == "fr") %>%
  .[,4]

headline <- paste0("Fédérales 2023: pourcentages de voix pour le National obtenues dans le canton de ",counted_cantons$area_name_fr[c])
headline <- str_replace_all(headline,"canton de Jura","canton du Jura")
headline <- str_replace_all(headline,"canton de Tessin","canton du Tessin")
headline <- str_replace_all(headline,"dans le canton de Valais","en Valais")
headline <- str_replace_all(headline,"canton de Argovie","canton d'Argovie")
headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
headline <- str_replace_all(headline,"canton de Grisons","canton des Grisons")
headline <- str_replace_all(headline,"canton de Obwald","canton d'Obwald")
headline <- str_replace_all(headline,"canton de Uri","canton d'Uri")

dw_edit_chart(chart_id,
              title=headline)
              #folderId = folders_NR_FR[c])

dw_data_to_chart(results_parties[,c(9,4)],chart_id)
dw_publish_chart(chart_id)
metadata_chart <- dw_retrieve_chart_metadata(chart_id)

adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  color_label  <- paste0(results_parties$shortname_fr[i],
                         trimws(format(round2(results_parties$voter_share[i],1),nsmall=1)),"%",
                         "(+",trimws(format(round2(results_parties$voter_share_change[i],1),nsmall=1)),"%)")
  
  color_label <- gsub("[+]-","-",color_label)
  color_label <- gsub("[+]0[.]0%","-",color_label)
  adapted_list$`custom-colors`[color_label] <- results_parties$party_color[i]
}
dw_edit_chart(chart_id,
              visualize = adapted_list,
              annotate = texts_chart[3]
)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

new_entry <- data.frame("Nationalrat Wähleranteile FR",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        chart_id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)


##Chart Seats FR
#data_chart <- dw_copy_chart("dxhAy")
#chart_id <- data_chart$id
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_seats",
         language == "fr") %>%
  .[,4]

headline <- paste0("Fédérales 2023: répartition des sièges pour le National dans le canton de ",counted_cantons$area_name_fr[c])
headline <- str_replace_all(headline,"canton de Jura","canton du Jura")
headline <- str_replace_all(headline,"canton de Tessin","canton du Tessin")
headline <- str_replace_all(headline,"dans le canton de Valais","en Valais")
headline <- str_replace_all(headline,"canton de Argovie","canton d'Argovie")
headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
headline <- str_replace_all(headline,"canton de Grisons","canton des Grisons")
headline <- str_replace_all(headline,"canton de Obwald","canton d'Obwald")
headline <- str_replace_all(headline,"canton de Uri","canton d'Uri")

dw_edit_chart(chart_id,
              title=headline)
              #folderId = folders_NR_FR[c])

dw_data_to_chart(results_parties[,c(10,6)],chart_id)
dw_publish_chart(chart_id)
metadata_chart <- dw_retrieve_chart_metadata(chart_id)

#Farben anpassen
adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  color_label <-  paste0(results_parties$shortname_fr[i],
                         results_parties$seats[i],"(+",
                         results_parties$seats_change[i],")")
  
  
  color_label <- gsub("[+]-","-",color_label)
  color_label <- gsub("[+]0","-",color_label)
  adapted_list$`custom-colors`[color_label] <- results_parties$party_color[i]
}
dw_edit_chart(chart_id,
              visualize = adapted_list,
              annotate = texts_chart[3]
)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

new_entry <- data.frame("Nationalrat Sitzverteilung FR",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        chart_id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}

#saveRDS(grafiken_uebersicht,"./Preparations/grafiken_uebersicht_fr.RDS")
#write.xlsx(grafiken_uebersicht,"./Preparations/grafiken_uebersicht_fr.xlsx",row.names = FALSE)

###HISTORY CHARTS

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(counted_cantons)) {
source("NR_prepare_results_charts_history.R")

  ##Chart History FR
  #data_chart <- dw_copy_chart("c4u4l")
  #chart_id <- data_chart$id
  chart_id <- datawrapper_codes %>%
    filter(election_ID == counted_cantons$election_ID[c],
           chart_type == "proporz_history",
           language == "fr") %>%
    .[,4]
  
  headline <- paste0("Fédérales 2023: évolution de la force des partis pour le National dans le canton de ",counted_cantons$area_name_fr[c])
  headline <- str_replace_all(headline,"canton de Jura","canton du Jura")
  headline <- str_replace_all(headline,"canton de Tessin","canton du Tessin")
  headline <- str_replace_all(headline,"dans le canton de Valais","en Valais")
  headline <- str_replace_all(headline,"canton de Argovie","canton d'Argovie")
  headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
  headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
  headline <- str_replace_all(headline,"canton de Grisons","canton des Grisons")
  headline <- str_replace_all(headline,"canton de Obwald","canton d'Obwald")
  headline <- str_replace_all(headline,"canton de Uri","canton d'Uri")
  
  dw_edit_chart(chart_id,
                title=headline)
                #folderId = folders_NR_FR[c])
  dw_data_to_chart(results_history_fr,chart_id)
  dw_publish_chart(chart_id)
  metadata_chart <- dw_retrieve_chart_metadata(chart_id)

  #Farben anpassen
  chart_metadata <- dw_retrieve_chart_metadata(chart_id)
  adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]
  
  for ( i in 1:nrow(results_parties)) {
    adapted_list$`custom-colors`[results_parties$shortname_fr[i]] <- results_parties$party_color[i]
  }
  dw_edit_chart(chart_id,
                visualize = adapted_list,
                annotate = texts_chart_fr[3]
  )
  
  dw_publish_chart(chart_id)
  print("Datawrapper-Chart updated")

new_entry <- data.frame("Nationalrat historische Parteistärken FR",
                          counted_cantons$area_ID[c],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          chart_id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}

#saveRDS(grafiken_uebersicht,"./Preparations/grafiken_uebersicht_history_fr.RDS")
#write.xlsx(grafiken_uebersicht,"./Preparations/grafiken_uebersicht_history_fr.xlsx",row.names = FALSE)

