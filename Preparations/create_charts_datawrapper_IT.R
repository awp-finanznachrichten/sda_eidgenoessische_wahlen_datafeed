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

##Chart Overview IT
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

results_parties$text_votes_it <- paste0("<b>",results_parties$shortname_it,"<br>",
                                        trimws(format(round2(results_parties$voter_share,1),nsmall=1)),"%",
                                        "<br>(+",trimws(format(round2(results_parties$voter_share_change,1),nsmall=1)),"%)")
results_parties$text_votes_it <- gsub("[+]-","-",results_parties$text_votes_it)
results_parties$text_votes_it <- gsub("[+]0[.]0%","-",results_parties$text_votes_it)  

results_parties$text_seats_it <- paste0("<b>",results_parties$shortname_it,"<br>",
                                        results_parties$seats,
                                        "<br>(+",results_parties$seats_change,")")
results_parties$text_seats_it <- gsub("[+]-","-",results_parties$text_seats_it)
results_parties$text_seats_it <- gsub("[+]0","-",results_parties$text_seats_it)

results_parties$shortname_it <- paste0("<br>",results_parties$shortname_it)

texts_chart <- get_text_charts(language="it",
                               elections_metadata = counted_cantons[c,])

##Chart Overview IT
#data_chart <- dw_copy_chart("iGOt1")
#chart_id <- data_chart$id

#Get id 
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_overview",
         language == "it") %>%
  .[,4]


headline <- paste0("Federali 2023: risultati dell'elezione per il Consiglio nazionale nel canton ",counted_cantons$area_name_it[c])

dw_edit_chart(chart_id,
              title=headline)
              #folderId = folders_NR_IT[c])
dw_data_to_chart(results_parties[c(3,4:7)],chart_id)
dw_publish_chart(chart_id)
metadata_chart <- dw_retrieve_chart_metadata(chart_id)

#Farben anpassen
adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  adapted_list$`custom-colors`[results_parties$shortname_it[i]] <- results_parties$party_color[i]
  adapted_list$`highlighted-series`[[i]] <- results_parties$shortname_it[i]
}


dw_edit_chart(chart_id,
              visualize = adapted_list,
              intro = texts_chart[1]
)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

new_entry <- data.frame("Nationalrat Übersicht IT",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        chart_id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)


##Chart Votes IT
#data_chart <- dw_copy_chart("zvdX9")
#chart_id <- data_chart$id

#Get id 
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_votes",
         language == "it") %>%
  .[,4]

headline <- paste0("Federali 2023: percentuali di voti ottenuti nel canton ",counted_cantons$area_name_it[c]," per il Nazionale")

dw_edit_chart(chart_id,
              title=headline)
              #folderId = folders_NR_IT[c])

dw_data_to_chart(results_parties[,c(9,4)],chart_id)
dw_publish_chart(chart_id)
metadata_chart <- dw_retrieve_chart_metadata(chart_id)

adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  color_label  <- paste0(results_parties$shortname_it[i],
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

new_entry <- data.frame("Nationalrat Wähleranteile IT",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        chart_id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)


##Chart Seats IT
#data_chart <- dw_copy_chart("8W6uw")
#chart_id <- data_chart$id
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_seats",
         language == "it") %>%
  .[,4]

headline <- paste0("Federali 2023: ripartizione dei seggi al Nazionale per il canton ",counted_cantons$area_name_it[c])

dw_edit_chart(chart_id,
              title=headline)
              #folderId = folders_NR_IT[c])

dw_data_to_chart(results_parties[,c(10,6)],chart_id)
dw_publish_chart(chart_id)
metadata_chart <- dw_retrieve_chart_metadata(chart_id)

#Farben anpassen
adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  color_label <-  paste0(results_parties$shortname_it[i],
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

new_entry <- data.frame("Nationalrat Sitzverteilung IT",
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

#saveRDS(grafiken_uebersicht,"./Preparations/grafiken_uebersicht_it.RDS")
#write.xlsx(grafiken_uebersicht,"./Preparations/grafiken_uebersicht_it.xlsx",row.names = FALSE)

###HISTORY CHARTS

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(counted_cantons)) {
source("NR_prepare_results_charts_history.R")

  ##Chart History IT
  #data_chart <- dw_copy_chart("49LAu")
  #chart_id <- data_chart$id
  chart_id <- datawrapper_codes %>%
    filter(election_ID == counted_cantons$election_ID[c],
           chart_type == "proporz_history",
           language == "it") %>%
    .[,4]
  
  headline <- paste0("Federali 2023: evoluzione della forza dei partiti al Nazionale per il canton ",counted_cantons$area_name_it[c])

  
  dw_edit_chart(chart_id,
                title=headline)
                #folderId = folders_NR_IT[c])
  dw_data_to_chart(results_history_it,chart_id)
  dw_publish_chart(chart_id)
  metadata_chart <- dw_retrieve_chart_metadata(chart_id)

  #Farben anpassen
  chart_metadata <- dw_retrieve_chart_metadata(chart_id)
  adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]
  
  for ( i in 1:nrow(results_parties)) {
    adapted_list$`custom-colors`[results_parties$shortname_it[i]] <- results_parties$party_color[i]
  }
  dw_edit_chart(chart_id,
                visualize = adapted_list,
                annotate = "&nbsp;"
  )
  
  dw_publish_chart(chart_id)
  print("Datawrapper-Chart updated")

new_entry <- data.frame("Nationalrat historische Parteistärken IT",
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

#saveRDS(grafiken_uebersicht,"./Preparations/grafiken_uebersicht_history_it.RDS")
#write.xlsx(grafiken_uebersicht,"./Preparations/grafiken_uebersicht_history_it.xlsx",row.names = FALSE)

