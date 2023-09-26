#Ordnerstruktur Kantone erstellen
team_id <- "6Gn1afus"

cantons_short <- unique(counted_cantons_all$area_ID)
folders_NR <- c()
folders_SR <- c()

for (canton in cantons_short) {
folder_KT <- dw_create_folder(canton,parent_id = "188044",organization_id = team_id)
folder_NR <- dw_create_folder("Nationalrat",parent_id = folder_KT$id)
folder_SR <- dw_create_folder("Ständerat",parent_id = folder_KT$id)
folders_NR <- c(folders_NR,folder_NR$id)
folders_SR <- c(folders_SR,folder_SR$id)
}  

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(counted_cantons)) {

##Chart Overview DE
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

results_parties$voter_share_change <- 0
results_parties$seats_change <- 0

results_parties$text_votes_de <- paste0("<b>",results_parties$shortname_de,"<br>",
                                        trimws(format(round2(results_parties$voter_share,1),nsmall=1)),"%",
                                        "<br>(+",trimws(format(round2(results_parties$voter_share_change,1),nsmall=1)),"%)")
results_parties$text_votes_de <- gsub("[+]-","-",results_parties$text_votes_de)
results_parties$text_votes_de <- gsub("[+]0[.]0%","-",results_parties$text_votes_de)  

results_parties$text_seats_de <- paste0("<b>",results_parties$shortname_de,"<br>",
                                        results_parties$seats,
                                        "<br>(+",results_parties$seats_change,")")
results_parties$text_seats_de <- gsub("[+]-","-",results_parties$text_seats_de)
results_parties$text_seats_de <- gsub("[+]0","-",results_parties$text_seats_de)

results_parties$shortname_de <- paste0("<br>",results_parties$shortname_de)

texts_chart <- get_text_charts(language="de",
                               elections_metadata = counted_cantons[c,])

##Chart Overview DE
data_chart <- dw_copy_chart("1c9nq")
dw_edit_chart(data_chart$id,
              title=paste0("Wahlen 2023: Ergebnisse Nationalrat Kanton ",counted_cantons$area_name_de[c]),
              folderId = folders_NR[c])
dw_data_to_chart(results_parties[c(1,4:7)],data_chart$id)
dw_publish_chart(data_chart$id)
metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)

#Farben anpassen
adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  adapted_list$`custom-colors`[results_parties$shortname_de[i]] <- results_parties$party_color[i]
  adapted_list$`highlighted-series`[[i]] <- results_parties$shortname_de[i]
}


dw_edit_chart(data_chart$id,
              visualize = adapted_list,
              intro = texts_chart[1]
)
dw_publish_chart(data_chart$id)
print("Datawrapper-Chart updated")



new_entry <- data.frame("Nationalrat Übersicht",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        metadata_chart$id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)



##Chart Votes DE
data_chart <- dw_copy_chart("7z51o")
dw_edit_chart(data_chart$id,
              title=paste0("Wahlen 2023: Wähleranteil Kanton ",counted_cantons$area_name_de[c]),
              folderId = folders_NR[c])

dw_data_to_chart(results_parties[,c(9,4)],data_chart$id)
dw_publish_chart(data_chart$id)
metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)

adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  color_label  <- paste0(results_parties$shortname_de[i],
                         trimws(format(round2(results_parties$voter_share[i],1),nsmall=1)),"%",
                         "(+",trimws(format(round2(results_parties$voter_share_change[i],1),nsmall=1)),"%)")
  
  color_label <- gsub("[+]-","-",color_label)
  color_label <- gsub("[+]0[.]0%","-",color_label)
  adapted_list$`custom-colors`[color_label] <- results_parties$party_color[i]
}
dw_edit_chart(data_chart$id,
              visualize = adapted_list,
              annotate = texts_chart[3]
)
dw_publish_chart(data_chart$id)
print("Datawrapper-Chart updated")

new_entry <- data.frame("Nationalrat Wähleranteile",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        metadata_chart$id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)


##Chart Seats DE
data_chart <- dw_copy_chart("Ne3Wp")
dw_edit_chart(data_chart$id,
              title=paste0("Wahlen 2023: Sitzverteilung Kanton ",counted_cantons$area_name_de[c]),
              folderId = folders_NR[c])

dw_data_to_chart(results_parties[,c(10,6)],data_chart$id)
dw_publish_chart(data_chart$id)
metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)

#Farben anpassen
adapted_list <- metadata_chart[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  color_label <-  paste0(results_parties$shortname_de[i],
                         results_parties$seats[i],"(+",
                         results_parties$seats_change[i],")")
  
  
  color_label <- gsub("[+]-","-",color_label)
  color_label <- gsub("[+]0","-",color_label)
  adapted_list$`custom-colors`[color_label] <- results_parties$party_color[i]
}
dw_edit_chart(data_chart$id,
              visualize = adapted_list,
              annotate = texts_chart[3]
)
dw_publish_chart(data_chart$id)
print("Datawrapper-Chart updated")

new_entry <- data.frame("Nationalrat Sitzverteilung",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        metadata_chart$id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)

}


###HISTORY CHARTS

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(counted_cantons)) {
source("NR_prepare_results_charts_history.R")

  ##Chart History DE
  data_chart <- dw_copy_chart("l8a2N")
  dw_edit_chart(data_chart$id,
                title=paste0("Wahlen 2023: Historische Parteistärken im Kanton ",counted_cantons$area_name_de[c]),
                folderId = folders_NR[c])
  dw_data_to_chart(results_history,data_chart$id)
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
  #Farben anpassen
  chart_metadata <- dw_retrieve_chart_metadata(data_chart$id)
  adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]
  
  for ( i in 1:nrow(results_parties)) {
    adapted_list$`custom-colors`[results_parties$shortname_de[i]] <- results_parties$party_color[i]
  }
  dw_edit_chart(data_chart$id,
                visualize = adapted_list,
                annotate = texts_chart[3]
  )
  
  dw_publish_chart(data_chart$id)
  print("Datawrapper-Chart updated")
  
new_entry <- data.frame("Nationalrat historische Parteistärken",
                          counted_cantons$area_ID[c],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}

saveRDS(grafiken_uebersicht,"grafiken_uebersicht_history.RDS")
write.xlsx(grafiken_uebersicht,"grafiken_uebersicht_history.xlsx",row.names = FALSE)
#grafiken_uebersicht_old <- readRDS("grafiken_uebersicht.RDS")
#folders_NR <- readRDS("folders_NR.RDS")


