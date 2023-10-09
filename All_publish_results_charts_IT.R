##Chart Overview IT
chart_id <- datawrapper_codes %>%
  filter(election_ID == "2023-10-22_CH_NR",
         chart_type == "proporz_overview",
         language == "it") %>%
  .[,4]

dw_data_to_chart(results_parties[c(3,4:7)],chart_id)

#Farben anpassen
chart_metadata <- dw_retrieve_chart_metadata(chart_id)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  adapted_list$`custom-colors`[results_parties$shortname_it[i]] <- results_parties$party_color[i]
  adapted_list$`highlighted-series`[[i]] <- results_parties$shortname_it[i]
}


dw_edit_chart(chart_id,
              visualize = adapted_list,
              intro = texts_chart_it[1]
)

dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

##Chart Overview Special IT
chart_id <- datawrapper_codes %>%
  filter(election_ID == "2023-10-22_CH_NR",
         chart_type == "proporz_overview_special",
         language == "it") %>%
  .[,4]

dw_data_to_chart(results_parties[c(3,4:7)],chart_id)

#Farben anpassen
chart_metadata <- dw_retrieve_chart_metadata(chart_id)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  adapted_list$`custom-colors`[results_parties$shortname_it[i]] <- results_parties$party_color[i]
  adapted_list$`highlighted-series`[[i]] <- results_parties$shortname_it[i]
}


dw_edit_chart(chart_id,
              visualize = adapted_list
)

dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

##Chart Votes DE
chart_id <- datawrapper_codes %>%
  filter(election_ID == "2023-10-22_CH_NR",
         chart_type == "proporz_votes",
         language == "it") %>%
  .[,4]
dw_data_to_chart(results_parties[,c(13,4)],chart_id)

#Farben anpassen
chart_metadata <- dw_retrieve_chart_metadata(chart_id)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

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
              annotate = texts_chart_it[3]
)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")


##Chart Seats DE
chart_id <- datawrapper_codes %>%
  filter(election_ID == "2023-10-22_CH_NR",
         chart_type == "proporz_seats",
         language == "it") %>%
  .[,4]
dw_data_to_chart(results_parties[,c(14,6)],chart_id)

#Farben anpassen
chart_metadata <- dw_retrieve_chart_metadata(chart_id)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

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
              annotate = texts_chart_it[3]
)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

