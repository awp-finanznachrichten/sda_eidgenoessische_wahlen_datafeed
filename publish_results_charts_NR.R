##Chart Overview DE
chart_id <- "bY6b7"
dw_data_to_chart(results_parties[c(1,4:7)],chart_id)
dw_edit_chart(chart_id,
              intro = texts_chart[1])
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")



##Chart Votes DE
chart_id <- "7z51o"
dw_data_to_chart(results_parties[,c(9,4)],chart_id)

#Farben anpassen
chart_metadata <- dw_retrieve_chart_metadata(chart_id)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  color_label  <- paste0(results_parties$shortname_de[i],
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


##Chart Seats DE
chart_id <- "Ne3Wp"
dw_data_to_chart(results_parties[,c(10,6)],chart_id)

#Farben anpassen
chart_metadata <- dw_retrieve_chart_metadata(chart_id)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
  color_label <-  paste0(results_parties$shortname_de[i],
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
