##Chart History DE
chart_id <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         chart_type == "proporz_history",
         language == "de") %>%
  .[,4]
dw_data_to_chart(results_history,chart_id)

#Farben anpassen
chart_metadata <- dw_retrieve_chart_metadata(chart_id)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

for ( i in 1:nrow(results_parties)) {
adapted_list$`custom-colors`[results_parties$shortname_de[i]] <- results_parties$party_color[i]
}
dw_edit_chart(chart_id,
              visualize = adapted_list,
              annotate = texts_chart[3]
              )

dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

