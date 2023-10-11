#Get Datawrapper Codes
mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT * FROM datawrapper_codes")
datawrapper_codes <- fetch(rs,n=-1)
dbDisconnectAll()

datawrapper_codes <- datawrapper_codes %>%
  filter(grepl("2023-10-22",election_ID),
         chart_type == "majorz_votes") 

for (chart_id in datawrapper_codes$datawrapper_ID) {
#chart_id <- "YZ3kb"
chart_metadata <- dw_retrieve_chart_metadata(chart_id)
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]
adapted_list$`dark-mode-invert` <- FALSE
publish_list <- chart_metadata[["content"]][["metadata"]][["publish"]]
publish_list$autoDarkMode <- TRUE
#publish_list$`embed-width` <- 600
#publish_list$`embed-height` <- 400
#publish_list$`chart-height` <- 261

dw_edit_chart(chart_id,
              visualize = adapted_list,
              publish = publish_list)
dw_publish_chart(chart_id)
}
View(datawrapper_codes)
