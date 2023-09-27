for (c in 1:nrow(counted_cantons)) {

selected_charts <- datawrapper_codes %>%
  filter(election_ID == counted_cantons$election_ID[c],
         language %in% strsplit(counted_cantons$languages[c],",")[[1]],
         visual_data_created == "no")

if (counted_cantons$status[c] == "parties finished") {
selected_charts <- selected_charts %>%
  filter(chart_type != "proporz_elected")
   
}
if (counted_cantons$status[c] == "candidates finished") {
selected_charts <- selected_charts %>%
    filter(chart_type == "proporz_elected") 
}

if (nrow(selected_charts) > 0) {
for (s in 1:nrow(selected_charts)) {
create_visual_data(selected_charts$datawrapper_ID[s],
                   upload = "no",
                   eps = ifelse(selected_charts$chart_type[s] == "proporz_elected" || selected_charts$chart_type[s] == "majorz_elected","no","yes"))

#Enter in DB
  mydb <- connectDB(db_name = "sda_elections")  
  sql_qry <- paste0(
    "UPDATE datawrapper_codes SET ",
    " visual_data_created = 'yes'",
    " WHERE election_ID = '",
    counted_cantons$election_ID[c],
    "' AND chart_type = '",
    selected_charts$chart_type[s],
    "' AND language = '",
    selected_charts$language[s],
    "'"
  )
  rs <- dbSendQuery(mydb, sql_qry)
  dbDisconnectAll()  
}  
} else {
print(paste0("No pending charts for canton ",counted_cantons$area_ID[c]," found"))
}  

}