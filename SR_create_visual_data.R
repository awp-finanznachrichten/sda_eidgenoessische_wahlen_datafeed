for (c in 1:nrow(counted_cantons_SR)) {

selected_charts <- datawrapper_codes %>%
  filter(election_ID == counted_cantons_SR$election_ID[c],
         language %in% strsplit(counted_cantons_SR$languages[c],",")[[1]],
         visual_data_created == "no")

if (nrow(selected_charts) > 0) {
for (s in 1:nrow(selected_charts)) {
create_visual_data(selected_charts$datawrapper_ID[s],
                   upload = "no",
                   eps = "no")

#Enter in DB
  mydb <- connectDB(db_name = "sda_elections")  
  sql_qry <- paste0(
    "UPDATE datawrapper_codes SET ",
    " visual_data_created = 'yes'",
    " WHERE election_ID = '",
    counted_cantons_SR$election_ID[c],
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
print(paste0("No pending charts for canton ",counted_cantons_SR$area_ID[c]," found"))
}  

}