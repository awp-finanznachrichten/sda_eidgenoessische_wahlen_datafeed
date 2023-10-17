counted_cantons_all <- election_metadata %>%
  filter(date == "2023-10-22",
         grepl("finished",status) == TRUE) %>% 
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton")

#Get counted cantons NR and SR
counted_cantons <- counted_cantons_all %>%
  filter(council == "NR")
counted_cantons_SR <- counted_cantons_all %>%
  filter(council == "SR") 

#Get intermediate results SR
intermediate_cantons_SR <- election_metadata %>%
  filter(date == "2023-10-22",
         grepl("ongoing",status) == TRUE,
         remarks == "new data available") %>% 
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton",
         council == "SR")