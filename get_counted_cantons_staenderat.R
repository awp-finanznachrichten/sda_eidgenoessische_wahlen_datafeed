counted_cantons_SR <- election_metadata %>%
  filter(date == Sys.Date(),
         grepl("finished",status) == TRUE) %>% 
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton",
         council == "SR") %>%
  mutate(seats_available_SR = ifelse(grepl("VD|AG|SH|SO|ZH",area_ID) == TRUE,
                                     seats_available_SR-1,
                                     seats_available_SR))

#Get intermediate results SR
intermediate_cantons_SR <- election_metadata %>%
  filter(date == Sys.Date(),
         grepl("ongoing",status) == TRUE,
         remarks == "new data available") %>% 
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton",
         council == "SR") %>%
  mutate(seats_available_SR = ifelse(grepl("VD|AG|SH|SO|ZH",area_ID) == TRUE,
                                     seats_available_SR-1,
                                     seats_available_SR))
