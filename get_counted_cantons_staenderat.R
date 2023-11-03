date <- Sys.Date()
if (test) {
  election_date <- "2023-11-12"
}

counted_cantons_SR <- election_metadata %>%
  filter(date == election_date,
         grepl("finished",status) == TRUE) %>% 
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton",
         council == "SR") %>%
  mutate(seats_available_SR = ifelse(grepl("FR|GE|VD|VS|AG|SH|SO|ZH|TI",area_ID) == TRUE,
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
  mutate(seats_available_SR = ifelse(grepl("FR|GE|VD|VS|AG|SH|SO|ZH|TI",area_ID) == TRUE,
                                     seats_available_SR-1,
                                     seats_available_SR))
