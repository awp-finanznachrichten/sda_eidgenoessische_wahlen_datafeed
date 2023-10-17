source("load_databases.R")
counted_cantons_all <- election_metadata %>%
  filter(date == "2023-10-22",
         grepl("finished",status) == TRUE) %>% 
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton")

counted_cantons <- counted_cantons_all %>%
  filter(council == "NR")
counted_cantons_SR <- counted_cantons_all %>%
  filter(council == "SR")
if (nrow(counted_cantons) > 0) {
source("NR_create_visual_data.R")
}  
source("SR_create_visual_data.R")