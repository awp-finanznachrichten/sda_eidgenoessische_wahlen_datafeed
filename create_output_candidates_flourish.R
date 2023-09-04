#Get parties results from Canton
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM candidates_results WHERE date = '2023-10-22' AND elected = 0" #CHANGE TO elected = 1
    )
  )
elected_candidates_overall <- fetch(rs, n = -1)
dbDisconnectAll()


#Get elected candidates
elected_candidates_overall <- elected_candidates_overall %>%
  filter(is.na(source_update)) %>% #REMOVE!
  left_join(people_metadata, join_by(person_id == id)) %>%
  left_join(parties_metadata, join_by (party_id == id)) %>%
  left_join(people_profession, join_by (person_id == person_id)) %>%
  left_join(areas_metadata, join_by (area_id == area_ID)) %>%
  filter(is.na(picture) == FALSE) #REMOVE!

#Make random selection for Testing
elected_candidates_overall <- elected_candidates_overall[sample(1:nrow(elected_candidates_overall),200),]

#Transform Data
elected_candidates_overall <- elected_candidates_overall %>%
  mutate(Name = paste0(firstname," ",lastname),
         Bild = paste0("https://164.ch/grafiken_wahlen2023/Nationalrat/",picture),
         `Bisher/Neu` = ifelse(status==2,"Bisher","Neu"),
         Alter = round2(as.numeric(difftime(Sys.Date(),birthdate, units = "weeks"))/52.25),
         Rat = ifelse(council == "NR","Nationalrat","Ständerat")
         ) %>%
  rename(Beruf = title,
         Wohnort = place,
         Kanton = area_name_de,
         Partei = shortname_de
         ) %>%
  select(Name,Bild,Kanton,`Bisher/Neu`,Beruf,Wohnort,Alter,Partei,Rat)

write.csv(elected_candidates_overall,"./Output/elected_candidates_overall.csv",row.names = FALSE)

View(elected_candidates_overall)

