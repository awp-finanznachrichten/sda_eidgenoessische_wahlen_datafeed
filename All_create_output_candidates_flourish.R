#Get parties results from Canton
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM candidates_results WHERE date = '2023-10-22' AND elected = 1" #CHANGE TO elected = 1
    )
  )
elected_candidates_overall <- fetch(rs, n = -1)
dbDisconnectAll()

#Get elected candidates
elected_candidates_overall <- elected_candidates_overall %>%
  #mutate(area_id = canton) %>%  #REMOVE!
  #filter(is.na(source_update)) %>% #REMOVE!
  left_join(people_metadata, join_by(person_id == id)) %>%
  left_join(parties_metadata, join_by (party_id == id)) %>%
  left_join(people_profession, join_by (person_id == person_id)) %>%
  left_join(areas_metadata, join_by (area_id == area_ID)) %>%
  mutate(picture = ifelse(is.na(picture),
                          "Replacement.jpg",
                          picture
                          )) %>%
  arrange(canton,
          desc(council),
          party_id,
          desc(votes),
          lastname)

elected_candidates_overall[is.na(elected_candidates_overall)] <- "-"

#Make random selection for Testing
#elected_candidates_overall <- elected_candidates_overall[sample(1:nrow(elected_candidates_overall),246),] #REMOVE!

#Transform Data
elected_candidates_overall_de <- elected_candidates_overall %>%
  mutate(Name = paste0(firstname," ",lastname),
         Bild = ifelse(grepl("NR",picture),
                paste0("https://164.ch/grafiken_wahlen2023/Nationalrat/",picture),
                paste0("https://164.ch/grafiken_wahlen2023/Staenderat/",picture)),
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


elected_candidates_overall_fr <- elected_candidates_overall %>%
  mutate(Nom = paste0(firstname," ",lastname),
         Image = paste0("https://164.ch/grafiken_wahlen2023/Nationalrat/",picture),
         Etat = ifelse(status == 2,
                               ifelse(gender == "m","sortant","sortante"),
                               ifelse(gender == "m","nouveau","nouvelle")),
         Age = round2(as.numeric(difftime(Sys.Date(),birthdate, units = "weeks"))/52.25),
         Conseil = ifelse(council == "NR","Conseil national ","Conseil des Etats")
  ) %>%
  rename(Profession = title,
         Domicile = place,
         Canton = area_name_fr,
         Parti = shortname_fr
  ) %>%
  select(Nom,Image,Canton,Etat,Profession,Domicile,Age,Parti,Conseil)


elected_candidates_overall_it <- elected_candidates_overall %>%
  mutate(Nome = paste0(firstname," ",lastname),
         Immagine = paste0("https://164.ch/grafiken_wahlen2023/Nationalrat/",picture),
         Stato = ifelse(status==2,"uscente","nuovo"),
         Eta = round2(as.numeric(difftime(Sys.Date(),birthdate, units = "weeks"))/52.25),
         Consiglio = ifelse(council == "NR","Nazionale","Consiglio degli Stati")
  ) %>%
  rename(Occupazione = title,
         Residenza = place,
         Canton = area_name_it,
         Partito = shortname_it
  ) %>%
  select(Nome,Immagine,Canton,Stato,Occupazione,Residenza,Eta,Partito,Consiglio)

#elected_candidates_overall$Rat[201:246] <- "Ständerat" #REMOVE

write.csv(elected_candidates_overall_de,"./Output/elected_candidates_overall_de.csv",row.names = FALSE)
write.csv(elected_candidates_overall_fr,"./Output/elected_candidates_overall_fr.csv",row.names = FALSE)
write.csv(elected_candidates_overall_it,"./Output/elected_candidates_overall_it.csv",row.names = FALSE)
