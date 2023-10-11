###Filter and sort from Left to right
overview_ch_flourish_de <- overview_ch %>%
  filter(seats_2023 != 0 |
           seats_2019 != 0) %>%
  arrange(position_parliament) %>%
  select(shortname_de,seats_2023,seats_2019) %>%
  rename(`Sitze 2023` = seats_2023,
         `Sitze 2019` = seats_2019,
         Partei = shortname_de)
  
write.csv(overview_ch_flourish_de,"./Output/parliament_NR_overview_de.csv",row.names = FALSE,fileEncoding = "latin1")

###Filter and sort from Left to right
overview_ch_flourish_fr <- overview_ch %>%
  filter(seats_2023 != 0 |
           seats_2019 != 0) %>%
  arrange(position_parliament) %>%
  select(shortname_fr,seats_2023,seats_2019) %>%
  rename(`nombre de sièges 2023` = seats_2023,
         `nombre de sièges 2019` = seats_2019,
         parti = shortname_fr)

write.csv(overview_ch_flourish_fr,"./Output/parliament_NR_overview_fr.csv",row.names = FALSE,fileEncoding = "latin1")

###Filter and sort from Left to right
overview_ch_flourish_it <- overview_ch %>%
  filter(seats_2023 != 0 |
           seats_2019 != 0) %>%
  arrange(position_parliament) %>%
  select(shortname_it,seats_2023,seats_2019) %>%
  rename(`numero di seggi 2023` = seats_2023,
         `numero di seggi 2019` = seats_2019,
         partito = shortname_it)

write.csv(overview_ch_flourish_it,"./Output/parliament_NR_overview_it.csv",row.names = FALSE,fileEncoding = "latin1")