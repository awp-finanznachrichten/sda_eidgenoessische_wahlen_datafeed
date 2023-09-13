#Average Community
parties_ergebnisse_2019 <- c(16.84,25.59,7.8,15.11,11.38,13.2)
parties_ids <- c(1,2,3,56,70,75)

differences_parties <- results_NR_communities %>%
  arrange(gemeinde_nummer,
          id) %>%
  filter(is.na(letzte_wahl_partei_staerke) == FALSE,
         gemeinde_nummer < 9000,
    id == 1 |
           id == 2 |
           id == 3 |
           id == 56 |
           id == 70 |
           id == 75 ) %>%
  group_by(gemeinde_nummer) %>%
  summarise(count = n(),
            gemeinde_bezeichnung = max(gemeinde_bezeichnung),
            diff_SP = letzte_wahl_partei_staerke[1]-parties_ergebnisse_2019[1],
            diff_SVP = letzte_wahl_partei_staerke[2]-parties_ergebnisse_2019[2],
            diff_GLP = letzte_wahl_partei_staerke[3]-parties_ergebnisse_2019[3],
            diff_FDP = letzte_wahl_partei_staerke[4]-parties_ergebnisse_2019[4],
            diff_Mitte = letzte_wahl_partei_staerke[5]-parties_ergebnisse_2019[5],
            diff_GP = letzte_wahl_partei_staerke[6]-parties_ergebnisse_2019[6]) %>%
  mutate(overall_difference_abs = abs(diff_SP) +
           abs(diff_SVP) +
           abs(diff_GLP) +
           abs(diff_FDP) +
           abs(diff_Mitte) +
           abs(diff_GP)) %>%
  arrange(overall_difference_abs)
