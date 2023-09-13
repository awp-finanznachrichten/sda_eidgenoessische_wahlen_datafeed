replace_variables_urlena <- function(text_urlena,
                                     ergebnisse_gemeinde_urlena,
                                     gemeinden) {

text_urlena <- gsub("#Gemeinde_d",gemeinden$Gemeinde_d[g],text_urlena)
text_urlena <- gsub("#Gemeinde_f",gemeinden$Gemeinde_f[g],text_urlena)
text_urlena <- gsub("#Gemeinde_i",gemeinden$Gemeinde_i[g],text_urlena)

ergebnisse_gemeinde_urlena  <- ergebnisse_gemeinde_urlena  %>%
  mutate(rank_current_election = rank(round2(-partei_staerke,1),ties.method = "min"),
         rank_last_election = rank(round2(-letzte_wahl_partei_staerke,1),ties.method = "min"),
         shortname_de = paste0("<b>",shortname_de,"</b>"),
         shortname_fr = paste0("<b>",shortname_fr,"</b>"),
         shortname_it = paste0("<b>",shortname_it,"</b>")
         )

text_urlena <- gsub("#Party1_name_d",ergebnisse_gemeinde_urlena$shortname_de[1],text_urlena)
text_urlena <- gsub("#Party1_name_f",ergebnisse_gemeinde_urlena$shortname_fr[1],text_urlena)
text_urlena <- gsub("#Party1_name_i",ergebnisse_gemeinde_urlena$shortname_it[1],text_urlena)

text_urlena <- gsub("#Party2_name_d",ergebnisse_gemeinde_urlena$shortname_de[2],text_urlena)
text_urlena <- gsub("#Party2_name_f",ergebnisse_gemeinde_urlena$shortname_fr[2],text_urlena)
text_urlena <- gsub("#Party2_name_i",ergebnisse_gemeinde_urlena$shortname_it[2],text_urlena)

text_urlena <- gsub("#Party3_name_d",ergebnisse_gemeinde_urlena$shortname_de[3],text_urlena)
text_urlena <- gsub("#Party3_name_f",ergebnisse_gemeinde_urlena$shortname_fr[3],text_urlena)
text_urlena <- gsub("#Party3_name_i",ergebnisse_gemeinde_urlena$shortname_it[3],text_urlena)

text_urlena <- gsub("#Party4_name_d",ergebnisse_gemeinde_urlena$shortname_de[4],text_urlena)
text_urlena <- gsub("#Party4_name_f",ergebnisse_gemeinde_urlena$shortname_fr[4],text_urlena)
text_urlena <- gsub("#Party4_name_i",ergebnisse_gemeinde_urlena$shortname_it[4],text_urlena)

text_urlena <- gsub("#Party1_voter_share",gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$partei_staerke[1],1)),text_urlena)
text_urlena <- gsub("#Party2_voter_share",gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$partei_staerke[2],1)),text_urlena)
text_urlena <- gsub("#Party3_voter_share",gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$partei_staerke[3],1)),text_urlena)

text_urlena <- gsub("#Party1_votes",ergebnisse_gemeinde_urlena$stimmen_partei[1],text_urlena)
text_urlena <- gsub("#Party2_votes",ergebnisse_gemeinde_urlena$stimmen_partei[2],text_urlena)
text_urlena <- gsub("#Party3_votes",ergebnisse_gemeinde_urlena$stimmen_partei[3],text_urlena)

ergebnisse_gemeinde_urlena
text_urlena <- gsub("#Party1_change",ifelse(grepl("[-]",ergebnisse_gemeinde_urlena$differenz_partei_staerke[1]),
                                             gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$differenz_partei_staerke[1],1)),
                                             paste0("+",gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$differenz_partei_staerke[1],1)))),
                    text_urlena)
text_urlena <- gsub("#Party2_change",ifelse(grepl("[-]",ergebnisse_gemeinde_urlena$differenz_partei_staerke[2]),
                                                 gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$differenz_partei_staerke[2],1)),
                                                 paste0("+",gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$differenz_partei_staerke[2],1)))),
                    text_urlena)
text_urlena <- gsub("#Party3_change",ifelse(grepl("[-]",ergebnisse_gemeinde_urlena$differenz_partei_staerke[3]),
                                                 gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$differenz_partei_staerke[3],1)),
                                                 paste0("+",gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$differenz_partei_staerke[3],1)))),
                    text_urlena)

text_urlena <- gsub("#Party1_distance_to_next",gsub("[.]",",",round2(ergebnisse_gemeinde_urlena$partei_staerke[1]-ergebnisse_gemeinde_urlena$partei_staerke[2],1)),text_urlena)
text_urlena <- gsub("#Party1_rank_in_commune_prev",ergebnisse_gemeinde_urlena$rank_last_election[1],text_urlena)


return(text_urlena)  
}  
