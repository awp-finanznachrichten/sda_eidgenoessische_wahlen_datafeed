###GET TESTDATA COMMUNITIES
setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")
source("get_testdata_2023.R")

###LOAD RESULT AND VOTERTURNOUT DATA
#setwd("C:/Users/simon/OneDrive/sda_eidgenoessische_wahlen_daten")
#data_NR_results <-
#  fromJSON("data_NR_results.json", flatten = TRUE)
#data_NR_voterturnout <-
#  fromJSON("data_NR_voterturnout.json", flatten = TRUE)
#setwd("C:/Users/simon/OneDrive/sda_eidgenoessische_wahlen_datafeed")

#results_NR_communities <- data_NR_results$level_gemeinden
#results_NR_communities <- results_NR_communities %>%
#  left_join(parties_metadata,
#            by = join_by(partei_id == bfs_id))

#stand_ch <- data_NR_results$stand
#stand_cantons <- data_NR_results$stand_kantone

#results_NR_communities_voterturnout <- data_NR_voterturnout$level_gemeinden %>%
#  filter(gemeinde_nummer < 9000)

#results_NR_CH <- data_NR_results$level_ch

#Get elected candidates
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
  mutate(area_id = canton) %>%  #REMOVE
  filter(is.na(source_update)) %>% #REMOVE!
  filter(is.na(place_id) == FALSE) %>%
  left_join(people_metadata, join_by(person_id == id)) %>%
  left_join(parties_metadata, join_by (party_id == id))

#Make random selection for Testing
elected_candidates_overall <- elected_candidates_overall[sample(1:nrow(elected_candidates_overall),246),]

#Dataframe Output
nationalrat_gemeinden_dw <- data.frame(0,"Gemeinde_de","Gemeinde_fr","Gemeinde_it","Text_de","Text_fr","Text_it","Tabelle_de","Tabelle_fr","Tabelle_it","no_data")
colnames(nationalrat_gemeinden_dw) <- c("ID","Gemeinde_de","Gemeinde_fr","Gemeinde_it","Text_de","Text_fr","Text_it","Tabelle_de","Tabelle_fr","Tabelle_it","Staerkste_Partei")

gemeinden <- results_NR_communities %>%
  filter(gemeinde_nummer < 9000) %>%
  distinct(gemeinde_nummer,.keep_all = TRUE)

#Merge with location data
gemeinden <- gemeinden %>%
  left_join(meta_gmd_kt,
            by = join_by(gemeinde_nummer == Gemeinde_Nr))

#storyboard_urlena_all <- c() #TEST
for (g in 1:nrow(gemeinden)) {
  
  #Filter Parteien: Stärker als 3% oder mehr als 3% verloren
  ergebnisse_gemeinde <- results_NR_communities %>%
    filter(gemeinde_nummer == gemeinden$gemeinde_nummer[g],
           is.na(partei_staerke) == FALSE)
  
  voter_turnout <- results_NR_communities_voterturnout %>%
    filter(gemeinde_nummer == gemeinden$gemeinde_nummer[g],
           is.na(wahlbeteiligung) == FALSE)
  
  text_urlena_de <- "Resultat liegt noch nicht vor."
  text_urlena_fr <- "Résultats non encore disponibles."
  text_urlena_it <- "Risultato non ancora disponibile."
  tabelle_de <- "Resultat liegt noch nicht vor."
  tabelle_fr <- "Résultats non encore disponibles."
  tabelle_it <- "Risultato non ancora disponibile."
  staerkste_partei <- "no_data"
  
  #Check: Daten schon da?
  if (nrow(ergebnisse_gemeinde) > 0) {
    
  ergebnisse_gemeinde_tabelle <- ergebnisse_gemeinde %>%
      filter(!is.na(partei_staerke),
             partei_staerke >= 3) %>% #|
               #differenz_partei_staerke < -3) %>% 
             #shortname_de != "weitere") 
      # shortname_de != "BDP") %>%
      arrange(desc(partei_staerke))
  
  ergebnisse_gemeinde_urlena <- ergebnisse_gemeinde %>%
    filter(!is.na(partei_staerke)) %>%
    arrange(desc(partei_staerke))
    
  staerkste_partei <- ergebnisse_gemeinde_tabelle$shortname_de[1]
  
  #Create Table
  tabelle_de <- create_table_communities(ergebnisse_gemeinde_tabelle,
                                      voter_turnout)
  tabelle_fr <- create_table_communities(ergebnisse_gemeinde_tabelle,
                                         voter_turnout)
  tabelle_it <- create_table_communities(ergebnisse_gemeinde_tabelle,
                                         voter_turnout)
  
  #Find Story Ur-Lena
  storyboard_urlena <- get_storyboard_urlena(ergebnisse_gemeinde_urlena)

  #Seed mit Gemeinde-Nr -> So wird immer dieselbe Variante bei Gemeinde gewählt  
  set.seed(gemeinden$gemeinde_nummer[g])
  text_urlena_de <- get_texts(storyboard_urlena,
                           texts_spreadsheet_UrLena,
                           "de")
  text_urlena_fr <- get_texts(storyboard_urlena,
                              texts_spreadsheet_UrLena,
                              "fr")
  text_urlena_it <- get_texts(storyboard_urlena,
                              texts_spreadsheet_UrLena,
                              "it")
  #Replace Variables
  text_urlena_de <- replace_variables_urlena(text_urlena_de,
                                          ergebnisse_gemeinde_urlena,
                                          gemeinden
                                          )
  text_urlena_fr <- replace_variables_urlena(text_urlena_fr,
                                             ergebnisse_gemeinde_urlena,
                                             gemeinden
  )
  text_urlena_it <- replace_variables_urlena(text_urlena_it,
                                             ergebnisse_gemeinde_urlena,
                                             gemeinden
  )
  
  }  

  #New Entry
  new_entry <- data.frame(gemeinden$gemeinde_nummer[g],
                          gemeinden$Gemeinde_KT_d[g],
                          gemeinden$Gemeinde_KT_f[g],
                          gemeinden$Gemeinde_KT_i[g],
                          paste(text_urlena_de,collapse="<br><br>"),
                          paste(text_urlena_fr,collapse="<br><br>"),
                          paste(text_urlena_it,collapse="<br><br>"),
                          tabelle_de,
                          tabelle_fr,
                          tabelle_it,
                          staerkste_partei)
  colnames(new_entry) <- c("ID","Gemeinde_de","Gemeinde_fr","Gemeinde_it","Text_de","Text_fr","Text_it","Tabelle_de","Tabelle_fr","Tabelle_it","Staerkste_Partei")
  nationalrat_gemeinden_dw <- rbind(nationalrat_gemeinden_dw,new_entry)
}

###Final adaptions Tabelle
nationalrat_gemeinden_dw <- nationalrat_gemeinden_dw[-1,]
nationalrat_gemeinden_dw$Tabelle_de <- gsub("[<]","$",nationalrat_gemeinden_dw$Tabelle_de)
nationalrat_gemeinden_dw$Tabelle_de <- gsub("[>]","£",nationalrat_gemeinden_dw$Tabelle_de)
nationalrat_gemeinden_dw$Tabelle_de <- gsub(";","¢",nationalrat_gemeinden_dw$Tabelle_de)
nationalrat_gemeinden_dw$Tabelle_fr <- gsub("[<]","$",nationalrat_gemeinden_dw$Tabelle_fr)
nationalrat_gemeinden_dw$Tabelle_fr <- gsub("[>]","£",nationalrat_gemeinden_dw$Tabelle_fr)
nationalrat_gemeinden_dw$Tabelle_fr <- gsub(";","¢",nationalrat_gemeinden_dw$Tabelle_fr)
nationalrat_gemeinden_dw$Tabelle_it <- gsub("[<]","$",nationalrat_gemeinden_dw$Tabelle_it)
nationalrat_gemeinden_dw$Tabelle_it <- gsub("[>]","£",nationalrat_gemeinden_dw$Tabelle_it)
nationalrat_gemeinden_dw$Tabelle_it <- gsub(";","¢",nationalrat_gemeinden_dw$Tabelle_it)

write.csv(nationalrat_gemeinden_dw[,c(1,2,8,11)],file="./Output/nationalrat_ergebnisse_gemeinden_tabelle_de.csv",row.names = FALSE)
write.csv(nationalrat_gemeinden_dw[,c(1,3,9,11)],file="./Output/nationalrat_ergebnisse_gemeinden_tabelle_fr.csv",row.names = FALSE)
write.csv(nationalrat_gemeinden_dw[,c(1,4,10,11)],file="./Output/nationalrat_ergebnisse_gemeinden_tabelle_it.csv",row.names = FALSE)

###SPECIAL TEXT PARTS###
included_communities <- c()

##Add special texts if CH counted
if (stand_ch$wahl_abgeschlossen == TRUE) {
  
  #Parties
  nationalrat_gemeinden_dw <- add_parties(nationalrat_gemeinden_dw,
                      results_NR_communities,
                      texts_spreadsheet_UrLena,
                      area = "ch")

  #Participation
  nationalrat_gemeinden_dw <- add_participations(nationalrat_gemeinden_dw,
                             results_NR_communities_voterturnout,
                             texts_spreadsheet_UrLena,
                             area = "ch")
}  

##Add special texts if Canton counted
for (c in 1:nrow(stand_cantons)) {
  if (stand_cantons$kanton_abgeschlossen[c] == TRUE) {
  
    #Parties
    nationalrat_gemeinden_dw <- add_parties(nationalrat_gemeinden_dw,
                                                   results_NR_communities,
                                                   texts_spreadsheet_UrLena,
                                                   area = "canton")
    
    #Participation
    nationalrat_gemeinden_dw <- add_participations(nationalrat_gemeinden_dw,
                                                          results_NR_communities_voterturnout,
                                                          texts_spreadsheet_UrLena,
                                                          area = "canton")
  }  
}  

##Add Nationalräte
nationalrat_gemeinden_dw <- add_elected_candidates(elected_candidates_overall,
                                               nationalrat_gemeinden_dw,
                                               texts_spreadsheet_UrLena)

###Final adaptions Texts Urlena
nationalrat_gemeinden_dw$Text_de <- gsub("<br><br><br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_de)
nationalrat_gemeinden_dw$Text_de <- gsub("<br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_de)
nationalrat_gemeinden_dw$Text_fr <- gsub("<br><br><br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_fr)
nationalrat_gemeinden_dw$Text_fr <- gsub("<br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_fr)
nationalrat_gemeinden_dw$Text_it <- gsub("<br><br><br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_it)
nationalrat_gemeinden_dw$Text_it <- gsub("<br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_it)

write.csv(nationalrat_gemeinden_dw[,c(1,2,5,11)],file="./Output/nationalrat_ergebnisse_gemeinden_urlena_de.csv",row.names = FALSE)
write.csv(nationalrat_gemeinden_dw[,c(1,3,6,11),],file="./Output/nationalrat_ergebnisse_gemeinden_urlena_fr.csv",row.names = FALSE)
write.csv(nationalrat_gemeinden_dw[,c(1,4,7,1),],file="./Output/nationalrat_ergebnisse_gemeinden_urlena_it.csv",row.names = FALSE)


#View(table(nationalrat_gemeinden_dw$Storyboard))
#write.xlsx(nationalrat_gemeinden_dw,"./Texte/texts_urlena.xlsx",row.names = FALSE)

###INFO###
##The following placeholders will be replaced directly in the datawrapper maps so the data file doesn't become too large (2 MB limit):

#tab_h = "<table><tr><td><b>Partei</b></td>",
#"<td><b></b></td>",
#"<td><b>Anteil</b></td>",
#"§<b>+/-</b></td></tr>"
#tab_r1 = <td><div style='width:
#tab_r2 = px; height:15px; background-color:
#tab_r3 = = ; color:white; padding:4px 4px 0px 4px; vertical-align:bottom; font-weight:bold; display:inline-block;'></div></td>"