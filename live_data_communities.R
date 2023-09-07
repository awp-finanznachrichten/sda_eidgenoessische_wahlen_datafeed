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

#results_NR_communities_voterturnout <- data_NR_voterturnout$level_gemeinden

#Dataframe Tabellen-Output
nationalrat_gemeinden_dw <- data.frame(0,"Gemeinde","Tabelle","no_data")
colnames(nationalrat_gemeinden_dw) <- c("ID","Gemeinde","Tabelle","Staerkste_Partei")

#Dataframe Text-Output
nationalrat_gemeinden_dw_urlena <- data.frame(0,"Gemeinde","Storyboard","Text","no_data")
colnames(nationalrat_gemeinden_dw_urlena) <- c("ID","Gemeinde","Storyboard","Text","Staerkste_Partei")

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
  
  text <- "Resultat liegt noch nicht vor."
  tabelle <- "Resultat liegt noch nicht vor."
  staerkste_partei <- "no_data"
  
  #Check: Daten schon da?
  if (nrow(ergebnisse_gemeinde) > 0) {
    
  ergebnisse_gemeinde_tabelle <- ergebnisse_gemeinde %>%
      filter(!is.na(partei_staerke),
             partei_staerke >= 3 |
               differenz_partei_staerke < -3, 
             shortname_de != "weitere") %>%
      # shortname_de != "BDP") %>%
      arrange(desc(partei_staerke))
  
  ergebnisse_gemeinde_urlena <- ergebnisse_gemeinde %>%
    filter(!is.na(partei_staerke)) %>%
    arrange(desc(partei_staerke))
    
  staerkste_partei <- ergebnisse_gemeinde_tabelle$shortname_de[1]
  
  #Create Table
  tabelle <- create_table_communities(ergebnisse_gemeinde_tabelle,
                                      voter_turnout)
  
  #Find Story Ur-Lena
  storyboard_urlena <- get_storyboard_urlena(ergebnisse_gemeinde_urlena)
  print(storyboard_urlena)
  text_urlena <- get_texts(storyboard_urlena,
                           texts_spreadsheet_UrLena,
                           "de")
  print(text_urlena)

  #storyboard_urlena_all <- c(storyboard_urlena_all,storyboard_urlena) #TEST
  }  

  #New Entry Tabelle
  new_entry <- data.frame(gemeinden$gemeinde_nummer[g],
                          gemeinden$Gemeinde_KT_d[g],
                          tabelle,
                          staerkste_partei)
  colnames(new_entry) <- c("ID","Gemeinde","Tabelle","Staerkste_Partei")
  nationalrat_gemeinden_dw <- rbind(nationalrat_gemeinden_dw,new_entry)
  
  #New Entry Text Urlena
  new_entry <- data.frame(gemeinden$gemeinde_nummer[g],
                          gemeinden$Gemeinde_KT_d[g],
                          paste(storyboard_urlena,collapse="; "),
                          paste(text_urlena,collapse=" "),
                          staerkste_partei)
  colnames(new_entry) <- c("ID","Gemeinde","Storyboard","Text","Staerkste_Partei")
  nationalrat_gemeinden_dw_urlena <- rbind(nationalrat_gemeinden_dw_urlena,new_entry)
}

#Final adaptions Tabelle
nationalrat_gemeinden_dw <- nationalrat_gemeinden_dw[-1,]
nationalrat_gemeinden_dw$Tabelle <- gsub("[<]","$",nationalrat_gemeinden_dw$Tabelle)
nationalrat_gemeinden_dw$Tabelle <- gsub("[>]","£",nationalrat_gemeinden_dw$Tabelle)
nationalrat_gemeinden_dw$Tabelle <- gsub(";","¢",nationalrat_gemeinden_dw$Tabelle)
write.csv(nationalrat_gemeinden_dw,file="./Output/nationalrat_ergebnisse_parteien_gemeinden.csv",row.names = FALSE)

#Final adaptions Texts
nationalrat_gemeinden_dw_urlena <- nationalrat_gemeinden_dw_urlena[-1,]

#View(table(nationalrat_gemeinden_dw_urlena$Storyboard))
#write.xlsx(nationalrat_gemeinden_dw_urlena,"./Texte/texts_urlena.xlsx",row.names = FALSE)

###INFO###
##The following placeholders will be replaced directly in the datawrapper maps so the data file doesn't become too large (2 MB limit):

#tab_h = "<table><tr><td><b>Partei</b></td>",
#"<td><b></b></td>",
#"<td><b>Anteil</b></td>",
#"§<b>+/-</b></td></tr>"
#tab_r1 = <td><div style='width:
#tab_r2 = px; height:15px; background-color:
#tab_r3 = = ; color:white; padding:4px 4px 0px 4px; vertical-align:bottom; font-weight:bold; display:inline-block;'></div></td>"