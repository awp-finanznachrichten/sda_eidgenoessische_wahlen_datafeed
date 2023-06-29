setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")

nationalrat_gemeinden_dw <- data.frame(0,"Gemeinde","Tabelle","no_data")
colnames(nationalrat_gemeinden_dw) <- c("ID","Gemeinde","Tabelle","Staerkste_Partei")

gemeinden <- results_NR_communities %>%
  filter(gemeinde_nummer < 9000) %>%
  distinct(gemeinde_nummer,.keep_all = TRUE)

for (g in 1:nrow(gemeinden)) {

  ergebnisse_gemeinde <- results_NR_communities %>%
    filter(gemeinde_nummer == gemeinden$gemeinde_nummer[g],
           is.na(partei_staerke) == FALSE,
           partei_staerke > 1,
           shortname_de != "weitere") %>%
    arrange(desc(partei_staerke))
  
  tabelle <- "Resultat liegt noch nicht vor."
  staerkste_partei <- "no_data"
  
  #Check: Daten schon da?
  if (nrow(ergebnisse_gemeinde) > 0) {
    staerkste_partei <- ergebnisse_gemeinde$shortname_de[1]
    tabelle <- "tab_h"
    
    for (i in 1:nrow(ergebnisse_gemeinde)) {
      tabelle <- paste0(tabelle,
                        "<tr><td>",ergebnisse_gemeinde$shortname_de[i],"</td>",
                        "tab_r1",round2(ergebnisse_gemeinde$partei_staerke[i]*1.5),
                        "tab_r2",ergebnisse_gemeinde$party_color[i],
                        "tab_r3",
                        "<td><b>",format(round2(ergebnisse_gemeinde$partei_staerke[i],1),nsmall =1 ),"%</b></td>",
                        "§+",format(round2(ergebnisse_gemeinde$differenz_partei_staerke[i],1),nsmall=1),"%</td></tr>"
      )  
    }
    tabelle <- paste0(tabelle,"</table>")
    
    #Wahlbeteiligung verfügbar?
    voter_turnout <- results_NR_communities_voterturnout %>%
      filter(gemeinde_nummer == gemeinden$gemeinde_nummer[g],
             is.na(wahlbeteiligung) == FALSE)

    if (nrow(voter_turnout) == 1) {
    tabelle <- paste0(tabelle,"<br>Gültige Wahlzettel: <b>",format(voter_turnout$gueltige_wahlzettel,big.mark = "'"),"</b><br>",
                      "Wahlbeteiligung: <b>",format(round2(voter_turnout$wahlbeteiligung,1),nsmall =1),"%</b> (+",
                      format(round2(voter_turnout$differenz_wahlbeteiligung,1),nsmall=1),"%)")  
    }  

    tabelle <- gsub("[+]-","-",tabelle)
    tabelle <- gsub("[+]0[.]0%","unv.",tabelle)
    
  }

  new_entry <- data.frame(gemeinden$gemeinde_nummer[g],
                          gemeinden$gemeinde_bezeichnung[g],
                          tabelle,
                          staerkste_partei)
  colnames(new_entry) <- c("ID","Gemeinde","Tabelle","Staerkste_Partei")
  nationalrat_gemeinden_dw <- rbind(nationalrat_gemeinden_dw,new_entry)
  
}

nationalrat_gemeinden_dw <- nationalrat_gemeinden_dw[-1,]


nationalrat_gemeinden_dw$Tabelle <- gsub("[<]","$",nationalrat_gemeinden_dw$Tabelle)
nationalrat_gemeinden_dw$Tabelle <- gsub("[>]","£",nationalrat_gemeinden_dw$Tabelle)
nationalrat_gemeinden_dw$Tabelle <- gsub(";","¢",nationalrat_gemeinden_dw$Tabelle)

write.csv(nationalrat_gemeinden_dw,file="./Output/nationalrat_ergebnisse_parteien_gemeinden.csv",row.names = FALSE)

###INFO###
##The following placeholders will be replaced directly in the datawrapper maps so the data file doesn't become too large (2 MB limit):

#tab_h = "<table><tr><td><b>Partei</b></td>",
#"<td><b></b></td>",
#"<td><b>Anteil</b></td>",
#"§<b>+/-</b></td></tr>"
#tab_r1 = <td><div style='width:
#tab_r2 = px; height:15px; background-color:
#tab_r3 = = ; color:white; padding:4px 4px 0px 4px; vertical-align:bottom; font-weight:bold; display:inline-block;'></div></td>"

View(nationalrat_gemeinden_dw)
