create_table_communities <- function(ergebnisse_gemeinde,
                                     voter_turnout,
                                     language = "de") {

    tabelle <- "tab_h"
    
    if (language == "de") {
    
    tabelle <- "tab_h"  
      
    for (i in 1:nrow(ergebnisse_gemeinde)) {
      tabelle <- paste0(tabelle,
                        "<tr><td>",ergebnisse_gemeinde$shortname_de[i],"</td>",
                        "tab_r1",round2(ergebnisse_gemeinde$partei_staerke[i]*1.5),
                        "tab_r2",ergebnisse_gemeinde$party_color[i],
                        "tab_r3",
                        "<td><b>",format(round2(ergebnisse_gemeinde$partei_staerke[i],1),nsmall =1 ),"%</b></td>",
                        "§+",format(round2(ergebnisse_gemeinde$differenz_partei_staerke[i],1),nsmall=1),"</td></tr>"
      )  
    }
    tabelle <- paste0(tabelle,"</table>")
    
    #Wahlbeteiligung verfügbar?
    if (nrow(voter_turnout) == 1) {
    
    #Ergänzung Tabelle
    tabelle <- paste0(tabelle,"<br>Gültige Wahlzettel: <b>",format(voter_turnout$gueltige_wahlzettel,big.mark = "'"),"</b><br>",
                      "Wahlbeteiligung: <b>",format(round2(voter_turnout$wahlbeteiligung,1),nsmall =1),"%</b> (+",
                      format(round2(voter_turnout$differenz_wahlbeteiligung,1),nsmall=1),"%)")  
    }  

    tabelle <- gsub("[+]-","-",tabelle)
    tabelle <- gsub("[+]0[.]0%","-",tabelle)
    }
    
    if (language == "fr") {
      tabelle <- "tab_h_fr"  
      for (i in 1:nrow(ergebnisse_gemeinde)) {
        tabelle <- paste0(tabelle,
                          "<tr><td>",ergebnisse_gemeinde$shortname_fr[i],"</td>",
                          "tab_r1",round2(ergebnisse_gemeinde$partei_staerke[i]*1.5),
                          "tab_r2",ergebnisse_gemeinde$party_color[i],
                          "tab_r3",
                          "<td><b>",format(round2(ergebnisse_gemeinde$partei_staerke[i],1),nsmall =1 ),"%</b></td>",
                          "§+",format(round2(ergebnisse_gemeinde$differenz_partei_staerke[i],1),nsmall=1),"</td></tr>"
        )  
      }
      tabelle <- paste0(tabelle,"</table>")
      
      #Wahlbeteiligung verfügbar?
      if (nrow(voter_turnout) == 1) {
        
        #Ergänzung Tabelle
        tabelle <- paste0(tabelle,"<br>Bulletins de vote valables: <b>",format(voter_turnout$gueltige_wahlzettel,big.mark = "'"),"</b><br>",
                          "Taux de participation: <b>",format(round2(voter_turnout$wahlbeteiligung,1),nsmall =1),"%</b> (+",
                          format(round2(voter_turnout$differenz_wahlbeteiligung,1),nsmall=1),"%)")  
      }  
      
      tabelle <- gsub("[+]-","-",tabelle)
      tabelle <- gsub("[+]0[.]0%","-",tabelle)
    }
    
    if (language == "it") {
      
      tabelle <- "tab_h_it" 
      
      for (i in 1:nrow(ergebnisse_gemeinde)) {
        tabelle <- paste0(tabelle,
                          "<tr><td>",ergebnisse_gemeinde$shortname_it[i],"</td>",
                          "tab_r1",round2(ergebnisse_gemeinde$partei_staerke[i]*1.5),
                          "tab_r2",ergebnisse_gemeinde$party_color[i],
                          "tab_r3",
                          "<td><b>",format(round2(ergebnisse_gemeinde$partei_staerke[i],1),nsmall =1 ),"%</b></td>",
                          "§+",format(round2(ergebnisse_gemeinde$differenz_partei_staerke[i],1),nsmall=1),"</td></tr>"
        )  
      }
      tabelle <- paste0(tabelle,"</table>")
      
      #Wahlbeteiligung verfügbar?
      if (nrow(voter_turnout) == 1) {
        
        #Ergänzung Tabelle
        tabelle <- paste0(tabelle,"<br>Schede elettorali valide: <b>",format(voter_turnout$gueltige_wahlzettel,big.mark = "'"),"</b><br>",
                          "Tasso di partecipazione: <b>",format(round2(voter_turnout$wahlbeteiligung,1),nsmall =1),"%</b> (+",
                          format(round2(voter_turnout$differenz_wahlbeteiligung,1),nsmall=1),"%)")  
      }  
      
      tabelle <- gsub("[+]-","-",tabelle)
      tabelle <- gsub("[+]0[.]0%","-",tabelle)
    }

return(tabelle)    
}    