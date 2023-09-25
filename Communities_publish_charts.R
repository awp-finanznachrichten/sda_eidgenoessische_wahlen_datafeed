
  
  if (sum(results$Gebiet_Ausgezaehlt) > 0 ) {
    
    undertitel_de <- paste0("Es sind <b>",sum(results$Gebiet_Ausgezaehlt),"</b> von <b>",nrow(results),
                            "</b> Gemeinden ausgezählt. Stand: <b>",
                            round(results_national$jaStimmenInProzent,1)," %</b> Ja, <b>",
                            round(100-results_national$jaStimmenInProzent,1)," %</b> Nein")
    
    undertitel_fr <- paste0("Les résultats de <b>",sum(results$Gebiet_Ausgezaehlt),"</b> des <b>",nrow(results),
                            "</b> communes sont connus. Etat: <b>",
                            round(results_national$jaStimmenInProzent,1)," %</b> oui, <b>",
                            round(100-results_national$jaStimmenInProzent,1)," %</b> non")
    
    undertitel_it <- paste0("I risultati di <b>",sum(results$Gebiet_Ausgezaehlt),"</b> dei <b>",nrow(results),
                            "</b> comuni sono noti. Stato: <b>",
                            round(results_national$jaStimmenInProzent,1)," %</b> sì, <b>",
                            round(100-results_national$jaStimmenInProzent,1)," %</b> no")
    
  }  
  
  datawrapper_codes_vorlage <- datawrapper_codes[datawrapper_codes$Vorlage == vorlagen_short[i],]
  
  #Karten Gemeinden
  dw_edit_chart(datawrapper_codes_vorlage[1,5],intro=undertitel_de,annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
  dw_publish_chart(datawrapper_codes_vorlage[1,5])
  
  dw_edit_chart(datawrapper_codes_vorlage[3,5],intro=undertitel_fr,annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
  dw_publish_chart(datawrapper_codes_vorlage[3,5])
  
  dw_edit_chart(datawrapper_codes_vorlage[5,5],intro=undertitel_it,annotate=paste0("Ultimo aggiornamento: ",format(Sys.time(),"%d.%m.%Y %H:%M")))
  dw_publish_chart(datawrapper_codes_vorlage[5,5])
  
  #Karten Kantone
  dw_edit_chart(datawrapper_codes_vorlage[2,5],intro=undertitel_de,annotate=paste0("Letzte Aktualisierung: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
  dw_publish_chart(datawrapper_codes_vorlage[2,5])
  
  dw_edit_chart(datawrapper_codes_vorlage[4,5],intro=undertitel_fr,annotate=paste0("dernière mise à jour: ",format(Sys.time(),"%d.%m.%Y %Hh%M")))
  dw_publish_chart(datawrapper_codes_vorlage[4,5])
  
  dw_edit_chart(datawrapper_codes_vorlage[6,5],intro=undertitel_it,annotate=paste0("Ultimo aggiornamento: ",format(Sys.time(),"%d.%m.%Y %H:%M")))
  dw_publish_chart(datawrapper_codes_vorlage[6,5])
}  