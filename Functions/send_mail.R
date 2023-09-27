send_mail <- function(type = "",
                      recipients= "robot-notification@awp.ch") {


if (type == "NR_Results") {
  selected_charts <- datawrapper_codes %>%
    filter(election_ID == counted_cantons$election_ID[c],
           language == "de",
           chart_type == "proporz_overview" |
             chart_type == "proporz_votes" | 
             chart_type == "proporz_seats" |
             chart_type == "proporz_history")
  
  
  Subject <- paste0("Kanton ",counted_cantons$area_name_de[c],": Endergebnis Sitzverteilung/Wähleranteil")
  Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                 "Die definitive Sitzverteilung und der Wähleranteile des Kantons ",counted_cantons$area_name_de[c]," sind bekannt. Ihr findet die Meldungen dazu im Mars.\n\n",
                 "Es wurden folgende Grafiken erstellt:\n",
                 "Ergebnisse: https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[2],"/\n",
                 "Wähleranteile: https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[4],"/\n",
                 "Sitzverteilung: https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[3],"/\n",
                 "Historische Parteistärke: https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[1],"/\n\n",
                 "Es sind bereits folgende Kantone ausgezählt: ",paste(counted_cantons$area_name_de, collapse= ", "),"\n\n",
                 "Die aktualisierten CSVs für die Flourish-Parlamentsgrafik findet ihr unter folgenden Links (zum Download Rechtsklick und 'Speichern unter' wählen):\n",
                 "DE: https://raw.githubusercontent.com/awp-finanznachrichten/sda_eidgenoessische_wahlen_datafeed/main/Output/parliament_NR_overview_de.csv\n",
                 "FR: https://raw.githubusercontent.com/awp-finanznachrichten/sda_eidgenoessische_wahlen_datafeed/main/Output/parliament_NR_overview_fr.csv\n",
                 "IT: https://raw.githubusercontent.com/awp-finanznachrichten/sda_eidgenoessische_wahlen_datafeed/main/Output/parliament_NR_overview_it.csv\n\n",
                 "Liebe Grüsse\n\nLENA")
  send_notification(Subject,Body,recipients)
}
  
if (type == "NR_Candidates") {
  selected_charts <- datawrapper_codes %>%
    filter(election_ID == counted_cantons$election_ID[c],
           language == "de",
           chart_type == "proporz_elected")
  
  Subject <- paste0("Kanton ",counted_cantons$area_name_de[c],": Endergebnis gewählte Nationalratsmitglieder")
  Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                 "Die gewählten Nationalratsmitglieder des Kantons ",counted_cantons$area_name_de[c]," sind bekannt. Ihr findet die Meldungen dazu im Mars.\n\n",
                 "Es wurde folgende Grafik erstellt:\n",
                 "Gewählte Nationalratsmitglieder: https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[1],"/\n\n",
                 "Das aktualisierte CSV für die Flourish-Grafik der Gewählten findet ihr unter folgenden Link (zum Download Rechtsklick und 'Speichern unter' wählen):\n",
                 "https://raw.githubusercontent.com/awp-finanznachrichten/sda_eidgenoessische_wahlen_datafeed/main/Output/elected_candidates_overall.csv\n\n",
                 "Liebe Grüsse\n\nLENA")
  send_notification(Subject,Body,recipients)
}  
 
  if (type == "NR_Overview") {
    Subject <- paste0("Nationalrat: Neue Zwischenstandsmeldung bereit")
    Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                   "Eine neue Meldung zum Zwischenstand bei der Sitzverteilung des Nationalrates wurde erstellt. Ihr findet diese im Mars.\n\n",
                   "Es sind bereits folgende Kantone ausgezählt: ",paste(counted_cantons$area_name_de, collapse= ", "),"\n\n",
                   "Liebe Grüsse\n\nLENA")
    send_notification(Subject,Body,recipients)
  }    
  
  
if (type == "SR_Candidates") {
  selected_charts <- datawrapper_codes %>%
    filter(election_ID == counted_cantons_SR$election_ID[c],
           language == "de",
           chart_type == "majorz_votes")
  
  Subject <- paste0("Kanton ",counted_cantons_SR$area_name_de[c],": Endergebnis Ständerat")
  Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                 "Die Ständeratergebnisse des Kantons ",counted_cantons_SR$area_name_de[c]," sind bekannt. Ihr findet die Meldungen dazu im Mars.\n\n",
                 "Es wurde folgende Grafik erstellt:\n",
                 "Ergebnisse Ständerat: https://www.datawrapper.de/_/",selected_charts$datawrapper_ID[1],"/\n\n",
                 "Es sind bereits folgende Ständerats-Ergebnisse bekannt: ",paste(counted_cantons_SR$area_name_de, collapse= ", "),"\n\n",
                 "Das aktualisierte CSV für die Flourish-Grafik der Gewählten findet ihr unter folgenden Link (zum Download Rechtsklick und 'speichern unter'):\n",
                 "https://raw.githubusercontent.com/awp-finanznachrichten/sda_eidgenoessische_wahlen_datafeed/main/Output/elected_candidates_overall.csv\n\n",
                 "Liebe Grüsse\n\nLENA")
  send_notification(Subject,Body,recipients)
  
}  

}