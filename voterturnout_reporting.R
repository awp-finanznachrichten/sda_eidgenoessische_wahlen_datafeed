voterturnout_communities <- data_NR_voterturnout$level_gemeinden %>%
  filter(gemeinde_nummer < 9000,
         is.na(wahlbeteiligung) == FALSE) %>%
  arrange(desc(wahlbeteiligung))

voterturnout_towns <- data_NR_voterturnout$level_gemeinden %>%
  filter(gemeinde_nummer < 9000,
         is.na(wahlbeteiligung) == FALSE) %>%
  arrange(desc(wahlberechtigte))

voterturnout_change <- data_NR_voterturnout$level_gemeinden %>%
  filter(gemeinde_nummer < 9000,
         is.na(wahlbeteiligung) == FALSE,
         is.na(letzte_wahl_wahlbeteiligung) == FALSE,
         wahlberechtigte >= 500) %>%
  arrange(desc(differenz_wahlbeteiligung))

voterturnout_cantons <- data_NR_voterturnout$level_kantone %>%
  filter(is.na(wahlbeteiligung) == FALSE) %>%
  arrange(desc(wahlbeteiligung))

voterturnout_ch <- data_NR_voterturnout$level_ch

text_intro <- paste0("<h2>Wahlbeteiligung Wahlen 2023</h2>",
                     "<h3>Gesamt: ",round2(voterturnout_ch$wahlbeteiligung,1),"%, ",
                     ifelse(voterturnout_ch$differenz_wahlbeteiligung > 0,
                            paste0("+",round2(voterturnout_ch$differenz_wahlbeteiligung,1)),
                            round2(voterturnout_ch$differenz_wahlbeteiligung,1)),
                     " Prozentpunkte im Vergleich zu 2019</h3>"
)

tabelle_kantone <- paste0("<table style='text-align:center; border: 1px solid black;'><tr style='background-color:lightblue'><td><b>Kanton</b></td>",
                          "<td><b>Wahlbeteiligung<br>in %</b></td>",
                          "<td><b>Differenz zu 2019<br> in Prozentpunkten</b></td></tr>")
for (c in 1:nrow(voterturnout_cantons)) {
tabelle_kantone <- paste0(tabelle_kantone,
                          "<tr><td style='text-align:left'>",voterturnout_cantons$kanton_bezeichnung[c],"</td>",
                          "<td>",round2(voterturnout_cantons$wahlbeteiligung[c],1),"</td>",
                          ifelse(voterturnout_cantons$differenz_wahlbeteiligung[c] > 0,
                                 paste0("<td>+",round2(voterturnout_cantons$differenz_wahlbeteiligung[c],1),"</td>"),
                                 paste0("<td>",round2(voterturnout_cantons$differenz_wahlbeteiligung[c],1),"</td>")),
                          "</tr>")
}  

tabelle_towns <- paste0("<table style='text-align:center; border: 1px solid black;'><tr style='background-color:lightblue'><td><b>Stadt</b></td>",
                          "<td><b>Wahlbeteiligung<br> in %</b></td>",
                          "<td><b>Differenz zu 2019<br> in Prozentpunkten</b></td></tr>")
for (c in 1:20) {
tabelle_towns <- paste0(tabelle_towns,
                            "<tr><td style='text-align:left'>",voterturnout_towns$gemeinde_bezeichnung[c],"</td>",
                            "<td>",round2(voterturnout_towns$wahlbeteiligung[c],1),"</td>",
                            ifelse(voterturnout_towns$differenz_wahlbeteiligung[c] > 0,
                                   paste0("<td>+",round2(voterturnout_towns$differenz_wahlbeteiligung[c],1),"</td>"),
                                   paste0("<td>",round2(voterturnout_towns$differenz_wahlbeteiligung[c],1),"</td>")),
                            "</tr>")
}  

tabelle_highest <- paste0("<table style='text-align:center; border: 1px solid black;'><tr style='background-color:lightgreen'><td><b>Gemeinde</b></td>",
                        "<td><b>Wahlbeteiligung<br> in %</b></td>",
                        "<td><b>Differenz zu 2019<br> in Prozentpunkten</b></td></tr>")
for (c in 1:10) {
  tabelle_highest <- paste0(tabelle_highest,
                          "<tr><td style='text-align:left'>",voterturnout_communities$gemeinde_bezeichnung[c],"</td>",
                          "<td>",round2(voterturnout_communities$wahlbeteiligung[c],1),"</td>",
                          ifelse(voterturnout_communities$differenz_wahlbeteiligung[c] > 0,
                                 paste0("<td>+",round2(voterturnout_communities$differenz_wahlbeteiligung[c],1),"</td>"),
                                 paste0("<td>",round2(voterturnout_communities$differenz_wahlbeteiligung[c],1),"</td>")),
                          "</tr>")
}  

tabelle_lowest <- paste0("<table style='text-align:center; border: 1px solid black;'><tr style='background-color:red'><td><b>Gemeinde</b></td>",
                          "<td><b>Wahlbeteiligung<br> in %</b></td>",
                          "<td><b>Differenz zu 2019<br> in Prozentpunkten</b></td></tr>")
for (c in 0:9) {
  tabelle_lowest <- paste0(tabelle_lowest,
                            "<tr><td style='text-align:left'>",voterturnout_communities$gemeinde_bezeichnung[nrow(voterturnout_communities)-c],"</td>",
                            "<td>",round2(voterturnout_communities$wahlbeteiligung[nrow(voterturnout_communities)-c],1),"</td>",
                            ifelse(voterturnout_communities$differenz_wahlbeteiligung[nrow(voterturnout_communities)-c] > 0,
                                   paste0("<td>+",round2(voterturnout_communities$differenz_wahlbeteiligung[nrow(voterturnout_communities)-c],1),"</td>"),
                                   paste0("<td>",round2(voterturnout_communities$differenz_wahlbeteiligung[nrow(voterturnout_communities)-c],1),"</td>")),
                            "</tr>")
}  

tabelle_highest_change <- paste0("<table style='text-align:center; border: 1px solid black;'><tr style='background-color:lightgreen'><td><b>Gemeinde</b></td>",
                          "<td><b>Wahlbeteiligung<br> in %</b></td>",
                          "<td><b>Differenz zu 2019<br> in Prozentpunkten</b></td></tr>")
for (c in 1:10) {
  tabelle_highest_change <- paste0(tabelle_highest_change,
                            "<tr><td style='text-align:left'>",voterturnout_change$gemeinde_bezeichnung[c],"</td>",
                            "<td>",round2(voterturnout_change$wahlbeteiligung[c],1),"</td>",
                            ifelse(voterturnout_change$differenz_wahlbeteiligung[c] > 0,
                                   paste0("<td>+",round2(voterturnout_change$differenz_wahlbeteiligung[c],1),"</td>"),
                                   paste0("<td>",round2(voterturnout_change$differenz_wahlbeteiligung[c],1),"</td>")),
                            "</tr>")
}  

tabelle_lowest_change <- paste0("<table style='text-align:center; border: 1px solid black;'><tr style='background-color:red'><td><b>Gemeinde</b></td>",
                         "<td><b>Wahlbeteiligung<br> in %</b></td>",
                         "<td><b>Differenz zu 2019<br> in Prozentpunkten</b></td></tr>")
for (c in 0:9) {
  tabelle_lowest_change <- paste0(tabelle_lowest_change,
                           "<tr><td style='text-align:left'>",voterturnout_change$gemeinde_bezeichnung[nrow(voterturnout_change)-c],"</td>",
                           "<td>",round2(voterturnout_change$wahlbeteiligung[nrow(voterturnout_change)-c],1),"</td>",
                           ifelse(voterturnout_change$differenz_wahlbeteiligung[nrow(voterturnout_change)-c] > 0,
                                  paste0("<td>+",round2(voterturnout_change$differenz_wahlbeteiligung[nrow(voterturnout_change)-c],1),"</td>"),
                                  paste0("<td>",round2(voterturnout_change$differenz_wahlbeteiligung[nrow(voterturnout_change)-c],1),"</td>")),
                           "</tr>")
}  


###Combine all text elements
text <- paste0("<html lang='de-CH'><head><meta charset='latin1'/></head>",
               "<body>",
               text_intro,
               "<h3>Wahlbeteiligung pro Kanton</h3>",
               tabelle_kantone,
               "</table>",
               "<h3>Wahlbeteiligung der 20 grössten Städte</h3>",
               tabelle_towns,
               "</table>",
               "<h3>Die 10 Gemeinden mit der grössten Wahlbeteiligung</h3>",
               tabelle_highest,
               "</table>",
               "<h3>Die 10 Gemeinden mit der tiefsten Wahlbeteiligung</h3>",
               tabelle_lowest,
               "</table>",
               "<h3>Die 10 Gemeinden mit der grössten Steigerung der Wahlbeteiligung</h3>",
               tabelle_highest_change,
               "</table>",
               "Achtung: Nur Gemeinden mit mindestens 500 Wahlberechtigten berücksichtigt",
               "<h3>Die 10 Gemeinden mit der grössten Verringerung der Wahlbeteiligung</h3>",
               tabelle_lowest_change,
               "</table>",
               "Achtung: Nur Gemeinden mit mindestens 500 Wahlberechtigten berücksichtigt",
               "</body></html>")

filename <- "voterturnout_report.html"
cat(text, file= (con <- file(paste0("Output/",filename),"w",encoding="latin1"))) 
close(con)


#Send Mail
subject <- paste0("Wahlen 2023: Analyse Wahlbeteiligung")
htmlbody <- paste0("Output/",filename)
send_html_notification(subject, 
                       htmlbody,
                       recipients=paste0(DEFAULT_EMAILS,",inland@keystone-ats.ch,suisse@keystone-ats.ch"))