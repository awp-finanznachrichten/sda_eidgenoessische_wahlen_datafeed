counted_cantons <- sum(grepl("all_counted",overview_cantons$status))

if (counted_cantons == 0) {
intro_text_de <- paste0("Live-Zwischenstand von <b>",format(Sys.time(),"%H:%M")," Uhr</b>. Es sind noch keine Kantone komplett ausgezählt.")
intro_text_fr <- paste0("Résultat en direct à <b>",format(Sys.time(),"%Hh%M"),"</b>. Les résultats complets d'un canton ne sont pas encore disponibles.")
intro_text_it <- paste0("Risultato in diretta alle <b>",format(Sys.time(),"%H:%M"),"</b>. Non sono ancora disponibili risultati completi di un cantone.")
} else if (counted_cantons == 26) {
intro_text_de <- paste0("Endergebnis vom ",format(Sys.Date(),"%d.%m.%Y")," ",format(Sys.time(),"%H:%M")," Uhr.")
intro_text_fr <- paste0("Résultat final du ",format(Sys.Date(),"%d.%m.%Y")," à ",format(Sys.time(),"%Hh%M"),".")
intro_text_it <- paste0("Risultato finale del ",format(Sys.Date(),"%d.%m.%Y")," alle ",format(Sys.time(),"%Hh%M"),".") 
} else {
intro_text_de <- paste0("Live-Zwischenstand von <b>",format(Sys.time(),"%H:%M")," Uhr</b>. Es sind <b>",counted_cantons,"</b> der 26 Kantone komplett ausgezählt.")
intro_text_fr <- paste0("Résultat en direct à <b>",format(Sys.time(),"%Hh%M"),"</b>. Les résultats complets de <b>",counted_cantons,"</b> des 26 cantons sont disponibles.")
intro_text_it <- paste0("Risultato in diretta alle <b>",format(Sys.time(),"%H:%M"),"</b>. Sono disponibili i risultati completi di <b>",counted_cantons,"</b> dei 26 cantoni.")
}  

##Chart Overview DE
chart_id <- "LsCy0"
dw_data_to_chart(overview_cantons,chart_id)
dw_edit_chart(chart_id,
              intro = intro_text_de)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

##Chart Overview FR
chart_id <- "W5pIB"
dw_data_to_chart(overview_cantons,chart_id)
dw_edit_chart(chart_id,
              intro = intro_text_fr)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

##Chart Overview IT
chart_id <- "qeVQQ"
dw_data_to_chart(overview_cantons,chart_id)
dw_edit_chart(chart_id,
              intro = intro_text_it)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")