##Chart Overview DE
chart_id <- "LsCy0"
dw_data_to_chart(overview_cantons,chart_id)
counted_cantons <- sum(grepl("all_counted",overview_cantons$status))

if (counted_cantons == 0) {
intro_text_de <- paste0("Live-Zwischenstand von <b>",format(Sys.time(),"%H:%M")," Uhr</b>. Es sind noch keine Kantone komplett ausgezählt.")
} else if (counted_cantons == 26) {
intro_text_de <- paste0("Endergebnis vom ",format(Sys.Date(),"%d.%m.%Y")," ",format(Sys.time(),"%H:%M")," Uhr.")  
} else {
intro_text_de <- paste0("Live-Zwischenstand von <b>",format(Sys.time(),"%H:%M")," Uhr</b>. Es sind <b>",counted_cantons,"</b> der 26 Kantone komplett ausgezählt.")
}  

dw_edit_chart(chart_id,
              intro = intro_text_de[1]
)
dw_publish_chart(chart_id)
print("Datawrapper-Chart updated")

overview_cantons$content_de[1]
