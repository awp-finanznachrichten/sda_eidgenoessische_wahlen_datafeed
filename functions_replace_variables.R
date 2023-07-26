replace_variables <- function(texts,
                              metadata,
                              results) {
texts <- gsub("#Kanton_short",metadata$area_ID[c],texts)
texts <- gsub("#Kanton_d",metadata$area_name_de[c],texts)
texts <- gsub("#Sitze_NR",metadata$seats_available_NR[c],texts)

#Gewinner Parteien
if (grepl("#Parteien_Gewinner",texts[3]) == TRUE) {
winners <- results %>%
  filter(seats_change > 0)

text_winners <- ""
for (p in 1:nrow(winners)) {
  text_winners <- paste0(text_winners,"die ",winners$shortname_de[p]," (+",winners$seats_change[p],"), ")  
  }
text_winners <- substr(text_winners,1,nchar(text_winners)-2)
text_winners <- stri_replace_last(text_winners,fixed=","," und")

texts <- gsub("#Parteien_Gewinner_d",text_winners,texts) 
    
} else if (grepl("#Partei_Gewinner",texts[3]) == TRUE) {
winner <- results %>%
  filter(seats_change > 0) %>%
  arrange(desc(seats_change))
texts <- gsub("#Partei_Gewinner_d",winner$shortname_de,texts) 
texts <- gsub("#Sitze_Gewinn",winner$seats_change,texts) 
} 

#Verlierer Parteien
if (grepl("#Parteien_Verlierer",texts[4]) == TRUE) {
  losers <- results %>%
    filter(seats_change < 0) %>%
    arrange(seats_change)

  text_losers <- ""
  for (p in 1:nrow(losers)) {
    text_losers <- paste0(text_losers,"die ",losers$shortname_de[p]," (",losers$seats_change[p],"), ")   
  }
  text_losers <- substr(text_losers,1,nchar(text_losers)-2)
  text_losers <- stri_replace_last(text_losers,fixed=","," und")
  text_losers <- sub(".","D",text_losers)
  texts <- gsub("#Parteien_Verlierer_d",text_losers,texts) 

} else if (grepl("#Partei_Verlierer",texts[4]) == TRUE) {
  loser <- results %>%
    filter(seats_change < 0)
texts <- gsub("#Partei_Verlierer_d",loser$shortname_de,texts)
texts <- gsub("#Sitze_Verlust",loser$seats_change,texts) 
} 

#Text voted out candidates
text_voted_out <- ""
if (grepl("#Abgewaehlte_NR",texts[5]) == TRUE) {
  for (l in 1:nrow(voted_out_candidates)) {
    text_voted_out <- paste0(text_voted_out,voted_out_candidates$vorname[l]," ",voted_out_candidates$name[l],
                             " (",voted_out_candidates$shortname_de[l],"), ")  
  }
  text_voted_out <- substr(text_voted_out,1,nchar(text_voted_out)-2)
  text_voted_out <- stri_replace_last(text_voted_out,fixed=","," und")
  texts <- gsub("#Abgewaehlte_NR_d",text_voted_out,texts)
} else if (grepl("#Abgewaehlt_NR",texts[5]) == TRUE) {
text_voted_out <- paste0(text_voted_out,voted_out_candidates$vorname," ",voted_out_candidates$name,
                           " (",voted_out_candidates$shortname_de,")")  
texts <- gsub("#Abgewaehlt_NR_d",text_voted_out,texts)
}  

#Adapt Grüne
texts <- gsub("hat die Grüne ","haben die Grünen ",texts)
texts <- gsub("Die Grüne hat ","Die Grünen haben ",texts)
texts <- gsub("die Grüne, ","die Grünen, ",texts)
texts <- gsub("die Grüne ","die Grünen ",texts)

#Adapt Numbers
texts <- gsub(" 2 "," zwei ",texts)
texts <- gsub(" 3 "," drei ",texts)
texts <- gsub(" 4 "," vier ",texts)
texts <- gsub(" 5 "," fünf ",texts)
texts <- gsub(" 6 "," sechs ",texts)
texts <- gsub(" 7 "," sieben ",texts)
texts <- gsub(" 8 "," acht ",texts)
texts <- gsub(" 9 "," neun ",texts)
texts <- gsub(" 10 "," zehn ",texts)
texts <- gsub(" 11 "," elf ",texts)
texts <- gsub(" 12 "," zwölf ",texts)

return(texts)  
}  