get_winners_losers <- function(texts,
                              results,
                              language = "de") {

#Gewinner Parteien
if (grepl("#Parteien_Gewinner",texts[3]) == TRUE) {
winners <- results %>%
  filter(seats_change > 0)
text_winners <- ""

if (language == "de") {
for (p in 1:nrow(winners)) {
  text_winners <- paste0(text_winners,"die ",winners$shortname_de[p]," (+",winners$seats_change[p],"), ")  
  }
text_winners <- substr(text_winners,1,nchar(text_winners)-2)
text_winners <- stri_replace_last(text_winners,fixed=","," und")
texts <- gsub("#Parteien_Gewinner_d",text_winners,texts) 
} 
if (language == "fr") {
for (p in 1:nrow(winners)) {
text_winners <- paste0(text_winners,"le ",winners$shortname_fr[p]," (+",winners$seats_change[p],"), ")  
}
text_winners <- substr(text_winners,1,nchar(text_winners)-2)
text_winners <- stri_replace_last(text_winners,fixed=","," et")
text_winners <- sub(".","L",text_winners)
texts <- gsub("#Parteien_Gewinner_f",text_winners,texts) 
}  

    
} else if (grepl("#Partei_Gewinner",texts[3]) == TRUE) {
winner <- results %>%
  filter(seats_change > 0) %>%
  arrange(desc(seats_change))
if (language == "de") {
texts <- gsub("#Partei_Gewinner_d",winner$shortname_de,texts)
} 
if (language == "fr") {
texts <- gsub("#Partei_Gewinner_f",winner$shortname_fr,texts)
}  
texts <- gsub("#Sitze_Gewinn",winner$seats_change,texts) 
} 

#Verlierer Parteien
if (grepl("#Parteien_Verlierer",texts[4]) == TRUE) {
  losers <- results %>%
    filter(seats_change < 0) %>%
    arrange(seats_change)

  text_losers <- ""
  
  if (language == "de") {
  for (p in 1:nrow(losers)) {
    text_losers <- paste0(text_losers,"die ",losers$shortname_de[p]," (",losers$seats_change[p],"), ")   
  }
  text_losers <- substr(text_losers,1,nchar(text_losers)-2)
  text_losers <- stri_replace_last(text_losers,fixed=","," und")
  text_losers <- sub(".","D",text_losers)
  texts <- gsub("#Parteien_Verlierer_d",text_losers,texts)
  } 
  
  if (language == "fr") {
    for (p in 1:nrow(losers)) {
      text_losers <- paste0(text_losers,"le ",losers$shortname_fr[p]," (",losers$seats_change[p],"), ")   
    }
    text_losers <- substr(text_losers,1,nchar(text_losers)-2)
    text_losers <- stri_replace_last(text_losers,fixed=","," et")
    text_losers <- sub(".","L",text_losers)
    texts <- gsub("#Parteien_Verlierer_f",text_losers,texts)
    
  }  

} else if (grepl("#Partei_Verlierer",texts[4]) == TRUE) {
  loser <- results %>%
    filter(seats_change < 0)
if (language == "de") {
texts <- gsub("#Partei_Verlierer_d",loser$shortname_de,texts)
} 
if (language == "fr") {
texts <- gsub("#Partei_Verlierer_f",loser$shortname_fr,texts) 
}  
texts <- gsub("#Sitze_Verlust",loser$seats_change,texts) 
} 

return(texts)  
}  