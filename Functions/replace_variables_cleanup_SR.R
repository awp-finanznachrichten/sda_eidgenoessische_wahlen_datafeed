replace_variables_cleanup_SR <- function(texts,
                              metadata,
                              elected_candidates,
                              language = "de",
                              type = "canton") {
  

texts <- gsub("#Kanton_short",metadata$area_ID[c],texts)
texts <- gsub("#Kanton_d",metadata$area_name_de[c],texts)
texts <- gsub("#Kanton_f",metadata$area_name_fr[c],texts)
texts <- gsub("#Kanton_i",metadata$area_name_it[c],texts)

texts <- gsub("#Datum_SR_d",paste0(day(metadata$second_ballot_date[c]),". November"),texts)
texts <- gsub("#Datum_SR_f",paste0(day(metadata$second_ballot_date[c])," novembre"),texts)
texts <- gsub("#Datum_SR_i",paste0(day(metadata$second_ballot_date[c])," novembre"),texts)

if (nrow(elected_candidates) > 0) {
texts <- gsub("#Vorname_SR1",elected_candidates$firstname[1],texts)
texts <- gsub("#Nachname_SR1",elected_candidates$lastname[1],texts)
texts <- gsub("#Partei_SR1_d",elected_candidates$shortname_de[1],texts)
texts <- gsub("#Status_SR1_d",elected_candidates$status_text[1],texts) 
texts <- gsub("#Partei_SR1_f",elected_candidates$shortname_fr[1],texts)
texts <- gsub("#Status_SR1_f",elected_candidates$status_text_fr[1],texts) 
texts <- gsub("#Partei_SR1_i",elected_candidates$shortname_it[1],texts)
texts <- gsub("#Status_SR1_i",elected_candidates$status_text_it[1],texts) 
}
if (nrow(elected_candidates) == 2) {
texts <- gsub("#Vorname_SR2",elected_candidates$firstname[2],texts)
texts <- gsub("#Nachname_SR2",elected_candidates$lastname[2],texts)
texts <- gsub("#Partei_SR2_d",elected_candidates$shortname_de[2],texts)
texts <- gsub("#Status_SR2_d",elected_candidates$status_text[2],texts) 
texts <- gsub("#Partei_SR2_f",elected_candidates$shortname_fr[2],texts)
texts <- gsub("#Status_SR2_f",elected_candidates$status_text_fr[2],texts) 
texts <- gsub("#Partei_SR2_i",elected_candidates$shortname_it[2],texts)
texts <- gsub("#Status_SR2_i",elected_candidates$status_text_it[2],texts) 
}

##TO DO##
#Datum

###Deutsch
if (language == "de") {
  
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
}

if (language == "fr") {
  ##Französisch
texts <- str_replace_all(texts,"de Henniez","d'Henniez")
texts <- str_replace_all(texts,"de Hermance","d'Hermance")
texts <- str_replace_all(texts,"de Hermenches","d'Hermenches")

texts <- str_replace_all(texts,"canton de Jura","canton du Jura")
texts <- str_replace_all(texts,"canton de Tessin","canton du Tessin")
texts <- str_replace_all(texts,"du canton de Valais","en Valais")
texts <- str_replace_all(texts,"dans le canton de Valais","en Valais")
texts <- str_replace_all(texts,"canton de Valais","canton du Valais")
texts <- str_replace_all(texts,"canton de Argovie","canton d'Argovie")
texts <- str_replace_all(texts,"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
texts <- str_replace_all(texts,"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
texts <- str_replace_all(texts,"L'élu au Conseil national dans le canton d'Appenzell Rhodes-Intérieures","L'élu au Conseil national dans le canton d'Appenzell AI")
texts <- str_replace_all(texts,"L'élu au Conseil national dans le canton d'Appenzell Rhodes-Extérieures","L'élu au Conseil national dans le canton d'Appenzell AR")
texts <- str_replace_all(texts,"canton de Grisons","canton des Grisons")
texts <- str_replace_all(texts,"canton de Obwald","canton d'Obwald")
texts <- str_replace_all(texts,"canton de Uri","canton d'Uri")

texts <- gsub(" 1 "," un ",texts)
texts <- gsub(" 2 "," deux ",texts)
texts <- gsub(" 3 "," trois ",texts)
texts <- gsub(" 4 "," quatre ",texts)
texts <- gsub(" 5 "," cinq ",texts)
texts <- gsub(" 6 "," six ",texts)
texts <- gsub(" 7 "," sept ",texts)
texts <- gsub(" 8 "," huit ",texts)
texts <- gsub(" 9 "," neuf ",texts)
texts <- gsub(" 10 "," dix ",texts)
texts <- gsub(" 11 "," onze ",texts)
texts <- gsub(" 12 "," douze ",texts)  
  
texts <- str_replace_all(texts,"de A","d'A") 
texts <- str_replace_all(texts,"de E","d'E")
texts <- str_replace_all(texts,"de I","d'I") 
texts <- str_replace_all(texts,"de O","d'O") 
texts <- str_replace_all(texts,"de U","d'U")
texts <- str_replace_all(texts,"de Yv","d'Yv")

texts <- str_replace_all(texts,"De A","d'A") 
texts <- str_replace_all(texts,"De E","d'E")
texts <- str_replace_all(texts,"De I","d'I") 
texts <- str_replace_all(texts,"De O","d'O") 
texts <- str_replace_all(texts,"De U","d'U")
texts <- str_replace_all(texts,"De Yv","d'Yv")

texts <- str_replace_all(texts,"le A","l'A") 
texts <- str_replace_all(texts,"le E","l'E")
texts <- str_replace_all(texts,"le I","l'I") 
texts <- str_replace_all(texts,"le O","l'O") 
texts <- str_replace_all(texts,"le U","l'U")
texts <- str_replace_all(texts,"le Yv","l'Yv")

texts <- str_replace_all(texts,"Le A","L'A") 
texts <- str_replace_all(texts,"Le E","L'E")
texts <- str_replace_all(texts,"Le I","L'I") 
texts <- str_replace_all(texts,"Le O","L'O") 
texts <- str_replace_all(texts,"Le U","L'U")
texts <- str_replace_all(texts,"Le Yv","L'Yv")

texts <- str_replace_all(texts,"de Les ","des ")
texts <- str_replace_all(texts,"de Le ","du ")
texts <- str_replace_all(texts,"à Les ","aux ")
texts <- str_replace_all(texts,"A Les ","Aux ")
texts <- str_replace_all(texts,"à Le ","au ")
texts <- str_replace_all(texts,"A Le ","Au ")
texts <- str_replace_all(texts,"du Vaud","de Le Vaud")

}

return(texts)  
}  