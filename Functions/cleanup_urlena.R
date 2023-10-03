cleanup_urlena <- function(nationalrat_gemeinden_dw) {

all_parties <- parties_metadata %>%
  filter(is.na(position_parliament) == FALSE,
         shortname_de != "weitere")

all_parties$shortname_de <- gsub("Grüne","Grünen",all_parties$shortname_de)


  #Adapt Grüne
  nationalrat_gemeinden_dw$Text_de <- gsub("hat die Grüne ","haben die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne hat ","Die Grünen haben ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("die Grüne, ","die Grünen, ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("die Grüne ","die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne ","Die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("die MCG ","das MCG ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die MCG ","Das MCG ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("die weitere","weitere Parteien/Listen",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die weitere","Weitere Parteien/Listen",nationalrat_gemeinden_dw$Text_de)
  
  #Adapt Numbers
    nationalrat_gemeinden_dw$Text_de <- gsub(" 2 "," zwei ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 3 "," drei ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 4 "," vier ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 5 "," fünf ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 6 "," sechs ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 7 "," sieben ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 8 "," acht ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 9 "," neun ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 10 "," zehn ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 11 "," elf ",nationalrat_gemeinden_dw$Text_de)
    nationalrat_gemeinden_dw$Text_de <- gsub(" 12 "," zwölf ",nationalrat_gemeinden_dw$Text_de)


  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s a","Les Vert-e-s ont",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s","les Vert-e-s",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s ","Les Vert-e-s ",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le divers ","Autres partis/listes ",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le divers ","autres partis/listes ",nationalrat_gemeinden_dw$Text_fr)
  
  ##Französisch
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de Henniez","d'Henniez")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de Hermance","d'Hermance")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de Hermenches","d'Hermenches")
  
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Jura","canton du Jura")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Tessin","canton du Tessin")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"du canton de Valais","en Valais")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Valais","canton du Valais")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Argovie","canton d'Argovie")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"L'élu au Conseil national dans le canton d'Appenzell Rhodes-Intérieures","L'élu au Conseil national dans le canton d'Appenzell AI")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"L'élu au Conseil national dans le canton d'Appenzell Rhodes-Extérieures","L'élu au Conseil national dans le canton d'Appenzell AR")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Grisons","canton des Grisons")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Obwald","canton d'Obwald")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"canton de Uri","canton d'Uri")
  
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 1 "," un ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 2 "," deux ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 3 "," trois ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 4 "," quatre ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 5 "," cinq ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 6 "," six ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 7 "," sept ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 8 "," huit ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 9 "," neuf ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 10 "," dix ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 11 "," onze ",nationalrat_gemeinden_dw$Text_fr)
    nationalrat_gemeinden_dw$Text_fr <- gsub(" 12 "," douze ",nationalrat_gemeinden_dw$Text_fr)  
  
  
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de A","d'A") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de E","d'E")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de I","d'I") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de O","d'O") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de U","d'U")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de u","d'u")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de Yv","d'Yv")
  
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"De A","d'A") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"De E","d'E")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"De I","d'I") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"De O","d'O") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"De U","d'U")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"De Yv","d'Yv")
  
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"le A","l'A") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"le E","l'E")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"le I","l'I") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"le O","l'O") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"le U","l'U")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"le Yv","l'Yv")
  
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"Le A","L'A") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"Le E","L'E")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"Le I","L'I") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"Le O","L'O") 
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"Le U","L'U")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"Le Yv","L'Yv")
  
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de Les ","des ")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"de Le ","du ")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"à Les ","aux ")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"A Les ","Aux ")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"à Le ","au ")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"A Le ","Au ")
  nationalrat_gemeinden_dw$Text_fr <- str_replace_all(nationalrat_gemeinden_dw$Text_fr,"du Vaud","de Le Vaud")

    nationalrat_gemeinden_dw$Text_it <- gsub(" 1 "," uno ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 2 "," due ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 3 "," tre ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 4 "," quattro ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 5 "," cinque ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 6 "," sei ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 7 "," sette ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 8 "," otto ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 9 "," nove ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 10 "," dieci ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 11 "," undici ",nationalrat_gemeinden_dw$Text_it)
    nationalrat_gemeinden_dw$Text_it <- gsub(" 12 "," dodici ",nationalrat_gemeinden_dw$Text_it)  

  
  
  nationalrat_gemeinden_dw$Text_it <- gsub("Altro ","Altri partiti/liste ",nationalrat_gemeinden_dw$Text_it)
  nationalrat_gemeinden_dw$Text_it <- gsub("altro ","altri partiti/liste ",nationalrat_gemeinden_dw$Text_it)

#Make Parties Bold
for (p in 1:nrow(all_parties)) {
  nationalrat_gemeinden_dw$Text_de <- gsub(all_parties$shortname_de[p],
                                           paste0("<b>",all_parties$shortname_de[p],"</b>"),
                                           nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_fr <- gsub(all_parties$shortname_fr[p],
                                           paste0("<b>",all_parties$shortname_fr[p],"</b>"),
                                           nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_it <- gsub(all_parties$shortname_it[p],
                                           paste0("<b>",all_parties$shortname_it[p],"</b>"),
                                           nationalrat_gemeinden_dw$Text_it)
}  


  nationalrat_gemeinden_dw$Text_de <- gsub("<br><br><br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("<br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_fr <- gsub("<br><br><br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("<br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_it <- gsub("<br><br><br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_it)
  nationalrat_gemeinden_dw$Text_it <- gsub("<br><br><br><br>","<br><br>",nationalrat_gemeinden_dw$Text_it)

return(nationalrat_gemeinden_dw)  
}  
