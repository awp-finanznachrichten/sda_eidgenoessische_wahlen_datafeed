cleanup_urlena <- function(nationalrat_gemeinden_dw) {

  
  #Add Gemeinde again (just to be sure)
  for (i in 1:nrow(nationalrat_gemeinden_dw)) {
    nationalrat_gemeinden_dw$Text_de[i] <- gsub("#Gemeinde_d",str_sub(nationalrat_gemeinden_dw$Gemeinde_de[i],end=-4),nationalrat_gemeinden_dw$Text_de[i])
    nationalrat_gemeinden_dw$Text_fr[i] <- gsub("#Gemeinde_f",str_sub(nationalrat_gemeinden_dw$Gemeinde_fr[i],end=-6),nationalrat_gemeinden_dw$Text_fr[i])
    nationalrat_gemeinden_dw$Text_it[i] <- gsub("#Gemeinde_i",str_sub(nationalrat_gemeinden_dw$Gemeinde_it[i],end=-4),nationalrat_gemeinden_dw$Text_it[i])
    }                                                

  #Adapt Grüne
  nationalrat_gemeinden_dw$Text_de <- gsub("bleibt die Grüne ","bleiben die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne holt","Die Grünen holen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("die Grüne holt","die Grünen holen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne kann ","Die Grünen können ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("baut die Grüne ","bauen die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne festigt ","Die Grünen festigen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("verliert die Grüne ","verlieren die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("die zweitplatzierte Grüne ","die zweitplatzierten Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("verteidigt die Grüne ","verteidigen die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne ist ","Die Grünen sind ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("ist die Grüne ","sind die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("ist die Grüne[.] ","sind die Grünen. ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("hat die Grüne ","haben die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne hat ","Die Grünen haben ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("an der Grüne ","an den Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne erhöht ","Die Grünen erhöhen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("liegt die Grüne ","liegen die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("landet die Grüne ","landen die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("ist neu die Grüne ","sind neu die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne steigert ","Die Grünen steigern ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("muss die Grüne ","müssen die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne verliert ","Die Grünen verlieren ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne belegt ","Die Grünen belegen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("belegt die Grüne ","belegen die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("belegt die Grüne[.] ","belegen die Grünen. ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne landet ","Die Grünen landen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("geht an die Grüne","geht and die Grünen",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne sichert ","Die Grünen sichern ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("kann die Grüne","können die Grünen",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne verzeichnet ","Die Grünen verzeichnen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("kann sich die Grüne ","können sich die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne muss ","Die Grünen müssen sich ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne bleibt ","Die Grünen bleiben ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("gefolgt von der Grüne ","gefolgt von den Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne geht ","Die Grünen gehen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne erhielt ","Die Grünen erhielten ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("stehen die Grüne ","stehen die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne kommt ","Die Grünen kommen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("holt sich die Grüne ","holen sich die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne erreicht ","Die Grünen erreichen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("schneidet die Grüne ","schneiden die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("erzielt die Grüne ","erzielen die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("der Grüne[.] Diese muss ","den Grünen. Diese müssen ",nationalrat_gemeinden_dw$Text_de)
  
  nationalrat_gemeinden_dw$Text_de <- gsub("die Grüne, ","die Grünen, ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("die Grüne[.] ","die Grünen. ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("die Grüne ","die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die Grüne ","Die Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("der Grüne ","den Grünen ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("der Grüne, ","der Grünen, ",nationalrat_gemeinden_dw$Text_de)
 
  
  
  #Adapt weitere Parteien
  nationalrat_gemeinden_dw$Text_de <- gsub("die MCG ","das MCG ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Die MCG ","Das MCG ",nationalrat_gemeinden_dw$Text_de)
  nationalrat_gemeinden_dw$Text_de <- gsub("Solidarités","Bewegung Solidarités",nationalrat_gemeinden_dw$Text_de)
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

    
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Lega","La Lega",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Lega","la Lega",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Solidarités","Solidarités gagne",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Solidarités","Solidarités",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le divers ","Autres partis/listes ",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le divers ","autres partis/listes ",nationalrat_gemeinden_dw$Text_fr)
  
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s reste","Les Vert-e-s restent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s maintient sa","Les Vert-e-s maintiennent leur",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s renforce encore sa","Les Vert-e-s maintiennent leur",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s consolide sa","Les Vert-e-s consolident leur",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s perd","Les Vert-e-s perdent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s est","Les Vert-e-s sont",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s, qui réalise","le Vert-e-s, qui réalise",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("au Vert-e-s, qui termine","aux Vert-e-s, qui terminent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s maintient sa","Les Vert-e-s maintiennent leur",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s est","les Vert-e-s sont",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s est","Les Vert-e-s sont",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s, qui réalise","Les Vert-e-s, qui réalisent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s règne","Les Vert-e-s règnent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s, menace","les Vert-e-s, menacent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s est le leader incontesté","Les Vert-e-s sont les leaders incontestés",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s est le nouveau parti le plus fort","les Vert-e-s sont les nouveaux leaders",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s grimpe","Les Vert-e-s grimpent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s progresse[.] Il passe","Les Vert-e-s progressent. Ils passent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s obtient","Les Vert-e-s obtiennent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s réalise","Les Vert-e-s réalisent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s qui réalise","les Vert-e-s qui réalisent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s se classe","Les Vert-e-s se classent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s arrive","Les Vert-e-s arrivent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s progresse","Les Vert-e-s progressent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s connaît","Les Vert-e-s connaissent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s perd","Les Vert-e-s perdent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s occupe","les Vert-e-s occupent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s complète","Les Vert-e-s complètent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("figure le Vert-e-s, qui réalise","figurent les Vert-e-s, qui réalisent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s[.] Il obtient","les Vert-e-s. Ils obtiennent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s a","Les Vert-e-s ont",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s enregistre","Les Vert-e-s enregistrent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s, troisième parti, a perdu","Les Vert-e-s, troisième parti, ont perdu",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s doit","Les Vert-e-s doivent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s reste premier","Les Vert-e-s restent premiers",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s conserve","Les Vert-e-s conservent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s a obtenu","les Vert-e-s ont obtenu",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s est premier","Les Vert-e-s sont premiers",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s engrange","Les Vert-e-s engrangent",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s a enregistré","Les Vert-e-s ont enregistré",nationalrat_gemeinden_dw$Text_fr)
  
  nationalrat_gemeinden_dw$Text_fr <- gsub("Du Vert-e-s","Des Vert-e-s",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("du Vert-e-s","des Vert-e-s",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Au Vert-e-s","Aux Vert-e-s",nationalrat_gemeinden_dw$Text_fr)  
  nationalrat_gemeinden_dw$Text_fr <- gsub("au Vert-e-s","aux Vert-e-s",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("le Vert-e-s","les Vert-e-s",nationalrat_gemeinden_dw$Text_fr)
  nationalrat_gemeinden_dw$Text_fr <- gsub("Le Vert-e-s","Les Vert-e-s",nationalrat_gemeinden_dw$Text_fr)
  

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
  all_parties <- parties_metadata %>%
    filter(is.na(position_parliament) == FALSE,
           shortname_de != "weitere")
  all_parties$shortname_de <- gsub("Grüne","Grünen",all_parties$shortname_de)
  
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
