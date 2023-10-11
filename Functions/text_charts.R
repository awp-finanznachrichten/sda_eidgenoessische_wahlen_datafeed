get_text_charts <- function(language_chart,
                            elections_metadata) {

intro_text <- ""
absolute_majority_text <- ""
annotate_text <- "&nbsp;"
  
if (language_chart == "de") {
  
  if (elections_metadata$type == "majorz") {
  intro_text <- paste0(elections_metadata$status_text_de,"<br>",
                       "Verfügbare Sitze: <b>",elections_metadata$seats_available_SR,"</b>")
  } else {
  intro_text <- paste0(elections_metadata$status_text_de,"<br>",
                         "Verfügbare Sitze: <b>",elections_metadata$seats_available_NR,"</b>")  
  }  
  if (elections_metadata$status == "ongoing") {
    intro_text <- paste0("Live-Zwischenstand von <b>",format(Sys.time(),"%H:%M")," Uhr</b>. ",intro_text)
  }
  
  if (elections_metadata$status == "finished") {
  if (elections_metadata$type == "majorz") {
  text_other_election <- ""
  if (elections_metadata$second_ballot == "no") {
  text_other_election <- ifelse(elections_metadata$other_election_needed == "yes",
                                  "Es ist ein zweiter Wahlgang nötig.",
                                  "Es ist kein zweiter Wahlgang nötig.")
  }
  intro_text <- paste0("Endergebnis vom ",format(Sys.Date(),"%d.%m.%Y")," ",format(Sys.time(),"%H:%M")," Uhr. ",text_other_election,"<br>",
                         "Verfügbare Sitze: <b>",elections_metadata$seats_available_SR,"</b>")
  annotate_text <- "&#x2714;&#xFE0F; = gewählt"
  } else {
  intro_text <- paste0("Endergebnis vom ",format(Sys.Date(),"%d.%m.%Y")," ",format(Sys.time(),"%H:%M")," Uhr.<br>",
                       "Verfügbare Sitze: <b>",elections_metadata$seats_available_NR,"</b>") 
  annotate_text <- paste0("Endergebnis: ",format(Sys.Date(),"%d.%m.%Y")," ",format(Sys.time(),"%H:%M Uhr"))
  }  
  }  
}

if (language_chart == "fr") {
  
  if (elections_metadata$type == "majorz") {
    intro_text <- paste0(elections_metadata$status_text_fr,"<br>",
                         "Siège(s) à repourvoir: <b>",elections_metadata$seats_available_SR,"</b>")
  } else {
    intro_text <- paste0(elections_metadata$status_text_fr,"<br>",
                         "Siège(s) à repourvoir: <b>",elections_metadata$seats_available_NR,"</b>")
  }  

  if (elections_metadata$status == "ongoing") {  
    intro_text <- paste0("Résultat en direct à <b>",format(Sys.time(),"%Hh%M"),"</b>. ",intro_text)
  }  
  if (elections_metadata$status == "finished") {
  if (elections_metadata$type == "majorz") {
    text_other_election <- ""
    if (elections_metadata$second_ballot == "no") {
    text_other_election <- ifelse(elections_metadata$other_election_needed == "yes",
                                  "Un 2e tour est nécessaire.",
                                  "Un 2e tour n'est pas nécessaire.")
    }
    intro_text <- paste0("Résultat final du ",format(Sys.Date(),"%d.%m.%Y")," à ",format(Sys.time(),"%Hh%M"),". ",text_other_election,"<br>",
                         "Siège(s) à repourvoir: <b>",elections_metadata$seats_available_SR,"</b>")
    annotate_text <- "&#x2714;&#xFE0F; = Elu(e)"
   } else {
    intro_text <- paste0("Résultat final du ",format(Sys.Date(),"%d.%m.%Y")," à ",format(Sys.time(),"%Hh%M"),".<br>",
                          "Siège(s) à repourvoir: <b>",elections_metadata$seats_available_NR,"</b>")
    annotate_text <- paste0("Résultat final: ",format(Sys.Date(),"%d.%m.%Y")," à ",format(Sys.time(),"%Hh%M"))
   }
  }
}

if (language_chart == "it") {
  if (elections_metadata$type == "majorz") {
    intro_text <- paste0(elections_metadata$status_text_it,"<br>",
                         "Seggi disponibili: <b>",elections_metadata$seats_available_SR,"</b>")
  } else {
    intro_text <- paste0(elections_metadata$status_text_it,"<br>",
                         "Seggi disponibili: <b>",elections_metadata$seats_available_NR,"</b>")  
    }  
  if (elections_metadata$status == "ongoing") { 
    intro_text <- paste0("Risultato in diretta alle <b>",format(Sys.time(),"%H:%M"),"</b>. ",intro_text)
  }
  if (elections_metadata$status == "finished") {
    if (elections_metadata$type == "majorz") {
      text_other_election <- ""
      if (elections_metadata$second_ballot == "no") {
      text_other_election <- ifelse(elections_metadata$other_election_needed == "yes",
                                    "Un secondo turno è necessario.",
                                    "Un secondo turno non è necessario.")
      }
      intro_text <- paste0("Risultato finale del ",format(Sys.Date(),"%d.%m.%Y")," alle ",format(Sys.time(),"%Hh%M"),". ",text_other_election,"<br>",
                           "Seggi disponibili: <b>",elections_metadata$seats_available_SR,"</b>")
      annotate_text <- "&#x2714;&#xFE0F; = eletto"
    } else {
      intro_text <- paste0("Risultato finale del ",format(Sys.Date(),"%d.%m.%Y")," alle ",format(Sys.time(),"%Hh%M"),".<br>",
                           "Seggi disponibili: <b>",elections_metadata$seats_available_NR,"</b>")
      annotate_text <- paste0("Risultato finale: ",format(Sys.Date(),"%d.%m.%Y")," alle ",format(Sys.time(),"%H:%M"))
    }
  }
}

#Adapt Intro: Remove NA
intro_text <- gsub("NA","",intro_text)

if (elections_metadata$type == "majorz") {
  if (language_chart == "de") {
absolute_majority_text <- paste0("absolutes Mehr: <b>",format(elections_metadata$absolute_majority,big.mark = "'"),"<b/>")
  }
if (language_chart == "fr") {
absolute_majority_text <- paste0("majorité absolue: <b>",format(elections_metadata$absolute_majority,big.mark = "'"),"<b/>")
}
if (language_chart == "it") {
absolute_majority_text <- paste0("maggioranza assoluta: <b>",format(elections_metadata$absolute_majority,big.mark = "'"),"<b/>")
}
}

return(c(intro_text,absolute_majority_text,annotate_text))  
}  

