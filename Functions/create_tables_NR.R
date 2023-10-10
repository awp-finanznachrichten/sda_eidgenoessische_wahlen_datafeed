create_table_NR_results <- function(results_parties,
                                     language) {

if (language == "de") {  
  ##Create Table DE
  tabelle <- paste0("<table><tbody><tr>",
                    "<td></td>",
                    "<td>Sitze</td>",
                    "<td>Veränderung zu 2019</td>",
                    "<td>Wähleranteil</td>",
                    "<td>Veränderung zu 2019</td>",
                    "</tr>")
  
  for (p in 1:nrow(results_parties)) {
    tabelle <- paste0(tabelle,
                      "<tr>",
                      "<td>",results_parties$shortname_de[p],"</td>",
                      "<td>",results_parties$seats[p],"</td>",
                      "<td>+",results_parties$seats_change[p],"</td>",
                      "<td>",gsub("[.]",",",format(round2(results_parties$voter_share[p],1),nsmall=1)),"%</td>",
                      "<td>+",gsub("[.]",",",format(round2(results_parties$voter_share_change[p],1),nsmall=1)),"</td>", #%P
                      "</tr>")
    
  }                    
  tabelle <- paste0(tabelle,"</tbody></table>")
  tabelle <- gsub("[+]-","-",tabelle)
  tabelle <- gsub("[+]0[<]","-<",tabelle)
  tabelle <- gsub("[+]0,0","-",tabelle) #[+]0.0[%]P
}
  
if (language == "fr") {  
  ##Create Table FR
  tabelle <- paste0("<table><tbody><tr>",
                    "<td></td>",
                    "<td>Nombre de sièges</td>",
                    "<td>Différence par rapport à 2019</td>",
                    "<td>Pourcentage de voix</td>",
                    "<td>Différence par rapport à 2019</td>",
                    "</tr>")
  
  for (p in 1:nrow(results_parties)) {
    tabelle <- paste0(tabelle,
                      "<tr>",
                      "<td>",results_parties$shortname_fr[p],"</td>",
                      "<td>",results_parties$seats[p],"</td>",
                      "<td>+",results_parties$seats_change[p],"</td>",
                      "<td>",gsub("[.]",",",format(round2(results_parties$voter_share[p],1),nsmall=1)),"%</td>",
                      "<td>+",gsub("[.]",",",format(round2(results_parties$voter_share_change[p],1),nsmall=1)),"</td>", #%P
                      "</tr>")
    
  }                    
  tabelle <- paste0(tabelle,"</tbody></table>")
  tabelle <- gsub("[+]-","-",tabelle)
  tabelle <- gsub("[+]0[<]","-<",tabelle)
  tabelle <- gsub("[+]0,0","-",tabelle) #[+]0.0[%]P
}
  
  
if (language == "it") {  
  ##Create Table FR
    tabelle <- paste0("<table><tbody><tr>",
                      "<td></td>",
                      "<td>Numero di seggi</td>",
                      "<td>Variazione rispetto al 2019</td>",
                      "<td>Percentuali di voto</td>",
                      "<td>Variazione rispetto al 2019</td>",
                      "</tr>")
    
    for (p in 1:nrow(results_parties)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",results_parties$shortname_it[p],"</td>",
                        "<td>",results_parties$seats[p],"</td>",
                        "<td>+",results_parties$seats_change[p],"</td>",
                        "<td>",gsub("[.]",",",format(round2(results_parties$voter_share[p],1),nsmall=1)),"%</td>",
                        "<td>+",gsub("[.]",",",format(round2(results_parties$voter_share_change[p],1),nsmall=1)),"</td>", #%P
                        "</tr>")
      
    }                    
    tabelle <- paste0(tabelle,"</tbody></table>")
    tabelle <- gsub("[+]-","-",tabelle)
    tabelle <- gsub("[+]0[<]","-<",tabelle)
    tabelle <- gsub("[+]0,0","-",tabelle) #[+]0.0[%]P
  }  
  
return(tabelle)    
}    

create_table_NR_overview <- function(results_ch,
                                    language) {
  
  results_ch <- results_ch %>%
    arrange(desc(seats_2023))
  
  if (language == "de") {  
    ##Create Table DE
    tabelle <- paste0("<table><tbody><tr>",
                      "<td></td>",
                      "<td>Sitze</td>",
                      "<td>Veränderung zu 2019*</td>",
                      "</tr>")
    
    for (p in 1:nrow(results_ch)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",results_ch$shortname_de[p],"</td>",
                        "<td>",results_ch$seats_2023[p],"</td>",
                        "<td>+",results_ch$seats_change[p],"</td>",
                        "</tr>")
      
    }                    
    tabelle <- paste0(tabelle,"</tbody></table>")
    tabelle <- gsub("[+]-","-",tabelle)
    tabelle <- gsub("[+]0[<]","-<",tabelle)
    tabelle <- gsub("[+]0,0","-",tabelle) #[+]0.0[%]P
  }
  
  if (language == "fr") {  
    ##Create Table FR
    tabelle <- paste0("<table><tbody><tr>",
                      "<td></td>",
                      "<td>Nombre de sièges</td>",
                      "<td>Différence par rapport à 2019*</td>",
                      "</tr>")
    
    for (p in 1:nrow(results_ch)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",results_ch$shortname_fr[p],"</td>",
                        "<td>",results_ch$seats_2023[p],"</td>",
                        "<td>+",results_ch$seats_change[p],"</td>",
                        "</tr>")
      
    }                    
    tabelle <- paste0(tabelle,"</tbody></table>")
    tabelle <- gsub("[+]-","-",tabelle)
    tabelle <- gsub("[+]0[<]","-<",tabelle)
    tabelle <- gsub("[+]0,0","-",tabelle) #[+]0.0[%]P
  }
  
  
  if (language == "it") {  
    ##Create Table FR
    tabelle <- paste0("<table><tbody><tr>",
                      "<td></td>",
                      "<td>Numero di seggi</td>",
                      "<td>Variazione rispetto al 2019*</td>",
                      "</tr>")
    
    for (p in 1:nrow(results_ch)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",results_ch$shortname_it[p],"</td>",
                        "<td>",results_ch$seats_2023[p],"</td>",
                        "<td>+",results_ch$seats_change[p],"</td>",
                        "</tr>")
      
    }                    
    tabelle <- paste0(tabelle,"</tbody></table>")
    tabelle <- gsub("[+]-","-",tabelle)
    tabelle <- gsub("[+]0[<]","-<",tabelle)
    tabelle <- gsub("[+]0,0","-",tabelle) #[+]0.0[%]P
  }  
  
  return(tabelle)    
}    





create_table_NR_candidates <- function(elected_candidates,
                                    language) {

if (language == "de") {  
tabelle <- paste0("<table><tbody>")
                  #"<tr><td></td>",
                  #"<td></td>",
                  #"<td></td></tr>")
                  #"<td>Anzahl Stimmen</td>")

for (e in 1:nrow(elected_candidates)) {
  tabelle <- paste0(tabelle,
                    "<tr>",
                    "<td>",elected_candidates$firstname[e]," ",elected_candidates$lastname[e],"</td>",
                    "<td>",elected_candidates$shortname_de[e],"</td>",
                    "<td>",elected_candidates$status_text[e],"</td>",
                    #"<td>",format(elected_candidates$votes[e],big.mark = "'"),"</td>",
                    "</tr>")
}                    
}

  if (language == "fr") {  
    tabelle <- paste0("<table><tbody>")
                      #"<tr><td></td>",
                      #"<td></td>",
                      #"<td></td></tr>")
                      #"<td>nombre de voix</td>")
    
    for (e in 1:nrow(elected_candidates)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",elected_candidates$firstname[e]," ",elected_candidates$lastname[e],"</td>",
                        "<td>",elected_candidates$shortname_fr[e],"</td>",
                        "<td>",elected_candidates$status_text_fr[e],"</td>",
                        #"<td>",format(elected_candidates$votes[e],big.mark = "'"),"</td>",
                        "</tr>")
    }                    
  }
  
  if (language == "it") {  
    tabelle <- paste0("<table><tbody>")
                      #"<tr><td></td>",
                      #"<td></td>",
                      #"<td></td></tr>")
                      #"<td>numero di voti</td>")
    
    for (e in 1:nrow(elected_candidates)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",elected_candidates$firstname[e]," ",elected_candidates$lastname[e],"</td>",
                        "<td>",elected_candidates$shortname_it[e],"</td>",
                        "<td>",elected_candidates$status_text_it[e],"</td>",
                        #"<td>",format(elected_candidates$votes[e],big.mark = "'"),"</td>",
                        "</tr>")
    }                    
  }
tabelle <- paste0(tabelle,"</tbody></table>")

return(tabelle)    
}    


