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
                    "<td>sièges</td>",
                    "<td>changement par rapport à 2019</td>",
                    "<td>pourcentages des voix</td>",
                    "<td>changement par rapport à 2019</td>",
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
  
return(tabelle)    
}    

create_table_NR_candidates <- function(elected_candidates,
                                    language) {

if (language == "de") {  
tabelle <- paste0("<table><tbody><tr><td></td>",
                  "<td></td>",
                  "<td></td>",
                  "<td>Anzahl Stimmen</td></tr>")

for (e in 1:nrow(elected_candidates)) {
  tabelle <- paste0(tabelle,
                    "<tr>",
                    "<td>",elected_candidates$vorname[e]," ",elected_candidates$name[e],"</td>",
                    "<td>",elected_candidates$shortname_de[e],"</td>",
                    "<td>",elected_candidates$status_text[e],"</td>",
                    "<td>",format(elected_candidates$votes[e],big.mark = "'"),"</td>",
                    "</tr>")
}                    
}

  if (language == "fr") {  
    tabelle <- paste0("<table><tbody><tr><td></td>",
                      "<td></td>",
                      "<td></td>",
                      "<td>nombre de voix</td></tr>")
    
    for (e in 1:nrow(elected_candidates)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",elected_candidates$vorname[e]," ",elected_candidates$name[e],"</td>",
                        "<td>",elected_candidates$shortname_fr[e],"</td>",
                        "<td>",elected_candidates$status_text_fr[e],"</td>",
                        "<td>",format(elected_candidates$votes[e],big.mark = "'"),"</td>",
                        "</tr>")
    }                    
  }
tabelle <- paste0(tabelle,"</tbody></table>")

return(tabelle)    
}    
