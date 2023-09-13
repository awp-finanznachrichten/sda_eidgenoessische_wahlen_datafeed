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
                    "<td>nombre de sièges</td>",
                    "<td>différence par rapport à 2019</td>",
                    "<td>pourcentages des voix</td>",
                    "<td>différence par rapport à 2019</td>",
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
                      "<td>numero di seggi</td>",
                      "<td>variazione rispetto al 2019</td>",
                      "<td>percentuali di voto</td>",
                      "<td>variazione rispetto al 2019</td>",
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

create_table_NR_candidates <- function(elected_candidates,
                                    language) {

if (language == "de") {  
tabelle <- paste0("<table><tbody><tr><td></td>",
                  "<td></td>",
                  "<td></td></tr>")
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
    tabelle <- paste0("<table><tbody><tr><td></td>",
                      "<td></td>",
                      "<td></td></tr>")
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
    tabelle <- paste0("<table><tbody><tr><td></td>",
                      "<td></td>",
                      "<td></td></tr>")
                      #"<td>numero di voti</td>")
    
    for (e in 1:nrow(elected_candidates)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",elected_candidates$firstname[e]," ",elected_candidates$lastname[e],"</td>",
                        "<td>",elected_candidates$shortname_it[e],"</td>",
                        "<td>",elected_candidates$status_text_it[e],"</td>",
                        "<td>",format(elected_candidates$votes[e],big.mark = "'"),"</td>",
                        "</tr>")
    }                    
  }
tabelle <- paste0(tabelle,"</tbody></table>")

return(tabelle)    
}    


create_table_overview <- function(data_canton,
                                    language) {
  
  if (language == "de") {  
    ##Create Table DE
    tabelle <- "<table>
<tr>
<td><b>Partei</b></td>
<td><b>Sitze</b></td>
<td><b>+/-</b></td>
<td><b>Wähleranteil</b></td>
<td style='text-align: center'><b>+/-</b></td>
</tr>"
    
 for (i in 1:nrow(data_canton)) {
      tabelle <- paste0(tabelle,
                        "<tr><td>",data_canton$shortname_de[i],"</td>",
                        "<td style='text-align: center'>",data_canton$seats[i],"</td>")
      
      if (data_canton$seats_change[i] > 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: darkblue'><b>+",data_canton$seats_change[i],"</b></td>")  
      } else if (data_canton$seats_change[i] < 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: red'><b>",data_canton$seats_change[i],"</b></td>")  
      } else {
        tabelle <- paste0(tabelle,"<td style='text-align: center'>-</td>")
      }  
      tabelle <- paste0(tabelle,
                        "<td style='text-align: center'>",format(round2(data_canton$voter_share[i],1),nsmall=1),"%</td>")
      
      if (data_canton$voter_share_change[i] > 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: darkblue'><b>+",format(round2(data_canton$voter_share_change[i],1),nsmall=1),"</b></td></tr>")  
      } else if (data_canton$voter_share_change[i] < 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: red'><b>",format(round2(data_canton$voter_share_change[i],1),nsmall=1),"</b></td></tr>")  
      } else {
        tabelle <- paste0(tabelle,"<td style='text-align: center'>-</td></tr>")
      }  
      
    }      
    
  }    

  if (language == "fr") {  
  ###TO DO###
  }
  
  
  if (language == "it") {  
  ###TO DO###
  }
  
  tabelle <- paste0(tabelle,"</tbody></table>")
  return(tabelle)    
}    


