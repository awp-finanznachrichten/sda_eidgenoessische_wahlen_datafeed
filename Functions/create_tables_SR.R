create_table_SR_candidates <- function(results_candidates,
                                    language) {

if (language == "de") {  
tabelle <- paste0("<table><tbody><tr>",
                  "<td>Stimmen haben erhalten</td>",
                  "<td></td>",
                  "<td></td>",
                  "<td>Anzahl Stimmen</td>",
                  "<td>Gewählt</td>",
                  "</tr>")

for (e in 1:nrow(results_candidates)) {
  tabelle <- paste0(tabelle,
                    "<tr>",
                    "<td>",results_candidates$firstname[e]," ",results_candidates$lastname[e],"</td>",
                    "<td>",results_candidates$shortname_de[e],"</td>",
                    "<td>",results_candidates$status_text[e],"</td>",
                    "<td>",ifelse(results_candidates$votes[e] > 9999,
                                  format(results_candidates$votes[e],big.mark = "'"),
                                  results_candidates$votes[e]),"</td>",
                    "<td>",ifelse(results_candidates$elected[e] == 1,"ja","nein"),"</td>",
                    "</tr>")
}      
}

  if (language == "fr") {  
    tabelle <- paste0("<table><tbody><tr>",
                      "<td>Ont obtenu des voix</td>",
                      "<td></td>",
                      "<td></td>",
                      "<td>Total des voix</td>",
                      "<td>Élu(e)</td>",
                      "</tr>")
    
    for (e in 1:nrow(results_candidates)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",results_candidates$firstname[e]," ",results_candidates$lastname[e],"</td>",
                        "<td>",results_candidates$shortname_fr[e],"</td>",
                        "<td>",results_candidates$status_text_fr[e],"</td>",
                        "<td>",ifelse(results_candidates$votes[e] > 9999,
                                      format(results_candidates$votes[e],big.mark = "'"),
                                      results_candidates$votes[e]),"</td>",
                        "<td>",ifelse(results_candidates$elected[e] == 1,"oui","non"),"</td>",
                        "</tr>")
    }
    tabelle <- gsub("Vereinzelte","Autres",tabelle)
  }
  
  if (language == "it") {  
    tabelle <- paste0("<table><tbody><tr>",
                      "<td>Hanno ottenuto voti</td>",
                      "<td></td>",
                      "<td></td>",
                      "<td>Voti</td>",
                      "<td>Eletti</td>",
                      "</tr>")
    for (e in 1:nrow(results_candidates)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",results_candidates$firstname[e]," ",results_candidates$lastname[e],"</td>",
                        "<td>",results_candidates$shortname_it[e],"</td>",
                        "<td>",results_candidates$status_text_it[e],"</td>",
                        "<td>",ifelse(results_candidates$votes[e] > 9999,
                                      format(results_candidates$votes[e],big.mark = "'"),
                                      results_candidates$votes[e]),"</td>",
                        "<td>",ifelse(results_candidates$elected[e] == 1,"sì","no"),"</td>",
                        "</tr>")
    }
    tabelle <- gsub("Vereinzelte","Altri",tabelle)
  }
tabelle <- paste0(tabelle,"</tbody></table>")

return(tabelle)    
}    

