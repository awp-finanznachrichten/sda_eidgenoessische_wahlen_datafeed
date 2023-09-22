create_table_SR_candidates <- function(results_candidates,
                                    language) {

if (language == "de") {  
tabelle <- paste0("<table><tbody><tr>",
                  "<td>Stimmen haben erhalten</td>",
                  "<td></td>",
                  "<td></td>",
                  "<td>Anzahl Stimmen</td>",
                  "</tr>")

for (e in 1:nrow(results_candidates)) {
  tabelle <- paste0(tabelle,
                    "<tr>",
                    "<td>",results_candidates$firstname[e]," ",results_candidates$lastname[e],"</td>",
                    "<td>",results_candidates$shortname_de[e],"</td>",
                    "<td>",results_candidates$status_text[e],"</td>",
                    "<td>",format(results_candidates$votes[e],big.mark = "'"),"</td>",
                    "</tr>")
}                    
}

  if (language == "fr") {  
    tabelle <- paste0("<table><tbody><tr>",
                      "<td>Stimmen haben erhalten</td>",
                      "<td></td>",
                      "<td></td>",
                      "<td>Anzahl Stimmen</td>",
                      "</tr>")
    
    for (e in 1:nrow(results_candidates)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",results_candidates$firstname[e]," ",results_candidates$lastname[e],"</td>",
                        "<td>",results_candidates$shortname_fr[e],"</td>",
                        "<td>",results_candidates$status_text_fr[e],"</td>",
                        "<td>",format(results_candidates$votes[e],big.mark = "'"),"</td>",
                        "</tr>")
    }                    
  }
  
  if (language == "it") {  
    tabelle <- paste0("<table><tbody><tr>",
                      "<td>hanno ottenuto voti</td>",
                      "<td></td>",
                      "<td></td>",
                      "<td>voti</td>",
                      "</tr>")
    for (e in 1:nrow(results_candidates)) {
      tabelle <- paste0(tabelle,
                        "<tr>",
                        "<td>",results_candidates$firstname[e]," ",results_candidates$lastname[e],"</td>",
                        "<td>",results_candidates$shortname_it[e],"</td>",
                        "<td>",results_candidates$status_text_it[e],"</td>",
                        "<td>",format(results_candidates$votes[e],big.mark = "'"),"</td>",
                        "</tr>")
    }                    
  }
tabelle <- paste0(tabelle,"</tbody></table>")

return(tabelle)    
}    

