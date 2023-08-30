get_voted_out_candidates <- function(texts,
                              voted_out_candidates,
                              language = "de") {

#Text voted out candidates
text_voted_out <- ""

if (language == "de") {
if (grepl("#Abgewaehlte_NR",texts[5]) == TRUE) {
  for (l in 1:nrow(voted_out_candidates)) {
    text_voted_out <- paste0(text_voted_out,voted_out_candidates$firstname[l]," ",voted_out_candidates$lastname[l],
                             " (",voted_out_candidates$shortname_de[l],"), ")  
  }
  text_voted_out <- substr(text_voted_out,1,nchar(text_voted_out)-2)
  text_voted_out <- stri_replace_last(text_voted_out,fixed=","," und")
  texts <- gsub("#Abgewaehlte_NR_d",text_voted_out,texts)
} else if (grepl("#Abgewaehlt_NR",texts[5]) == TRUE) {
text_voted_out <- paste0(text_voted_out,voted_out_candidates$firstname," ",voted_out_candidates$lastname,
                           " (",voted_out_candidates$shortname_de,")")  
texts <- gsub("#Abgewaehlt_NR_d",text_voted_out,texts)
}  
}

if (language == "fr") {
  if (grepl("#Abgewaehlte_NR",texts[5]) == TRUE) {
    for (l in 1:nrow(voted_out_candidates)) {
      text_voted_out <- paste0(text_voted_out,voted_out_candidates$firstname[l]," ",voted_out_candidates$lastname[l],
                               " (",voted_out_candidates$shortname_fr[l],"), ")  
    }
    text_voted_out <- substr(text_voted_out,1,nchar(text_voted_out)-2)
    text_voted_out <- stri_replace_last(text_voted_out,fixed=","," et")
    texts <- gsub("#Abgewaehlte_NR_f",text_voted_out,texts)
  } else if (grepl("#Abgewaehlt_NR",texts[5]) == TRUE) {
    text_voted_out <- paste0(text_voted_out,voted_out_candidates$firstname," ",voted_out_candidates$lastname,
                             " (",voted_out_candidates$shortname_fr,")")  
    texts <- gsub("#Abgewaehlt_NR_f",text_voted_out,texts)
  }  
}

if (language == "it") {
  if (grepl("#Abgewaehlte_NR",texts[5]) == TRUE) {
    for (l in 1:nrow(voted_out_candidates)) {
      text_voted_out <- paste0(text_voted_out,voted_out_candidates$firstname[l]," ",voted_out_candidates$lastname[l],
                               " (",voted_out_candidates$shortname_it[l],"), ")  
    }
    text_voted_out <- substr(text_voted_out,1,nchar(text_voted_out)-2)
    text_voted_out <- stri_replace_last(text_voted_out,fixed=","," e")
    texts <- gsub("#Abgewaehlte_NR_i",text_voted_out,texts)
  } else if (grepl("#Abgewaehlt_NR",texts[5]) == TRUE) {
    text_voted_out <- paste0(text_voted_out,voted_out_candidates$firstname," ",voted_out_candidates$lastname,
                             " (",voted_out_candidates$shortname_it,")")  
    texts <- gsub("#Abgewaehlt_NR_i",text_voted_out,texts)
  }  
}

return(texts)  
}  