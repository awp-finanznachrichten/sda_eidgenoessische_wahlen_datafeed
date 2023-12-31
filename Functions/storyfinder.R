###Storyfinder Nationalrat Parties
get_story_NR_parties <- function(results_parties,
                                 seats_available,
                                 area_ID) {
storyboard <- c("Catchword")

#Headline
if (seats_available > 1) {
storyboard <- c(storyboard,"Headline")  
} else {
storyboard <- c(storyboard,"Headline_ein_Sitz")  
}
#Text
if (sum(results_parties$seats_change != 0) == 0) {
storyboard <- c(storyboard,"Text_Special_keineGewinnerVerlierer","")  
} else {
#Winners
if (sum(results_parties$seats_change > 0) > 1) {
  if (sum(results_parties$seats_change > 1) == 0) {
  storyboard <- c(storyboard,"Text_mehrere_Gewinner_ein_Sitz")
  } else {
  storyboard <- c(storyboard,"Text_mehrere_Gewinner")
  }  
} else if (sum(results_parties$seats_change > 0) == 1) {
  if (sum(results_parties$seats_change > 1) == 0) {
    storyboard <- c(storyboard,"Text_ein_Gewinner_ein_Sitz") 
  } else {
    storyboard <- c(storyboard,"Text_ein_Gewinner_mehrere_Sitze") 
  }  
} else {
  storyboard <- c(storyboard,"Text_Special_keinGewinner_ein_Verlierer","")  
}  
#Losers
if (sum(results_parties$seats_change < 0) > 1) {
  if (sum(results_parties$seats_change < -1) == 0) {
    storyboard <- c(storyboard,"Text_mehrere_Verlierer_ein_Sitz")
  } else {
    storyboard <- c(storyboard,"Text_mehrere_Verlierer")
  }  
} else if (sum(results_parties$seats_change < 0) == 1) {
  if (sum(results_parties$seats_change < -1) == 0) {
  storyboard <- c(storyboard,"Text_ein_Verlierer_ein_Sitz") 
  } else {
  storyboard <- c(storyboard,"Text_ein_Verlierer_mehrere_Sitze") 
  }  
} else {
  storyboard <- c(storyboard,"Text_Special_keinVerlierer_ein_Gewinner","")  
}    
}  
storyboard <- c(storyboard,"Intro_Tabelle","Outro_Tabelle")
#Outro Tabelle Special
if (area_ID == "ZH") {
storyboard <- c(storyboard,"Outro_Tabelle_Special_ZH")
} else if (area_ID == "BS") {
storyboard <- c(storyboard,"Outro_Tabelle_Special_BS")  
} else {
storyboard <- c(storyboard,"")   
}  

if (counted_cantons$source_update[c] == "BFS") {
storyboard <- c(storyboard,"Explainer","Disclaimer") 
} else {
storyboard <- c(storyboard,"Explainer","Disclaimer_Canton")   
}  
return(storyboard)
}  


###Storyfinder Nationalrat Parties Zwischenstand
get_story_NR_intermediate <- function(results_ch) {
  storyboard <- c("Catchword","Headline")
  #Text
  if (sum(results_ch$seats_change != 0) == 0) {
    storyboard <- c(storyboard,"Text_Special_keineGewinnerVerlierer","")  
  } else {
    #Winners
    if (sum(results_ch$seats_change > 0) > 1) {
      storyboard <- c(storyboard,"Text_mehrere_Gewinner") 
    } else if (sum(results_ch$seats_change > 0) == 1) {
      if (sum(results_ch$seats_change > 1) == 0) {
        storyboard <- c(storyboard,"Text_ein_Gewinner_ein_Sitz") 
      } else {
        storyboard <- c(storyboard,"Text_ein_Gewinner_mehrere_Sitze") 
      }  
    }  
    #Losers
    if (sum(results_ch$seats_change < 0) > 1) {
      storyboard <- c(storyboard,"Text_mehrere_Verlierer") 
    } else if (sum(results_ch$seats_change < 0) == 1) {
      if (sum(results_ch$seats_change < -1) == 0) {
        storyboard <- c(storyboard,"Text_ein_Verlierer_ein_Sitz") 
      } else {
        storyboard <- c(storyboard,"Text_ein_Verlierer_mehrere_Sitze") 
      }  
    }  
    
  storyboard <- c(storyboard,"Intro_Tabelle","Outro_Tabelle","Outro_Tabelle_add","Explainer","Disclaimer")
  return(storyboard)
  }  
}  




###Storyfinder Nationalrat candidates

get_story_NR_candidates <- function(seats_available,
                                    elected_candidates,
                                    voted_out_candidates) {
  storyboard <- c("Catchword")
  count_men <- sum(grepl("M|m",elected_candidates$gender))
  count_women <- sum(grepl("F|f",elected_candidates$gender))

  #Headline and Intro
  if (seats_available > 1) {
    storyboard <- c(storyboard,"Headline","Intro_Tabelle","Outro_Tabelle")
  } else {
    if (count_women == 0) {
    storyboard <- c(storyboard,"Headline_ein_Sitz_Mann","Intro_Tabelle_ein_Sitz","")  
    } else {
    storyboard <- c(storyboard,"Headline_ein_Sitz_Frau","Intro_Tabelle_ein_Sitz","")   
    }  
  }
  
  #Abgewaehlt
  if (nrow(voted_out_candidates) == 1) {
  storyboard <- c(storyboard,"Abgewaehlte_eine_Person")   
  } else if (nrow(voted_out_candidates) > 1) {
  storyboard <- c(storyboard,"Abgewaehlte_mehrere")  
  } else {
  storyboard <- c(storyboard,"")
  }  
  
  if (counted_cantons$source_update[c] == "BFS") {
    storyboard <- c(storyboard,"Disclaimer") 
  } else {
    storyboard <- c(storyboard,"Disclaimer_Canton")  
  }  
  return(storyboard)
}  



###Storyfinder Ständerat candidates

get_story_SR_candidates <- function(seats_available,
                                    elected_candidates,
                                    voted_out_candidates) {
  
  count_men <- sum(grepl("M|m",elected_candidates$gender))
  count_women <- sum(grepl("F|f",elected_candidates$gender))
  count_men_voted_out <- sum(grepl("M|m",voted_out_candidates$gender))
  count_women_voted_out <- sum(grepl("F|f",voted_out_candidates$gender))

  if (nrow(elected_candidates) == 2) {
  if (nrow(voted_out_candidates) == 2) {
    if (count_women_voted_out == 2) {
      storyboard <- c("Catchword","Headline","Lead_Abgewählt_zwei_FF")
    } else {
    storyboard <- c("Catchword","Headline","Lead_Abgewählt_zwei")    
    }  
  } else if (nrow(voted_out_candidates) == 1) {
    if (count_women_voted_out == 1) {
      storyboard <- c("Catchword","Headline","Lead_Abgewählt_F") 
    } else {
      storyboard <- c("Catchword","Headline","Lead_Abgewählt") 
    }  
  } else {
    if (count_women == 2) {
      storyboard <- c("Catchword","Headline","Lead_TwoElected") 
    } else {
      storyboard <- c("Catchword","Headline","Lead_TwoElected") 
    }
  }  
    
  } else if (nrow(elected_candidates) == 1) {
  if (seats_available == 2) {
    if (count_women == 1) {
      storyboard <- c("Catchword","Headline","Lead_OneElected")
    } else {
      storyboard <- c("Catchword","Headline","Lead_OneElected")  
    }  
  } else {
    if (count_women == 1) {
  storyboard <- c("Catchword","Headline","Lead_OneElected_Halbkanton")
   } else {
  storyboard <- c("Catchword","Headline","Lead_OneElected_Halbkanton")
   }
  }  
  } else {
  storyboard <- c("Catchword","Headline","Lead_NobodyElected")
  }  
  
  if (counted_cantons_SR$source_update[c] == "BFS") {
    storyboard <- c(storyboard,"","Disclaimer") 
  } else {
    if (counted_cantons_SR$absolute_majority[c] > 0) {
    storyboard <- c(storyboard,"Tabelle_Outro","Disclaimer_Canton")
    } else {
    storyboard <- c(storyboard,"","Disclaimer_Canton")
    }  
  }  
  return(storyboard)
}  

