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
  storyboard <- c(storyboard,"Text_mehrere_Gewinner") 
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
  storyboard <- c(storyboard,"Text_mehrere_Verlierer") 
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
storyboard <- c(storyboard,"Explainer","Disclaimer") 
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
  storyboard <- c(storyboard,"Disclaimer")
  return(storyboard)
  
}  