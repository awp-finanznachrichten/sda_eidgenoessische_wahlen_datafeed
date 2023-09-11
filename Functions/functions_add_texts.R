add_participations <- function(nationalrat_gemeinden_dw_urlena,
                               results_NR_communities_voterturnout,
                               texts_spreadsheet_UrLena,
                               area = "canton") {
 
  if (area == "canton") {
    text_highest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_Participation_highest_in_canton")
    text_lowest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_Participation_lowest_in_canton")
    
    results <- results_NR_communities_voterturnout %>%
      filter(kanton_nummer == stand_cantons$kanton_nummer[c],
             is.na(wahlbeteiligung) == FALSE) %>%
      arrange(desc(wahlbeteiligung))
    
    if (nrow(results) > 10 ) {
 
    highest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[1])
    lowest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[nrow(results)])
  
    #Replace Variables
    if (sum(highest == included_communities) == 0) {

    ##DE##
    text_highest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_highest$Text_d)
    nationalrat_gemeinden_dw_urlena$Text[highest] <- 
      paste0(nationalrat_gemeinden_dw_urlena$Text[highest]," ",
             text_highest$Text_d
      )
    print(nationalrat_gemeinden_dw_urlena$Text[highest])
    
    ##FR##
    
    ##IT##
    

    included_communities <<- c(included_communities,highest)


      if (sum(lowest == included_communities) == 0) {  
        
      ##DE##
    text_lowest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_d) 
    nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
      paste0(nationalrat_gemeinden_dw_urlena$Text[lowest]," ",
             text_lowest$Text_d
      )
    print(nationalrat_gemeinden_dw_urlena$Text[lowest])
    
    included_communities <<- c(included_communities,lowest)
    
      }
    }
    }  
  } else {
    text_highest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_Participation_highest_in_CH")
    text_lowest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_Participation_lowest_in_CH")
    
    text_top10 <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_Participation_Top10_in_CH")
    text_bottom10 <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_Participation_Bottom10_in_CH")
    
    results <- results_NR_communities_voterturnout %>%
      filter(is.na(wahlbeteiligung) == FALSE) %>%
      arrange(desc(wahlbeteiligung)
              )
   
    highest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[1])
    lowest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[nrow(results)])

    #Replace Variables
    ##DE##
      text_highest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_highest$Text_d)
      nationalrat_gemeinden_dw_urlena$Text[highest] <- 
        paste0(nationalrat_gemeinden_dw_urlena$Text[highest]," ",
               text_highest$Text_d
        )
      print(nationalrat_gemeinden_dw_urlena$Text[highest])
      
    ##FR##
      
    ##IT##
      
    included_communities <<- c(included_communities,highest)  
    
    ##DE##
      text_lowest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_d) 
      nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
        paste0(nationalrat_gemeinden_dw_urlena$Text[lowest]," ",
               text_lowest$Text_d
        )
      print(nationalrat_gemeinden_dw_urlena$Text[lowest])
    ##FR##
      
    ##IT##     
      
included_communities <<- c(included_communities,lowest)


      for (i in 2:10) {
      highest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[i])
      lowest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[nrow(results)-(i-1)]) 

      ##DE##
        text_top10$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_top10$Text_d)
        nationalrat_gemeinden_dw_urlena$Text[highest] <- 
          paste0(nationalrat_gemeinden_dw_urlena$Text[highest]," ",
                 text_top10$Text_d
          )
        print(nationalrat_gemeinden_dw_urlena$Text[highest])
        
        ##FR##
        
        ##IT##
        
        included_communities <<- c(included_communities,highest)
        
        ##DE##
        text_bottom10$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_bottom10$Text_d) 
        nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
          paste0(nationalrat_gemeinden_dw_urlena$Text[lowest]," ",
                 text_bottom10$Text_d
          )
        print(nationalrat_gemeinden_dw_urlena$Text[lowest])
        
        ##FR##
        
        ##IT##
        
        included_communities <<- c(included_communities,lowest)
      }  
}

return(nationalrat_gemeinden_dw_urlena)  
}  

add_parties <- function(nationalrat_gemeinden_dw_urlena,
                        results_NR_communities,
                        texts_spreadsheet_UrLena,
                        area = "canton") {

parties_ids <- c(2,1,56,70,75,3)
parties_name_de <- c("SVP","SP","FDP","Mitte","Grüne","GLP")

for (i in 1:length(parties_ids)) { 
  if (area == "canton") {
    text_highest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_highest_in_canton")
    text_lowest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_lowest_in_canton")

    results <- results_NR_communities %>%
      filter(kanton_nummer == stand_cantons$kanton_nummer[c],
             is.na(partei_staerke) == FALSE,
             id == parties_ids[i]) %>%
      arrange(desc(partei_staerke))
    
    if (nrow(results) > 10 ) {
      highest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[1])
      lowest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[nrow(results)])
      
      #Replace Variables
      if (sum(highest == included_communities) == 0) {
        
        ##DE##
        text_highest$Text_d <- gsub("#Party_highest_in_canton_de",parties_name_de[i],text_highest$Text_d)
        text_highest$Text_d <- gsub("Die Grüne hat","Die Grünen haben",text_highest$Text_d)
        nationalrat_gemeinden_dw_urlena$Text[highest] <- 
          paste0(nationalrat_gemeinden_dw_urlena$Text[highest]," ",
                 text_highest$Text_d
          )
        print(nationalrat_gemeinden_dw_urlena$Text[highest])
        
        ##FR##
        
        ##IT##
        
        
        included_communities <<- c(included_communities,highest)
      }
        
      if (sum(lowest == included_communities) == 0) {
        ##DE##
        text_lowest$Text_d <- gsub("#Party_lowest_in_canton_de",parties_name_de[i],text_lowest$Text_d)
        text_lowest$Text_d <- gsub("war die Grüne","waren die Grünen",text_lowest$Text_d)
        nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
          paste0(nationalrat_gemeinden_dw_urlena$Text[lowest]," ",
                 text_lowest$Text_d
          )
        print(nationalrat_gemeinden_dw_urlena$Text[lowest])
        
        ##FR##
        
        ##IT##
        
        included_communities <<- c(included_communities,lowest)
        
      }
    }  
  } else {
    text_highest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_highest_in_CH")
    text_lowest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_lowest_in_CH")
    
    results <- results_NR_communities %>%
      filter(is.na(partei_staerke) == FALSE,
             id == parties_ids[i]) %>%
      arrange(desc(partei_staerke))
    
    highest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[1])
    lowest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[nrow(results)])
    
    #Replace Variables
    
    ##DE##
      text_highest$Text_d <- gsub("#Party_highest_de",parties_name_de[i],text_highest$Text_d)
      nationalrat_gemeinden_dw_urlena$Text[highest] <- 
        paste0(nationalrat_gemeinden_dw_urlena$Text[highest]," ",
               text_highest$Text_d
        )
      print(nationalrat_gemeinden_dw_urlena$Text[highest])
      included_communities <<- c(included_communities,highest)
      
      
      text_lowest$Text_d <- gsub("#Party_lowest_de",parties_name_de[i],text_lowest$Text_d) 
      nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
        paste0(nationalrat_gemeinden_dw_urlena$Text[lowest]," ",
               text_lowest$Text_d
        )
      print(nationalrat_gemeinden_dw_urlena$Text[lowest])
      
      ##FR##
      
      ##IT##
      
      included_communities <<- c(included_communities,lowest)
  }  
}  
return(nationalrat_gemeinden_dw_urlena)  
}  

