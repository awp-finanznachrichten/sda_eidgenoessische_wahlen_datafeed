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
      paste0(nationalrat_gemeinden_dw_urlena$Text[highest],"<br><br>",
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
      paste0(nationalrat_gemeinden_dw_urlena$Text[lowest],"<br><br>",
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
        paste0(nationalrat_gemeinden_dw_urlena$Text[highest],"<br><br>",
               text_highest$Text_d
        )
      print(nationalrat_gemeinden_dw_urlena$Text[highest])
      
    ##FR##
      
    ##IT##
      
    included_communities <<- c(included_communities,highest)  
    
    ##DE##
      text_lowest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_d) 
      nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
        paste0(nationalrat_gemeinden_dw_urlena$Text[lowest],"<br><br>",
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
          paste0(nationalrat_gemeinden_dw_urlena$Text[highest],"<br><br>",
                 text_top10$Text_d
          )
        print(nationalrat_gemeinden_dw_urlena$Text[highest])
        
        ##FR##
        
        ##IT##
        
        included_communities <<- c(included_communities,highest)
        
        ##DE##
        text_bottom10$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_bottom10$Text_d) 
        nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
          paste0(nationalrat_gemeinden_dw_urlena$Text[lowest],"<br><br>",
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

results_NR_communities_nosmall <- results_NR_communities %>%
    group_by(gemeinde_nummer) %>%
    mutate(small_community = ifelse(max(stimmen_partei,na.omit = TRUE) > 200,FALSE,TRUE)) %>%
  filter(small_community == FALSE)
  
parties_ids <- c(2,1,56,70,75,3)
parties_name_de <- c("SVP","SP","FDP","Mitte","Grüne","GLP")

for (i in 1:length(parties_ids)) { 
  if (area == "canton") {
    text_highest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_highest_in_canton")
    text_lowest <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_lowest_in_canton")

    results <- results_NR_communities_nosmall %>%
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
        text_highest$Text_d <- gsub("#Party_highest_in_canton_d",parties_name_de[i],text_highest$Text_d)
        text_highest$Text_d <- gsub("Die Grüne hat","Die Grünen haben",text_highest$Text_d)
        nationalrat_gemeinden_dw_urlena$Text[highest] <- 
          paste0(nationalrat_gemeinden_dw_urlena$Text[highest],"<br><br>",
                 text_highest$Text_d
          )
        print(nationalrat_gemeinden_dw_urlena$Text[highest])
        
        ##FR##
        
        ##IT##
        
        
        included_communities <<- c(included_communities,highest)
      }
        
      if (sum(lowest == included_communities) == 0) {
        ##DE##
        text_lowest$Text_d <- gsub("#Party_lowest_in_canton_d",parties_name_de[i],text_lowest$Text_d)
        text_lowest$Text_d <- gsub("war die Grüne","waren die Grünen",text_lowest$Text_d)
        nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
          paste0(nationalrat_gemeinden_dw_urlena$Text[lowest],"<br><br>",
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
    
    results <- results_NR_communities_nosmall %>%
      filter(is.na(partei_staerke) == FALSE,
             id == parties_ids[i]) %>%
      arrange(desc(partei_staerke))
    
    highest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[1])
    lowest <- which(nationalrat_gemeinden_dw_urlena$ID == results$gemeinde_nummer[nrow(results)])
    
    #Replace Variables
    
    ##DE##
    if (sum(highest == included_communities) == 0) {
      text_highest$Text_d <- gsub("#Party_highest_d",parties_name_de[i],text_highest$Text_d)
      nationalrat_gemeinden_dw_urlena$Text[highest] <- 
        paste0(nationalrat_gemeinden_dw_urlena$Text[highest],"<br><br>",
               text_highest$Text_d
        )
      print(nationalrat_gemeinden_dw_urlena$Text[highest])
      
      ##FR##
      
      ##IT##
        
      included_communities <<- c(included_communities,highest)
    }
      
    if (sum(lowest == included_communities) == 0) {
      text_lowest$Text_d <- gsub("#Party_lowest_d",parties_name_de[i],text_lowest$Text_d) 
      nationalrat_gemeinden_dw_urlena$Text[lowest] <- 
        paste0(nationalrat_gemeinden_dw_urlena$Text[lowest],"<br><br>",
               text_lowest$Text_d
        )
      print(nationalrat_gemeinden_dw_urlena$Text[lowest])

      ##FR##
      
      ##IT##
      
      included_communities <<- c(included_communities,lowest)
    }
  }  
}  
return(nationalrat_gemeinden_dw_urlena)  
}  

add_elected_candidates <- function(elected_candidates_overall,
                                   nationalrat_gemeinden_dw_urlena,
                                   texts_spreadsheet_UrLena) {

communities_ids <- unique(elected_candidates_overall$place_id)  
for (community_id in communities_ids) {

  
community <- nationalrat_gemeinden_dw_urlena %>%
  filter(ID == community_id,
         Staerkste_Partei != "no_data")
selection <- which(nationalrat_gemeinden_dw_urlena$ID == community_id)

if (nrow(community) == 1) {
elected_candidates <- elected_candidates_overall %>%
  filter(place_id == community_id)
str_sub("Zürich ZH",end=-4)
if (nrow(elected_candidates) == 1) {
if (elected_candidates$gender == "f") {
  text_one_f <- texts_spreadsheet_UrLena %>%
    filter(Text_ID == "Add_NR_im_Wohnort_one_f")
  text_one_f$Text_d <- gsub("#Gemeinde_d",str_sub(community$Gemeinde,end=-4),text_one_f$Text_d)
  text_one_f$Text_d <- gsub("#Names_NR_pro_Ort",
                            paste0("<b>",elected_candidates$firstname," ",elected_candidates$lastname," (",elected_candidates$shortname_de,")</b>")
                            ,text_one_f$Text_d) 
  nationalrat_gemeinden_dw_urlena$Text[selection] <- 
    paste0(nationalrat_gemeinden_dw_urlena$Text[selection],"<br><br>",
           text_one_f$Text_d
    )
  print(nationalrat_gemeinden_dw_urlena$Text[selection])  
  
} else if (elected_candidates$gender == "m") {
  text_one_m <- texts_spreadsheet_UrLena %>%
    filter(Text_ID == "Add_NR_im_Wohnort_one_m")
  text_one_m$Text_d <- gsub("#Gemeinde_d",str_sub(community$Gemeinde,end=-4),text_one_m$Text_d)
  text_one_m$Text_d <- gsub("#Names_NR_pro_Ort",
                            paste0("<b>",elected_candidates$firstname," ",elected_candidates$lastname," (",elected_candidates$shortname_de,")</b>")
                            ,text_one_m$Text_d) 
  nationalrat_gemeinden_dw_urlena$Text[selection] <- 
    paste0(nationalrat_gemeinden_dw_urlena$Text[selection],"<br><br>",
           text_one_m$Text_d
    )
  print(nationalrat_gemeinden_dw_urlena$Text[selection]) 
}  
  
} else if (nrow(elected_candidates) < 5) {
  text_few <- texts_spreadsheet_UrLena %>%
    filter(Text_ID == "Add_NR_im_Wohnort_few")
  text_few$Text_d <- gsub("#Gemeinde_d",str_sub(community$Gemeinde,end=-4),text_few$Text_d)
  text_few$Text_d <- gsub("#Anzahl_NR_pro_Ort",nrow(elected_candidates),text_few$Text_d)
  text_elected <- ""
  for (l in 1:nrow(elected_candidates)) {
    text_elected <- paste0(text_elected,elected_candidates$firstname[l]," ",elected_candidates$lastname[l],
                             " (",elected_candidates$shortname_de[l],"), ")  
  }
  text_elected <- substr(text_elected,1,nchar(text_elected)-2)
  text_elected <- stri_replace_last(text_elected,fixed=","," und")
  
  text_few$Text_d <- gsub("#Names_NR_pro_Ort",
                            text_elected
                            ,text_few$Text_d) 
  nationalrat_gemeinden_dw_urlena$Text[selection] <- 
    paste0(nationalrat_gemeinden_dw_urlena$Text[selection],"<br><br>",
           text_few$Text_d
    )
  print(nationalrat_gemeinden_dw_urlena$Text[selection]) 

} else if (nrow(elected_candidates) >= 5) {
  text_many <- texts_spreadsheet_UrLena %>%
    filter(Text_ID == "Add_NR_im_Wohnort_many")
  text_many$Text_d <- gsub("#Gemeinde_d",str_sub(community$Gemeinde,end=-4),text_many$Text_d)
  text_many$Text_d <- gsub("#Anzahl_NR_pro_Ort",nrow(elected_candidates),text_many$Text_d)
  nationalrat_gemeinden_dw_urlena$Text[selection] <- 
    paste0(nationalrat_gemeinden_dw_urlena$Text[selection],"<br><br>",
           text_many$Text_d
    )
  print(nationalrat_gemeinden_dw_urlena$Text[selection]) 
}
}  
}  
return(nationalrat_gemeinden_dw_urlena)
}  

