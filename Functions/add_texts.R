add_participations <- function(nationalrat_gemeinden_dw,
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
 
    highest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[1])
    lowest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[nrow(results)])
  
    #Replace Variables
    if (sum(highest == included_communities) == 0) {

    ##DE##
    text_highest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_highest$Text_d)
    nationalrat_gemeinden_dw$Text_de[highest] <- 
      paste0(nationalrat_gemeinden_dw$Text_de[highest],"<br><br>",
             text_highest$Text_d)
    print(nationalrat_gemeinden_dw$Text_de[highest])
    
    ##FR##
    text_highest$Text_f <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_highest$Text_f)
    nationalrat_gemeinden_dw$Text_fr[highest] <- 
      paste0(nationalrat_gemeinden_dw$Text_fr[highest],"<br><br>",
             text_highest$Text_f)
    
    ##IT##
    text_highest$Text_i <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_highest$Text_i)
    nationalrat_gemeinden_dw$Text_it[highest] <- 
      paste0(nationalrat_gemeinden_dw$Text_it[highest],"<br><br>",
             text_highest$Text_i)
    

    included_communities <<- c(included_communities,highest)


      if (sum(lowest == included_communities) == 0) {  
        
      ##DE##
    text_lowest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_d) 
    nationalrat_gemeinden_dw$Text_de[lowest] <- 
      paste0(nationalrat_gemeinden_dw$Text_de[lowest],"<br><br>",
             text_lowest$Text_d)
    print(nationalrat_gemeinden_dw$Text_de[lowest])
    
    ##FR##
    text_lowest$Text_f <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_f) 
    nationalrat_gemeinden_dw$Text_fr[lowest] <- 
      paste0(nationalrat_gemeinden_dw$Text_fr[lowest],"<br><br>",
             text_lowest$Text_f)
    
    ##IT##
    text_lowest$Text_i <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_i) 
    nationalrat_gemeinden_dw$Text_it[lowest] <- 
      paste0(nationalrat_gemeinden_dw$Text_it[lowest],"<br><br>",
             text_lowest$Text_i)
    
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

    highest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[1])
    lowest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[nrow(results)])

    #Replace Variables
    ##DE##
      text_highest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_highest$Text_d)
      nationalrat_gemeinden_dw$Text_de[highest] <- 
        paste0(nationalrat_gemeinden_dw$Text_de[highest],"<br><br>",
               text_highest$Text_d)
      print(nationalrat_gemeinden_dw$Text_de[highest])
      
    ##FR##
      text_highest$Text_f <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_highest$Text_f)
      nationalrat_gemeinden_dw$Text_fr[highest] <- 
        paste0(nationalrat_gemeinden_dw$Text_fr[highest],"<br><br>",
               text_highest$Text_f)
      
    ##IT##
      text_highest$Text_i <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_highest$Text_i)
      nationalrat_gemeinden_dw$Text_it[highest] <- 
        paste0(nationalrat_gemeinden_dw$Text_it[highest],"<br><br>",
               text_highest$Text_i)
      
    included_communities <<- c(included_communities,highest)  
    
    ##DE##
      text_lowest$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_d) 
      nationalrat_gemeinden_dw$Text_de[lowest] <- 
        paste0(nationalrat_gemeinden_dw$Text_de[lowest],"<br><br>",
               text_lowest$Text_d)
      print(nationalrat_gemeinden_dw$Text_de[lowest])
    ##FR##
      text_lowest$Text_f <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_f) 
      nationalrat_gemeinden_dw$Text_fr[lowest] <- 
        paste0(nationalrat_gemeinden_dw$Text_fr[lowest],"<br><br>",
               text_lowest$Text_f)
      
    ##IT##    
      text_lowest$Text_i <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_lowest$Text_i) 
      nationalrat_gemeinden_dw$Text_it[lowest] <- 
        paste0(nationalrat_gemeinden_dw$Text_it[lowest],"<br><br>",
               text_lowest$Text_i)
      
included_communities <<- c(included_communities,lowest)


      for (i in 2:10) {
      highest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[i])
      lowest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[nrow(results)-(i-1)]) 

      ##DE##
        text_top10$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_top10$Text_d)
        nationalrat_gemeinden_dw$Text_de[highest] <- 
          paste0(nationalrat_gemeinden_dw$Text_de[highest],"<br><br>",
                 text_top10$Text_d)
        print(nationalrat_gemeinden_dw$Text_de[highest])
        
        ##FR##
        text_top10$Text_f <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_top10$Text_f)
        nationalrat_gemeinden_dw$Text_fr[highest] <- 
          paste0(nationalrat_gemeinden_dw$Text_fr[highest],"<br><br>",
                 text_top10$Text_f)
        
        ##IT##
        text_top10$Text_i <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[1],1)),text_top10$Text_i)
        nationalrat_gemeinden_dw$Text_it[highest] <- 
          paste0(nationalrat_gemeinden_dw$Text_it[highest],"<br><br>",
                 text_top10$Text_i)
        
        included_communities <<- c(included_communities,highest)
        
        ##DE##
        text_bottom10$Text_d <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_bottom10$Text_d) 
        nationalrat_gemeinden_dw$Text_de[lowest] <- 
          paste0(nationalrat_gemeinden_dw$Text_de[lowest],"<br><br>",
                 text_bottom10$Text_d)
        print(nationalrat_gemeinden_dw$Text_de[lowest])
        
        ##FR##
        text_bottom10$Text_f <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_bottom10$Text_f) 
        nationalrat_gemeinden_dw$Text_fr[lowest] <- 
          paste0(nationalrat_gemeinden_dw$Text_fr[lowest],"<br><br>",
                 text_bottom10$Text_f)
        
        ##IT##
        text_bottom10$Text_i <- gsub("#Gemeinde_participation",gsub("[.]",",",round2(results$wahlbeteiligung[nrow(results)],1)),text_bottom10$Text_i) 
        nationalrat_gemeinden_dw$Text_it[lowest] <- 
          paste0(nationalrat_gemeinden_dw$Text_it[lowest],"<br><br>",
                 text_bottom10$Text_i)
        
        included_communities <<- c(included_communities,lowest)
      }  
}

return(nationalrat_gemeinden_dw)  
}  

add_parties <- function(nationalrat_gemeinden_dw,
                        results_NR_communities,
                        texts_spreadsheet_UrLena,
                        area = "canton") {

results_NR_communities_nosmall <- results_NR_communities %>%
    group_by(gemeinde_nummer) %>%
    mutate(small_community = ifelse(max(stimmen_partei,na.omit = TRUE) > 200,FALSE,TRUE)) %>%
  filter(small_community == FALSE)
  
parties_ids <- c(2,1,56,70,75,3)
parties_name_de <- c("SVP","SP","FDP","Mitte","Grüne","GLP")
parties_name_fr <- c("UDC","PS","PLR","Centre","Vert-e-s","PVL")
parties_name_it <- c("UDC","PS","PLR","Centro","Verdi","PVL")

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
      highest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[1])
      lowest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[nrow(results)])
      
      #Replace Variables
      if (sum(highest == included_communities) == 0) {
        
        ##DE##
        text_highest$Text_d <- gsub("#Party_highest_in_canton_d",parties_name_de[i],text_highest$Text_d)
        nationalrat_gemeinden_dw$Text_de[highest] <- 
          paste0(nationalrat_gemeinden_dw$Text_de[highest],"<br><br>",
                 text_highest$Text_d)
        print(nationalrat_gemeinden_dw$Text_de[highest])
        
        ##FR##
        text_highest$Text_f <- gsub("#Party_highest_in_canton_f",parties_name_fr[i],text_highest$Text_f)
        nationalrat_gemeinden_dw$Text_fr[highest] <- 
          paste0(nationalrat_gemeinden_dw$Text_fr[highest],"<br><br>",
                 text_highest$Text_f)
        
        ##IT##
        text_highest$Text_i <- gsub("#Party_highest_in_canton_i",parties_name_it[i],text_highest$Text_i)
        nationalrat_gemeinden_dw$Text_it[highest] <- 
          paste0(nationalrat_gemeinden_dw$Text_it[highest],"<br><br>",
                 text_highest$Text_i)
        
        
        included_communities <<- c(included_communities,highest)
      }
        
      if (sum(lowest == included_communities) == 0) {
        ##DE##
        text_lowest$Text_d <- gsub("#Party_lowest_in_canton_d",parties_name_de[i],text_lowest$Text_d)
        nationalrat_gemeinden_dw$Text_de[lowest] <- 
          paste0(nationalrat_gemeinden_dw$Text_de[lowest],"<br><br>",
                 text_lowest$Text_d)
        print(nationalrat_gemeinden_dw$Text_de[lowest])
        
        ##FR##
        text_lowest$Text_f <- gsub("#Party_lowest_in_canton_f",parties_name_fr[i],text_lowest$Text_f)
        nationalrat_gemeinden_dw$Text_fr[lowest] <- 
          paste0(nationalrat_gemeinden_dw$Text_fr[lowest],"<br><br>",
                 text_lowest$Text_f)
        
        ##IT##
        text_lowest$Text_i <- gsub("#Party_lowest_in_canton_i",parties_name_it[i],text_lowest$Text_i)
        nationalrat_gemeinden_dw$Text_it[lowest] <- 
          paste0(nationalrat_gemeinden_dw$Text_it[lowest],"<br><br>",
                 text_lowest$Text_i)
        
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
    
    highest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[1])
    lowest <- which(nationalrat_gemeinden_dw$ID == results$gemeinde_nummer[nrow(results)])
    
    if (sum(highest == included_communities) == 0) {
      ##DE##
      text_highest$Text_d <- gsub("#Party_highest_d",parties_name_de[i],text_highest$Text_d)
      nationalrat_gemeinden_dw$Text_de[highest] <- 
        paste0(nationalrat_gemeinden_dw$Text_de[highest],"<br><br>",
               text_highest$Text_d)
      print(nationalrat_gemeinden_dw$Text_de[highest])
      
      ##FR##
      text_highest$Text_f <- gsub("#Party_highest_f",parties_name_fr[i],text_highest$Text_f)
      nationalrat_gemeinden_dw$Text_fr[highest] <- 
        paste0(nationalrat_gemeinden_dw$Text_fr[highest],"<br><br>",
               text_highest$Text_f)
      
      ##IT##
      text_highest$Text_i <- gsub("#Party_highest_i",parties_name_it[i],text_highest$Text_i)
      nationalrat_gemeinden_dw$Text_it[highest] <- 
        paste0(nationalrat_gemeinden_dw$Text_it[highest],"<br><br>",
               text_highest$Text_i)
        
      included_communities <<- c(included_communities,highest)
    }
      
    if (sum(lowest == included_communities) == 0) {
      text_lowest$Text_d <- gsub("#Party_lowest_d",parties_name_de[i],text_lowest$Text_d) 
      nationalrat_gemeinden_dw$Text_de[lowest] <- 
        paste0(nationalrat_gemeinden_dw$Text_de[lowest],"<br><br>",
               text_lowest$Text_d)
      print(nationalrat_gemeinden_dw$Text_de[lowest])

      ##FR##
      text_lowest$Text_f <- gsub("#Party_lowest_f",parties_name_fr[i],text_lowest$Text_f) 
      nationalrat_gemeinden_dw$Text_fr[lowest] <- 
        paste0(nationalrat_gemeinden_dw$Text_fr[lowest],"<br><br>",
               text_lowest$Text_f)
      
      ##IT##
      text_lowest$Text_i <- gsub("#Party_lowest_i",parties_name_it[i],text_lowest$Text_i) 
      nationalrat_gemeinden_dw$Text_it[lowest] <- 
        paste0(nationalrat_gemeinden_dw$Text_it[lowest],"<br><br>",
               text_lowest$Text_i
        )
      
      included_communities <<- c(included_communities,lowest)
    }
  }  
}  
return(nationalrat_gemeinden_dw)  
}  

add_elected_candidates <- function(elected_candidates_overall,
                                   nationalrat_gemeinden_dw,
                                   texts_spreadsheet_UrLena) {

communities_ids <- unique(elected_candidates_overall$place_id)  
for (community_id in communities_ids) {

  
community <- nationalrat_gemeinden_dw %>%
  filter(ID == community_id,
         Staerkste_Partei != "no_data")
selection <- which(nationalrat_gemeinden_dw$ID == community_id)

if (nrow(community) == 1) {
elected_candidates <- elected_candidates_overall %>%
  filter(place_id == community_id)

if (nrow(elected_candidates) == 1) {
if (elected_candidates$gender == "f") {
  
  if(grepl("AI|AR|GL|NW|OW|UR",elected_candidates$area_id) == TRUE) {
   text_one_f <- texts_spreadsheet_UrLena %>%
     filter(Text_ID == "Add_small_canton_NR_im_Wohnort_f")
 } else {
   text_one_f <- texts_spreadsheet_UrLena %>%
     filter(Text_ID == "Add_NR_im_Wohnort_one_f")
 } 
  ##DE##
  text_one_f$Text_d <- gsub("#Names_NR_pro_Ort",
                            paste0("<b>",elected_candidates$firstname," ",elected_candidates$lastname," (",elected_candidates$shortname_de,")</b>")
                            ,text_one_f$Text_d) 
  nationalrat_gemeinden_dw$Text_de[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_de[selection],"<br><br>",
           text_one_f$Text_d)
  print(nationalrat_gemeinden_dw$Text_de[selection]) 
  
  ##FR##
  text_one_f$Text_f <- gsub("#Names_NR_pro_Ort",
                            paste0("<b>",elected_candidates$firstname," ",elected_candidates$lastname," (",elected_candidates$shortname_fr,")</b>")
                            ,text_one_f$Text_f) 
  nationalrat_gemeinden_dw$Text_fr[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_fr[selection],"<br><br>",
           text_one_f$Text_f)
  
  ##IT##
  text_one_f$Text_i <- gsub("#Names_NR_pro_Ort",
                            paste0("<b>",elected_candidates$firstname," ",elected_candidates$lastname," (",elected_candidates$shortname_it,")</b>")
                            ,text_one_f$Text_i) 
  nationalrat_gemeinden_dw$Text_it[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_it[selection],"<br><br>",
           text_one_f$Text_i)
  
  
} else if (elected_candidates$gender == "m") {

  if(grepl("AI|AR|GL|NW|OW|UR",elected_candidates$area_id) == TRUE) {
    text_one_f <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_small_canton_NR_im_Wohnort_m")
  } else {
    text_one_m <- texts_spreadsheet_UrLena %>%
      filter(Text_ID == "Add_NR_im_Wohnort_one_m")
  } 
  
  ##DE##
  text_one_m$Text_d <- gsub("#Names_NR_pro_Ort",
                            paste0("<b>",elected_candidates$firstname," ",elected_candidates$lastname," (",elected_candidates$shortname_de,")</b>")
                            ,text_one_m$Text_d) 
  nationalrat_gemeinden_dw$Text_de[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_de[selection],"<br><br>",
           text_one_m$Text_d)
  print(nationalrat_gemeinden_dw$Text_de[selection])
  
  ##FR##
  text_one_m$Text_f <- gsub("#Names_NR_pro_Ort",
                            paste0("<b>",elected_candidates$firstname," ",elected_candidates$lastname," (",elected_candidates$shortname_fr,")</b>")
                            ,text_one_m$Text_f) 
  nationalrat_gemeinden_dw$Text_fr[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_fr[selection],"<br><br>",
           text_one_m$Text_f)
  
  ##IT##
  text_one_m$Text_i <- gsub("#Names_NR_pro_Ort",
                            paste0("<b>",elected_candidates$firstname," ",elected_candidates$lastname," (",elected_candidates$shortname_it,")</b>")
                            ,text_one_m$Text_i) 
  nationalrat_gemeinden_dw$Text_it[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_it[selection],"<br><br>",
           text_one_m$Text_i)
}  
  
} else if (nrow(elected_candidates) < 5) {
  text_few <- texts_spreadsheet_UrLena %>%
    filter(Text_ID == "Add_NR_im_Wohnort_few")

  text_few$Text_d <- gsub("#Anzahl_NR_pro_Ort",nrow(elected_candidates),text_few$Text_d)
  text_few$Text_f <- gsub("#Anzahl_NR_pro_Ort",nrow(elected_candidates),text_few$Text_f)
  text_few$Text_i <- gsub("#Anzahl_NR_pro_Ort",nrow(elected_candidates),text_few$Text_i)
  text_elected <- ""
  for (l in 1:nrow(elected_candidates)) {
    text_elected <- paste0(text_elected,elected_candidates$firstname[l]," ",elected_candidates$lastname[l],
                             " (",elected_candidates$shortname_de[l],"), ")  
  }
  text_elected <- substr(text_elected,1,nchar(text_elected)-2)
  text_elected <- stri_replace_last(text_elected,fixed=","," und")
  
  
  ##DE##
  text_few$Text_d <- gsub("#Names_NR_pro_Ort",
                            text_elected
                            ,text_few$Text_d) 
  nationalrat_gemeinden_dw$Text_de[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_de[selection],"<br><br>",
           text_few$Text_d)
  print(nationalrat_gemeinden_dw$Text_de[selection]) 
  
  ##FR##
  text_few$Text_f <- gsub("#Names_NR_pro_Ort",
                          gsub(" und "," et ",text_elected)
                          ,text_few$Text_f) 
  nationalrat_gemeinden_dw$Text_fr[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_fr[selection],"<br><br>",
           text_few$Text_f)
  
  ##IT##
  text_few$Text_i <- gsub("#Names_NR_pro_Ort",
                          gsub(" und "," e ",text_elected)
                          ,text_few$Text_i) 
  nationalrat_gemeinden_dw$Text_it[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_it[selection],"<br><br>",
           text_few$Text_i)

} else if (nrow(elected_candidates) >= 5) {
  text_many <- texts_spreadsheet_UrLena %>%
    filter(Text_ID == "Add_NR_im_Wohnort_many")
  
  ##DE##
  text_many$Text_d <- gsub("#Anzahl_NR_pro_Ort",nrow(elected_candidates),text_many$Text_d)
  nationalrat_gemeinden_dw$Text_de[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_de[selection],"<br><br>",
           text_many$Text_d)
  print(nationalrat_gemeinden_dw$Text_de[selection])
  
  ##FR##
  text_many$Text_f <- gsub("#Anzahl_NR_pro_Ort",nrow(elected_candidates),text_many$Text_f)
  nationalrat_gemeinden_dw$Text_fr[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_fr[selection],"<br><br>",
           text_many$Text_f)
  
  ##IT##
  text_many$Text_i <- gsub("#Anzahl_NR_pro_Ort",nrow(elected_candidates),text_many$Text_i)
  nationalrat_gemeinden_dw$Text_it[selection] <- 
    paste0(nationalrat_gemeinden_dw$Text_it[selection],"<br><br>",
           text_many$Text_i)
}
}  
}  
return(nationalrat_gemeinden_dw)
}  

add_easteregg <- function(nationalrat_gemeinden_dw,
                          texts_spreadsheet_UrLena) {
  
text_easteregg <- texts_spreadsheet_UrLena %>%
    filter(Text_ID == "Easteregg_Normalo")
  
  
return(nationalrat_gemeinden_dw)  
}  
