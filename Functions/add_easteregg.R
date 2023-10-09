add_easteregg <- function(nationalrat_gemeinden_dw,
                          texts_spreadsheet_UrLena) {
  
text_easteregg <- texts_spreadsheet_UrLena %>%
    filter(Text_ID == "Easteregg_Normalo")
  
#Get parties results
mydb <- connectDB(db_name = "sda_elections")
rs <-
  dbSendQuery(
    mydb,
    paste0(
      "SELECT * FROM parties_results WHERE election_id = '2023-10-22_CH_NR'"
    )
  )
results_parties <- fetch(rs, n = -1)
dbDisconnectAll()


#Merge with party_metadata
results_parties <- results_parties %>%
  left_join(parties_metadata,
            by = join_by(party_ID == id)) %>%
  filter(party_ID == 1 |
           party_ID == 2 |
           party_ID == 3 |
           party_ID == 56 |
           party_ID == 70 |
           party_ID == 75 )


#Average Community
differences_parties <- results_NR_communities %>%
  arrange(gemeinde_nummer,
          id) %>%
  filter(is.na(partei_staerke) == FALSE,   
         gemeinde_nummer < 9000,
         id == 1 |
           id == 2 |
           id == 3 |
           id == 56 |
           id == 70 |
           id == 75 ) %>%
  group_by(gemeinde_nummer) %>%
  summarise(count = n(),
            gemeinde_bezeichnung = max(gemeinde_bezeichnung),
            diff_SP = partei_staerke[1]-results_parties$voter_share_prior[1], #ADAPT TO voter_share
            diff_SVP = partei_staerke[2]-results_parties$voter_share_prior[2],
            diff_GLP = partei_staerke[3]-results_parties$voter_share_prior[3],
            diff_FDP = partei_staerke[4]-results_parties$voter_share_prior[4],
            diff_Mitte = partei_staerke[5]-results_parties$voter_share_prior[5],
            diff_GP = partei_staerke[6]-results_parties$voter_share_prior[6]) %>%
  mutate(overall_difference_abs = (abs(diff_SP) +
                                     abs(diff_SVP) +
                                     abs(diff_GLP) +
                                     abs(diff_FDP) +
                                     abs(diff_Mitte) +
                                     abs(diff_GP))/6) %>%
  arrange(overall_difference_abs) %>%
  filter(is.na(overall_difference_abs) == FALSE)

write.xlsx(differences_parties,"./Data/most_average_communities_2023.xlsx")

average_community <- which(nationalrat_gemeinden_dw$ID == differences_parties$gemeinde_nummer[1])

##DE##
text_easteregg$Text_d <- gsub("#Party1_name_d",results_parties$shortname_de[2],text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party1_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[2]+differences_parties$diff_SVP[1],1)),text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party1_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[2],1)),text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party2_name_d",results_parties$shortname_de[1],text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party2_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[1]+differences_parties$diff_SP[1],1)),text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party2_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[1],1)),text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party3_name_d",results_parties$shortname_de[5],text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party3_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[5]+differences_parties$diff_Mitte[5],1)),text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party3_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[5],1)),text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party4_name_d",results_parties$shortname_de[4],text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party4_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[4]+differences_parties$diff_FDP[1],1)),text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party5_name_d",results_parties$shortname_de[6],text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party5_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[6]+differences_parties$diff_GP[1],1)),text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party6_name_d",results_parties$shortname_de[3],text_easteregg$Text_d)
text_easteregg$Text_d <- gsub("#Party6_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[3]+differences_parties$diff_GLP[1],1)),text_easteregg$Text_d)

text_easteregg$Text_d <- gsub("die Grüne","die Grünen",text_easteregg$Text_d)

nationalrat_gemeinden_dw$Text_de[average_community] <- text_easteregg$Text_d
print(nationalrat_gemeinden_dw$Text_de[average_community])

##FR##
text_easteregg$Text_f <- gsub("#Party1_name_f",results_parties$shortname_fr[2],text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party1_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[2]+differences_parties$diff_SVP[1],1)),text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party1_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[2],1)),text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party2_name_f",results_parties$shortname_fr[1],text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party2_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[1]+differences_parties$diff_SP[1],1)),text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party2_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[1],1)),text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party3_name_f",results_parties$shortname_fr[5],text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party3_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[5]+differences_parties$diff_Mitte[5],1)),text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party3_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[5],1)),text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party4_name_f",results_parties$shortname_fr[4],text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party4_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[4]+differences_parties$diff_FDP[1],1)),text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party5_name_f",results_parties$shortname_fr[6],text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party5_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[6]+differences_parties$diff_GP[1],1)),text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party6_name_f",results_parties$shortname_fr[3],text_easteregg$Text_f)
text_easteregg$Text_f <- gsub("#Party6_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[3]+differences_parties$diff_GLP[1],1)),text_easteregg$Text_f)

text_easteregg$Text_f <- gsub("le Vert-e-s","les Vert-e-s",text_easteregg$Text_f)

nationalrat_gemeinden_dw$Text_fr[average_community] <- text_easteregg$Text_f


##IT##
text_easteregg$Text_i <- gsub("#Party1_name_i",results_parties$shortname_it[2],text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party1_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[2]+differences_parties$diff_SVP[1],1)),text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party1_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[2],1)),text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party2_name_i",results_parties$shortname_it[1],text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party2_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[1]+differences_parties$diff_SP[1],1)),text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party2_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[1],1)),text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party3_name_i",results_parties$shortname_it[5],text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party3_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[5]+differences_parties$diff_Mitte[5],1)),text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party3_overall_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[5],1)),text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party4_name_i",results_parties$shortname_it[4],text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party4_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[4]+differences_parties$diff_FDP[1],1)),text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party5_name_i",results_parties$shortname_it[6],text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party5_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[6]+differences_parties$diff_GP[1],1)),text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party6_name_i",results_parties$shortname_it[3],text_easteregg$Text_i)
text_easteregg$Text_i <- gsub("#Party6_voter_share",gsub("[.]",",",round2(results_parties$voter_share_prior[3]+differences_parties$diff_GLP[1],1)),text_easteregg$Text_i)

nationalrat_gemeinden_dw$Text_it[average_community] <- text_easteregg$Text_i

return(nationalrat_gemeinden_dw)  
}  
