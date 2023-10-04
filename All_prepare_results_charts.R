#Get parties results from Canton
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
            by = join_by(party_ID == id))

results_parties <- results_parties %>%
 filter(voter_share > 0,
         seats != 0 |
           seats_change != 0 |
           voter_share >= 3) %>%
  arrange(desc(seats),
          desc(voter_share)) %>%
  select(shortname_de,
         shortname_fr,
         shortname_it,
         voter_share,
         voter_share_change,
         seats,
         seats_change,
         party_color)

results_parties$text_votes_de <- paste0("<b>",results_parties$shortname_de,"<br>",
                             trimws(format(round2(results_parties$voter_share,1),nsmall=1)),"%",
                             "<br>(+",trimws(format(round2(results_parties$voter_share_change,1),nsmall=1)),"%)")
results_parties$text_votes_de <- gsub("[+]-","-",results_parties$text_votes_de)
results_parties$text_votes_de <- gsub("[+]0[.]0%","-",results_parties$text_votes_de)  

results_parties$text_seats_de <- paste0("<b>",results_parties$shortname_de,"<br>",
                            results_parties$seats,
                            "<br>(+",results_parties$seats_change,")")
results_parties$text_seats_de <- gsub("[+]-","-",results_parties$text_seats_de)
results_parties$text_seats_de <- gsub("[+]0","-",results_parties$text_seats_de)

results_parties$text_votes_fr <- paste0("<b>",results_parties$shortname_fr,"<br>",
                                        trimws(format(round2(results_parties$voter_share,1),nsmall=1)),"%",
                                        "<br>(+",trimws(format(round2(results_parties$voter_share_change,1),nsmall=1)),"%)")
results_parties$text_votes_fr <- gsub("[+]-","-",results_parties$text_votes_fr)
results_parties$text_votes_fr <- gsub("[+]0[.]0%","-",results_parties$text_votes_fr)  

results_parties$text_seats_fr <- paste0("<b>",results_parties$shortname_fr,"<br>",
                                        results_parties$seats,
                                        "<br>(+",results_parties$seats_change,")")
results_parties$text_seats_fr <- gsub("[+]-","-",results_parties$text_seats_fr)
results_parties$text_seats_fr <- gsub("[+]0","-",results_parties$text_seats_fr)

results_parties$text_votes_it <- paste0("<b>",results_parties$shortname_it,"<br>",
                                        trimws(format(round2(results_parties$voter_share,1),nsmall=1)),"%",
                                        "<br>(+",trimws(format(round2(results_parties$voter_share_change,1),nsmall=1)),"%)")
results_parties$text_votes_it <- gsub("[+]-","-",results_parties$text_votes_it)
results_parties$text_votes_it <- gsub("[+]0[.]0%","-",results_parties$text_votes_it)  

results_parties$text_seats_it <- paste0("<b>",results_parties$shortname_it,"<br>",
                                        results_parties$seats,
                                        "<br>(+",results_parties$seats_change,")")
results_parties$text_seats_it <- gsub("[+]-","-",results_parties$text_seats_it)
results_parties$text_seats_it <- gsub("[+]0","-",results_parties$text_seats_it)



#Merge with area, text and output overview
counted_ch <- election_metadata  %>%
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(election_ID == "2023-10-22_CH_NR")


texts_chart <- get_text_charts(language="de",
                               elections_metadata = counted_ch)
texts_chart_fr <- get_text_charts(language="fr",
                                  elections_metadata = counted_ch)
texts_chart_it <- get_text_charts(language="it",
                                  elections_metadata = counted_ch)


