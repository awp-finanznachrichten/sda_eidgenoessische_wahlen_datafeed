grafiken_overview <- rbind(grafiken_uebersicht,
                           grafiken_uebersicht_fr,
                           grafiken_uebersicht_it)

grafiken_overview <- grafiken_overview %>%
  arrange(Gebiet,
          Sprache)

write.xlsx(grafiken_overview,"./Data/overview_second_vote_SR_charts_datawrapper.xlsx",row.names = FALSE)
