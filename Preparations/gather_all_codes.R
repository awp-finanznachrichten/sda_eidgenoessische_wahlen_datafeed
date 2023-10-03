grafiken_overview <- rbind(grafiken_uebersicht_all,
                           grafiken_uebersicht_all_fr,
                           grafiken_uebersicht_all_it)

grafiken_overview <- grafiken_overview %>%
  arrange(Gebiet,
          Sprache)

write.xlsx(grafiken_overview,"./Data/overview_all_charts_datawrapper.xlsx",row.names = FALSE)
