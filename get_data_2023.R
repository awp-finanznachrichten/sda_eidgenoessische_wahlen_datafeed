library(httr)
library(XML)
library(xlsx)
library(jsonlite)
library(dplyr)

setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_2023")
source("./tools/Funktionen/Utils.R")

mydb <- connectDB(db_name="sda_elections")
rs <- dbSendQuery(mydb, "SELECT id,shortname_de,shortname_fr,shortname_it,bfs_id,party_color FROM parties_metadata")
parties_metadata <- fetch(rs,n=-1)
dbDisconnectAll()

parties_metadata <- parties_metadata %>%
  filter(id !=10)

###Nationalrat

###Results Parties
data_NR_parties <- fromJSON("./Testdaten/Testdata_NR_2023_Completed/sd-t-17.02-NRW2023-parteien.json", flatten = TRUE)
results_NR_communities <- data_NR_parties$level_gemeinden
results_NR_cantons <- data_NR_parties$level_kantone
results_NR_CH <- data_NR_parties$level_ch

#Merge with parties metadata
results_NR_communities <- results_NR_communities %>%
  left_join(parties_metadata,
            by = join_by(partei_id == bfs_id))

###Results voter turnout
data_NR_voterturnout<- fromJSON("./Testdaten/Testdata_NR_2023_Completed/sd-t-17.02-NRW2023-wahlbeteiligung.json", flatten = TRUE)
results_NR_communities_voterturnout <- data_NR_voterturnout$level_gemeinden
results_NR_cantons_voterturnout <- data_NR_voterturnout$level_kantone
results_NR_CH_voterturnout <- data_NR_voterturnout$level_ch

###Results Candidates
data_NR_candidates <- fromJSON("./Testdaten/Testdata_NR_2023_Completed/sd-t-17.02-NRW2023-kandidierende.json", flatten = TRUE)
results_NR_communities_candidates <- data_NR_candidates$level_gemeinden
results_NR_cantons_candidates <- data_NR_candidates$level_kantone



###Staenderat

#Candidate Results
#data_SR_candidates <- fromJSON("https://www.bfs.admin.ch/bfsstatic/dam/assets/9386472/master", flatten = TRUE)
data_SR_candidates <- fromJSON("./Daten Wahlen 2019/sd-t-17.02-SRW2019-kandidierende-erster-wahlgang-2124808.json", flatten = TRUE)
results_candidates <- data_SR_candidates$kandidierende


