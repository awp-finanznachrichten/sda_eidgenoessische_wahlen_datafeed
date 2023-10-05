library(httr)
library(XML)
library(xlsx)
library(jsonlite)
library(dplyr)
library(readr)

source("./tools/Funktionen/Utils.R")

#Load Databases
source("load_databases.R")

###Nationalrat

###Results Parties
data_NR_parties <- fromJSON(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten/Testdaten/Testdata_NR_2023_Completed/sd-t-17.02-NRW2023-parteien.json"), flatten = TRUE)

#Stand CH and Kantone
timestamp_NR <- data_NR_parties$timestamp
stand_ch <- data_NR_parties$stand
stand_cantons <- data_NR_parties$stand_kantone

#Results
results_NR_communities <- data_NR_parties$level_gemeinden
results_NR_communities$partei_id <- ifelse(results_NR_communities$partei_id == 2,34,results_NR_communities$partei_id)

results_NR_cantons <- data_NR_parties$level_kantone
results_NR_CH <- data_NR_parties$level_ch

#Merge with parties metadata
results_NR_cantons <- results_NR_cantons %>%
  left_join(parties_metadata,
            by = join_by(partei_id == bfs_id))
results_NR_communities <- results_NR_communities %>%
  left_join(parties_metadata,
            by = join_by(partei_id == bfs_id))

###Results voter turnout
data_NR_voterturnout<- fromJSON(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten/Testdaten/Testdata_NR_2023_Completed/sd-t-17.02-NRW2023-wahlbeteiligung.json"), flatten = TRUE)
results_NR_communities_voterturnout <- data_NR_voterturnout$level_gemeinden
results_NR_cantons_voterturnout <- data_NR_voterturnout$level_kantone
results_NR_CH_voterturnout <- data_NR_voterturnout$level_ch

###Results Candidates
data_NR_candidates <- fromJSON(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten/Testdaten/Testdata_NR_2023_Completed/sd-t-17.02-NRW2023-kandidierende.json"), flatten = TRUE)

#Stand CH and Kantone
timestamp_NR_candidates <- data_NR_candidates$timestamp
stand_ch_candidates <- data_NR_candidates$stand
stand_cantons_candidates <- data_NR_candidates$stand_kantone

#Results
results_NR_communities_candidates <- data_NR_candidates$level_gemeinden
results_NR_cantons_candidates <- data_NR_candidates$level_kantone

#Merge with parties metadata
results_NR_cantons_candidates <- results_NR_cantons_candidates %>%
  left_join(parties_metadata,
            by = join_by(kandidat_partei_id == bfs_id))

#Merge with location data
results_NR_cantons_candidates <- results_NR_cantons_candidates %>%
  left_join(areas_metadata, join_by(kanton_nummer == bfs_ID))

#Create source ID
results_NR_cantons_candidates$source_person_id <- paste0(formatC(results_NR_cantons_candidates$kanton_nummer,width = 2,flag = "0"),
                                                         formatC(results_NR_cantons_candidates$liste_nummer_bfs,width = 2,flag = "0"),
                                                         formatC(results_NR_cantons_candidates$kandidat_nummer,width = 2,flag = "0")
)


###Staenderat

#Candidate Results
#data_SR_candidates <- fromJSON("https://www.bfs.admin.ch/bfsstatic/dam/assets/9386472/master", flatten = TRUE)
data_SR_candidates <- fromJSON(paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_daten/Daten Wahlen 2019/sd-t-17.02-SRW2019-kandidierende-erster-wahlgang-2124808.json"), flatten = TRUE)
results_candidates <- data_SR_candidates$kandidierende



