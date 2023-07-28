library(httr)
library(XML)
library(xlsx)
library(jsonlite)
library(dplyr)
library(readr)
library(stringi)

#Working Directory
setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")

#Functions
source("functions_storyfinder.R")
source("functions_storybuilder.R")
source("functions_replace_variables.R")
source("./tools/Funktionen/Utils.R")

#Databases
source("load_databases.R")

#Texts
texts_spreadsheet <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Sitzverteilung")

#####START LOOP#####

###Get Data and update DB
source("get_data_bfs_feed.R")
source("update_DB_NR_cantons_parties.R")
source("update_DB_NR_cantons_candidates.R")

###NATIONALRAT###


#Check: Canton completed?


#Text Results

#Text Candidates

#Charts Results

#Charts Candidates

#Analytics


###STAENDERAT###

#Get Data and Update DB

#Text Candidates

#Charts Candidates


###Communities###

#Get Data Communities

#Output tables

#Output texts