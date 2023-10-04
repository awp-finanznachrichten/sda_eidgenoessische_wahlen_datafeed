library(git2r)
library(httr)
library(XML)
library(xlsx)
library(jsonlite)
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(DatawRappr)
library(lubridate)
library(rsvg)
library(magick)
library(zip)
library(RCurl)

#Main Data URL from BFS
BFS_API_URL <-
  "https://ckan.ogdch-abnahme.clients.liip.ch/api/3/action/package_show?id=eidg-wahlen-2023"
#BFS_API_URL <- "https://ckan.opendata.swiss/api/3/action/package_show?id=eidg-wahlen-2023"

#Datawrapper Auth
datawrapper_auth(Sys.getenv("DW_KEY"), overwrite = TRUE)

#Functions
setwd("./Functions")
source("storyfinder.R")
source("storyfinder_urlena.R")
source("storybuilder.R")
source("add_texts.R")
source("replace_variables_urlena.R")
source("cleanup_urlena.R")
source("replace_variables_cleanup.R")
source("replace_variables_cleanup_SR.R")
source("winners_losers.R")
source("voted_out_candidates.R")
source("text_charts.R")
source("create_tables_NR.R")
source("create_tables_SR.R")
source("create_tables_overview.R")
source("create_table_communities.R")
source("create_bilddaten.R")
source("create_visual_data.R")
source("function_reports.R")
source("send_mail.R")
source("github.R")
setwd("..")
source("./tools/Funktionen/Utils.R")

#Texts
texts_spreadsheet_NR_results <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Sitzverteilung")
texts_spreadsheet_NR_candidates <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Gewaehlte")
texts_spreadsheet_SR_candidates <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "SR_Resultate")
texts_spreadsheet_NR_intermediate <- read.xlsx("./Texte/Eidgenössische Wahlen 2023_ Textbausteine.xlsx",sheetName = "NR_Zwischenstand")
texts_spreadsheet_UrLena <- read.xlsx("./Texte/LENA Textbausteine Eidgenössische Wahlen 2023_ Gemeindeebene.xlsx", sheetName = "Textbausteine")
texts_spreadsheet_UrLena <- texts_spreadsheet_UrLena %>%
  filter(is.na(Text_d) == FALSE)

#Metadata Communities
meta_gmd_kt <- read_csv("Data/MASTERFILE_GDE.csv")