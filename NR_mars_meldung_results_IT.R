library(readr)
library(RCurl)

###Deutsch
date_and_time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),"+02:00")

#ID
ID <- sample(100000000000000:999999999999999,1)

#ID Long
ID_long <- paste0(format(Sys.Date(), "%Y%m%d"),":",format(Sys.time(), "%Y%m%d%H%M%S"),ID)

#Vorlage laden
vorlage <- read_file("./tools/SDA/Vorlage_SDA_Meldungen.txt")

#Abschnitte kreieren
text <- paste0('<p class="paragraph">',paste0(texts_parties_it[3]," ",texts_parties_it[4]),"</p>\n")
text <- paste0(text,'<p class="paragraph">',texts_parties_it[5],"</p>\n")
text <- paste0(text,tabelle_it,"\n")
text <- paste0(text,'<p class="paragraph">',paste0(texts_parties_it[6]," ",texts_parties_it[7]),"</p>\n")
text <- paste0(text,'<p class="paragraph">',texts_parties_it[8],"</p>\n")

###Daten einfügen
vorlage <- gsub("INSERT_LONGID",ID_long,vorlage)
vorlage <- gsub("INSERT_TIME",date_and_time,vorlage)
vorlage <- gsub("INSERT_PROVIDER","KSDA",vorlage)
vorlage <- gsub("INSERT_STATUS","withheld",vorlage)
vorlage <- gsub("INSERT_SERVICE","bsi",vorlage)
vorlage <- gsub("INSERT_NOTE",texts_parties_it[9],vorlage)
vorlage <- gsub("INSERT_MEMO","Dies ist ein Test",vorlage)
vorlage <- gsub("INSERT_HYPERLINK","",vorlage)
vorlage <- gsub("INSERT_URGENCY","3",vorlage)
vorlage <- gsub("INSERT_ID",ID,vorlage)
vorlage <- gsub("INSERT_DATELINE",counted_cantons$hauptort_it[c],vorlage)
vorlage <- gsub("INSERT_LANGUAGE","it",vorlage)
vorlage <- gsub("INSERT_GENRE","RES",vorlage)
vorlage <- gsub("INSERT_STORYTYPES",
                '<subject type="cpnat:abstract" qcode="sdastorytype:tble"></subject>',
                vorlage)
vorlage <- gsub("INSERT_CHANNELS",
                paste0('<subject type="cpnat:abstract" qcode="sdamarschannel:ELE"></subject>\n',
                       '<subject type="cpnat:abstract" qcode="sdamarschannel:LOH"></subject>\n',
                       '<subject type="cpnat:abstract" qcode="sdamarschannel:PRT"></subject>'),
                vorlage)
vorlage <- gsub("INSERT_LOCATIONS",
                paste0('<located type="loctype:city" qcode="sdamarsgeo:CH">\n<name>Schweiz</name>\n</located>'),
                vorlage)
vorlage <- gsub("INSERT_CATCHWORD",texts_parties_it[1],vorlage)
vorlage <- gsub("INSERT_HEADLINE",paste0("***TEST***",texts_parties_it[2]),vorlage)
vorlage <- gsub("INSERT_LEAD"," ",vorlage)
vorlage <- gsub("INSERT_CATCHLINE","",vorlage)
vorlage <- gsub("INSERT_TEXT",text,vorlage)

#Datei speichern
setwd("./Output_Mars")
filename <- paste0(format(Sys.Date(),"%Y%m%d"),"_",counted_cantons$area_ID[c],"_NR_Results_Wahlen2023_it.xml")
cat(vorlage, file = (con <- file(filename, "w", encoding="UTF-8"))); close(con)

Sys.sleep(5)
###FTP-Upload
ftpUpload(filename, paste0("ftp://awp-lena.sda-ats.ch/",filename),userpwd=Sys.getenv("ftp_sda"))

setwd("..")







