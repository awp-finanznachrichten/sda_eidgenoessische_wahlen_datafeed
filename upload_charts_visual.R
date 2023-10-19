library(RCurl)
library(dplyr)

###Set path with the charts
setwd("C:/sda_eidgenoessische_wahlen_charts")

###Get Folders
folders <- list.dirs(recursive = FALSE)

###Filter folders from today
folders <- folders[grepl(format(Sys.Date(),"%Y%m%d"),folders)]

###Upload chart in each folder
for (folder in folders) {

#Set wd
setwd(folder)

#Get name of .zip file
zipfile_name <- list.files(pattern=".zip")

#Upload data
ftp_adress <- paste0("ftp://ftp.keystone.ch/",zipfile_name)
ftpUpload(zipfile_name, ftp_adress,userpwd=Sys.getenv("ftp_visual"))

#set wd back
setwd("..")

print(paste0("Chart ",zipfile_name," uploaded successfully"))

Sys.sleep(5)
}
