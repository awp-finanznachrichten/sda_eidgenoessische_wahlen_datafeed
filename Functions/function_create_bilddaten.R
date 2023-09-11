create_bilddaten <- function(chart_id,
                             language = "de",
                             short_name,
                             object_name,
                             caption,
                             headline,
                             upload = "no",
                             eps = "yes"
                             ) {

  #Create Folder
  folder_name <- paste0(format(Sys.Date(),"%Y%m%d"),"_",short_name,"_",toupper(language))
  dir.create(folder_name)
  
  setwd(paste0("./",folder_name))
  
  #Als JPEG
  map <- dw_export_chart(chart_id, plain=FALSE,border_width = 20)
  image_write(map,path="preview.jpg",format="jpeg")
  
  
  #Als SVG &  EPS
  map <- dw_export_chart(chart_id , type="svg",plain=FALSE,border_width = 20)
  cat(map,file=paste0(short_name,".svg"))
  
  if (eps == "yes") {
  map <- charToRaw(map)
  rsvg_eps(map,paste0(short_name,".eps"),width=4800)
  }
  
  #Metadata
  if (language == "de") {

  metadata <- paste0("i5_object_name=INFOGRAFIK ",object_name,"\n",
                     "i55_date_created=",format(Sys.Date(),"%Y%m%d"),"\n",
                     "i120_caption=",caption," Diese Infografik wurde automatisiert vom Schreibroboter Lena erstellt.\n",
                     "i103_original_transmission_reference=\n",
                     "i90_city=\n",
                     "i100_country_code=CHE\n",
                     "i15_category=N\n",
                     "i105_headline=",headline,"\n",
                     ifelse(eps == "yes",
                            "i40_special_instructions=Die Infografik kann im Grafikformat EPS und SVG bezogen werden. Diese Infografik wurde automatisiert vom Schreibroboter Lena erstellt.\n",
                            "i40_special_instructions=Die Infografik kann im Grafikformat SVG bezogen werden. Diese Infografik wurde automatisiert vom Schreibroboter Lena erstellt.\n"),
                     "i110_credit=KEYSTONE\n",
                     "i115_source=KEYSTONE\n",
                     "i80_byline=Lena\n",
                     "i122_writer=Lena\n")
  
  filename <- "metadata.properties"
  cat(metadata, file= (con <- file(filename,"w",encoding="latin1")))
  close(con)
  }
  
  if (language == "fr") {
    
    metadata <- paste0("i5_object_name=INFOGRAPHIE ",object_name,"\n",
                       "i55_date_created=",format(Sys.Date(),"%Y%m%d"),"\n",
                       "i120_caption=",caption," Cette infographie a été réalisée de manière automatisée par le robot d'écriture Lena.\n",
                       "i103_original_transmission_reference=\n",
                       "i90_city=\n",
                       "i100_country_code=CHE\n",
                       "i15_category=N\n",
                       "i105_headline=",headline,"\n",
                       ifelse(eps == "yes",
                              "i40_special_instructions=L'infographie peut être obtenue aux formats graphiques EPS et SVG. Cette infographie a été réalisée de manière automatisée par le robot d'écriture Lena.\n",
                              "i40_special_instructions=L'infographie peut être obtenue aux formats graphiques SVG. Cette infographie a été réalisée de manière automatisée par le robot d'écriture Lena.\n"),
                       "i110_credit=KEYSTONE\n",
                       "i115_source=KEYSTONE\n",
                       "i80_byline=Lena\n",
                       "i122_writer=Lena\n")
    
    filename <- "metadata.properties"
    cat(metadata, file= (con <- file(filename,"w",encoding="latin1")))
    close(con)
  }
  
  if (language == "it") {
    
    metadata <- paste0("i5_object_name=INFOGRAFICA ",object_name,"\n",
                       "i55_date_created=",format(Sys.Date(),"%Y%m%d"),"\n",
                       "i120_caption=",caption," Questa infografica è stata creata automaticamente dal robot di scrittura Lena.\n",
                       "i103_original_transmission_reference=\n",
                       "i90_city=\n",
                       "i100_country_code=CHE\n",
                       "i15_category=N\n",
                       "i105_headline=",headline,"\n",
                       ifelse(eps == "yes",
                              "i40_special_instructionsL'infografica può essere ottenuta nei formati grafici EPS e SVG. Questa infografica è stata creata automaticamente dal robot di scrittura Lena.\n",
                              "i40_special_instructionsL'infografica può essere ottenuta nei formati grafici SVG. Questa infografica è stata creata automaticamente dal robot di scrittura Lena.\n"),
                       "i110_credit=KEYSTONE\n",
                       "i115_source=KEYSTONE\n",
                       "i80_byline=Lena\n",
                       "i122_writer=Lena\n")
    
    filename <- "metadata.properties"
    cat(metadata, file= (con <- file(filename,"w",encoding="latin1")))
    close(con)
  }
  
  #Zip-File erstellen
  if (eps == "yes") {
  zip::zip(zipfile = paste0(short_name,"_",toupper(language),".zip"), 
           c(paste0(short_name,".eps"),
             paste0(short_name,".svg"),
             "preview.jpg",
             "metadata.properties"), 
           mode="cherry-pick")
    
  } else {
  zip::zip(zipfile = paste0(short_name,"_",toupper(language),".zip"), 
             c(paste0(short_name,".svg"),
               "preview.jpg",
               "metadata.properties"), 
             mode="cherry-pick")
  }  
  
  print("Bilddaten erstellt")
  
  if (upload == "yes") {
  ftp_adress <- paste0("ftp://ftp.keystone.ch/",paste0(short_name,"_",toupper(language),".zip"))
  ftpUpload(paste0(short_name,"_",toupper(language),".zip"), ftp_adress,userpwd="keyg_in:5r6368vz")  
     
  print("Bilddaten hochgeladen")  

  }  
  setwd("..")
 
}  

