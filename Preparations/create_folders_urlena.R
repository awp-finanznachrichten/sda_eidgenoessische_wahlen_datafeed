#Ordnerstruktur Kantone erstellen
team_id <- "6Gn1afus"

cantons_short <- unique(counted_cantons_all$area_ID)
folders_URLENA <- c()
folders_URLENA_DE <- c()
folders_URLENA_FR <- c()
folders_URLENA_IT <- c()


for (canton in cantons_short) {
folder_URLENA <- dw_create_folder(canton,parent_id = "188046",organization_id = team_id)
folders_URLENA <- c(folders_URLENA,folder_URLENA$id)
}  


###Language Folders
for (folder in folders_URLENA) {
folder_URLENA_DE <- dw_create_folder("DE",parent_id = folder)
folder_URLENA_FR <- dw_create_folder("FR",parent_id = folder)
folder_URLENA_IT <- dw_create_folder("IT",parent_id = folder)
folders_URLENA_DE <- c(folders_URLENA_DE,folder_URLENA_DE$id)
folders_URLENA_FR <- c(folders_URLENA_FR,folder_URLENA_FR$id)
folders_URLENA_IT <- c(folders_URLENA_IT,folder_URLENA_IT$id)
}

saveRDS(folders_URLENA,"folders_URLENA.RDS")
saveRDS(folders_URLENA_DE,"folders_URLENA_DE.RDS")
saveRDS(folders_URLENA_FR,"folders_URLENA_FR.RDS")
saveRDS(folders_URLENA_IT,"folders_URLENA_IT.RDS")
