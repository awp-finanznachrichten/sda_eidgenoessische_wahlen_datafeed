#Ordnerstruktur Kantone erstellen
team_id <- "6Gn1afus"

cantons_short <- unique(counted_cantons_all$area_ID)
folders_NR <- c()
folders_NR_DE <- c()
folders_NR_FR <- c()
folders_NR_IT <- c()
folders_SR <- c()
folders_SR_DE <- c()
folders_SR_FR <- c()
folders_SR_IT <- c()

for (canton in cantons_short) {
folder_KT <- dw_create_folder(canton,parent_id = "188044",organization_id = team_id)
folder_NR <- dw_create_folder("Nationalrat",parent_id = folder_KT$id)
folder_SR <- dw_create_folder("Ständerat",parent_id = folder_KT$id)
folders_NR <- c(folders_NR,folder_NR$id)
folders_SR <- c(folders_SR,folder_SR$id)
}  


###Language Folders
for (folder in folders_NR) {
folder_NR_DE <- dw_create_folder("DE",parent_id = folder)
folder_NR_FR <- dw_create_folder("FR",parent_id = folder)
folder_NR_IT <- dw_create_folder("IT",parent_id = folder)
folders_NR_DE <- c(folders_NR_DE,folder_NR_DE$id)
folders_NR_FR <- c(folders_NR_FR,folder_NR_FR$id)
folders_NR_IT <- c(folders_NR_IT,folder_NR_IT$id)
}

saveRDS(folders_NR_DE,"folders_NR_DE.RDS")
saveRDS(folders_NR_FR,"folders_NR_FR.RDS")
saveRDS(folders_NR_IT,"folders_NR_IT.RDS")

for (folder in folders_SR) {
  folder_SR_DE <- dw_create_folder("DE",parent_id = folder)
  folder_SR_FR <- dw_create_folder("FR",parent_id = folder)
  folder_SR_IT <- dw_create_folder("IT",parent_id = folder)
  folders_SR_DE <- c(folders_SR_DE,folder_SR_DE$id)
  folders_SR_FR <- c(folders_SR_FR,folder_SR_FR$id)
  folders_SR_IT <- c(folders_SR_IT,folder_SR_IT$id)
}
folders_all <- dw_list_folders()
test <- folders_all[["list"]][[2]][["folders"]][[6]][["folders"]][[4]][["folders"]][[1]][["folders"]]


for (i in 1:26) {
folder_SR_DE <- test[[i]][["folders"]][[2]][["folders"]][[1]][["id"]]
folders_SR_DE <- c(folders_SR_DE,folder_SR_DE)
folder_SR_FR <- test[[i]][["folders"]][[2]][["folders"]][[2]][["id"]]
folders_SR_FR <- c(folders_SR_FR,folder_SR_FR)
folder_SR_IT <- test[[i]][["folders"]][[2]][["folders"]][[3]][["id"]]
folders_SR_IT <- c(folders_SR_IT,folder_SR_IT)
}

saveRDS(folders_SR_DE,"folders_SR_DE.RDS")
saveRDS(folders_SR_FR,"folders_SR_FR.RDS")
saveRDS(folders_SR_IT,"folders_SR_IT.RDS")
