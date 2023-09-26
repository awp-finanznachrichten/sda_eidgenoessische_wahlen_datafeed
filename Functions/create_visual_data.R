create_visual_data <- function(chart_id,
                               upload = "no",
                               eps = "yes") {

setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_charts")

chart_metadata <- dw_retrieve_chart_metadata(chart_id)

metadata_language <- substr(chart_metadata[["content"]][["language"]],1,2)
metadata_title <- gsub("’","'",toupper(chart_metadata[["content"]][["title"]]))
metadata_description <- gsub("’","'",paste0(chart_metadata[["content"]][["title"]]))

metadata_topic <- "Politik"
if (substr(chart_metadata[["content"]][["language"]],1,2) == "fr") {
metadata_topic <- "Politique"
}
if (substr(chart_metadata[["content"]][["language"]],1,2) == "it") {
metadata_topic <- "Politica"
}

#Bilddaten kreieren
create_bilddaten(chart_id,
                 metadata_language,
                 chart_id,
                 metadata_title,
                 metadata_description,
                 metadata_topic,
                 upload = upload,
                 eps = eps
)

setwd("C:/Users/sw/OneDrive/sda_eidgenoessische_wahlen_datafeed")
print("visual data created")
}

