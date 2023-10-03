LINK_GITHUB <- "https://raw.githubusercontent.com/awp-finanznachrichten/sda_eidgenoessische_wahlen_datafeed/main/Output_Cantons/nationalrat_ergebnisse_gemeinden_"

counted_cantons_all <- election_metadata %>%
  filter(date == "2023-10-22")

#Merge with area, text and output overview
counted_cantons_all <- counted_cantons_all  %>%
  left_join(areas_metadata) %>%
  left_join(status_texts) %>%
  left_join(output_overview) %>%
  filter(area_type == "canton")

###NATIONALRAT###
counted_cantons <- counted_cantons_all %>%
  filter(council == "NR")

###Grafiken erstellen und Daten speichern
grafiken_uebersicht <- data.frame("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(counted_cantons)) {

if (grepl("de",counted_cantons$languages[c]) == TRUE) {  
##Chart URLENA Text DE
data_chart_text <- dw_copy_chart("41Ljo")
chart_id_text <- data_chart_text$id

data_chart_tables <- dw_copy_chart("PMDQR")
chart_id_tables <- data_chart_tables$id


###DE###
undertitel_text <- paste0('<br><span style="line-height:30px">
  <a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_text,'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Texte&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_tables,'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Tabellen</a> &nbsp;
</span>')

undertitel_tabelle <- paste0('<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_text,'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Texte&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_tables,'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Tabellen</a> &nbsp;
</span>')


dw_edit_chart(chart_id_text,
              title=paste0("Wahlen 2023: Parteistärken pro Gemeinde im Kanton ",counted_cantons$area_name_de[c]),
              intro = undertitel_text,
              folderId = folders_URLENA_DE[c],
              data=list("external-data"=paste0(LINK_GITHUB,counted_cantons$area_ID[c],".csv")))
dw_publish_chart(chart_id_text,)
metadata_chart <- dw_retrieve_chart_metadata(chart_id_text)

dw_edit_chart(chart_id_tables,
              title=paste0("Wahlen 2023: Parteistärken pro Gemeinde im Kanton ",counted_cantons$area_name_de[c]),
              intro = undertitel_tabelle,
              folderId = folders_URLENA_DE[c],
              data=list("external-data"=paste0(LINK_GITHUB,counted_cantons$area_ID[c],".csv")))
dw_publish_chart(chart_id_tables)


print("Datawrapper-Chart updated")

new_entry <- data.frame("Ur-Lena DE",
                        counted_cantons$area_ID[c],
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        chart_id_text,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}

  if (grepl("fr",counted_cantons$languages[c]) == TRUE) {  

    data_chart_text <- dw_copy_chart("CjE5Q")
    chart_id_text <- data_chart_text$id
    
    data_chart_tables <- dw_copy_chart("3nrsF")
    chart_id_tables <- data_chart_tables$id
    
    
    ###DE###
    undertitel_text <- paste0('<br><span style="line-height:30px">
  <a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_text,'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;textes&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_tables,'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> &nbsp;&nbsp;tables&nbsp;&nbsp;</a> &nbsp;
</span>')
    
    undertitel_tabelle <- paste0('<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_text,'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;textes&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_tables,'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;tables&nbsp;&nbsp;</a> &nbsp;
</span>')
    
    headline <- paste0("Fédérales 2023: la force des partis par commune dans le canton de ",counted_cantons$area_name_fr[c])
    headline <- str_replace_all(headline,"canton de Jura","canton du Jura")
    headline <- str_replace_all(headline,"canton de Tessin","canton du Tessin")
    headline <- str_replace_all(headline,"dans le canton de Valais","en Valais")
    headline <- str_replace_all(headline,"canton de Argovie","canton d'Argovie")
    headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
    headline <- str_replace_all(headline,"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
    headline <- str_replace_all(headline,"canton de Grisons","canton des Grisons")
    headline <- str_replace_all(headline,"canton de Obwald","canton d'Obwald")
    headline <- str_replace_all(headline,"canton de Uri","canton d'Uri")
    
    
    dw_edit_chart(chart_id_text,
                  title=headline,
                  intro = undertitel_text,
                  folderId = folders_URLENA_FR[c],
                  data=list("external-data"=paste0(LINK_GITHUB,counted_cantons$area_ID[c],".csv")))
    dw_publish_chart(chart_id_text,)
    metadata_chart <- dw_retrieve_chart_metadata(chart_id_text)
    
    dw_edit_chart(chart_id_tables,
                  title=headline,
                  intro = undertitel_tabelle,
                  folderId = folders_URLENA_FR[c],
                  data=list("external-data"=paste0(LINK_GITHUB,counted_cantons$area_ID[c],".csv")))
    dw_publish_chart(chart_id_tables)
    
    
    print("Datawrapper-Chart updated")
    
    new_entry <- data.frame("Ur-Lena FR",
                            counted_cantons$area_ID[c],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            chart_id_text,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  
  if (grepl("it",counted_cantons$languages[c]) == TRUE) {  

    data_chart_text <- dw_copy_chart("JzPo1")
    chart_id_text <- data_chart_text$id
    
    data_chart_tables <- dw_copy_chart("fLPTi")
    chart_id_tables <- data_chart_tables$id
    
    undertitel_text <- paste0('<br><span style="line-height:30px">
  <a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_text,'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;testi&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_tables,'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;tavoli&nbsp;&nbsp;</a> &nbsp;
</span>')
    
    undertitel_tabelle <- paste0('<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_text,'/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;testi&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/',chart_id_tables,'/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;tavoli&nbsp;&nbsp;</a> &nbsp;
</span>')
    
    
    dw_edit_chart(chart_id_text,
                  title=paste0("Federali 2023: forza dei partiti per comune nel canton ",counted_cantons$area_name_it[c]),
                  intro = undertitel_text,
                  folderId = folders_URLENA_IT[c],
                  data=list("external-data"=paste0(LINK_GITHUB,counted_cantons$area_ID[c],".csv")))
    dw_publish_chart(chart_id_text,)
    metadata_chart <- dw_retrieve_chart_metadata(chart_id_text)
    
    dw_edit_chart(chart_id_tables,
                  title=paste0("Federali 2023: forza dei partiti per comune nel canton ",counted_cantons$area_name_it[c]),
                  intro = undertitel_tabelle,
                  folderId = folders_URLENA_IT[c],
                  data=list("external-data"=paste0(LINK_GITHUB,counted_cantons$area_ID[c],".csv")))
    dw_publish_chart(chart_id_tables)
    
    
    print("Datawrapper-Chart updated")
    
    new_entry <- data.frame("Ur-Lena IT",
                            counted_cantons$area_ID[c],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            chart_id_text,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Gebiet","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  
}

saveRDS(grafiken_uebersicht,"grafiken_uebersicht_URLENA.RDS")
write.xlsx(grafiken_uebersicht,"grafiken_uebersicht_URLENA.xlsx",row.names = FALSE)


