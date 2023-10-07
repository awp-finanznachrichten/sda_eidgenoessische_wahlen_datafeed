chart_id_text_de <- "3YqND"
chart_id_tabelle_de <- "9qTxY"
chart_id_text_fr <- "XQ32E"
chart_id_tabelle_fr <- "1PjhW"
chart_id_text_it <- "n1scm"
chart_id_tabelle_it <- "a3EW0"

undertitel_de <- paste0("Letzte Aktualisierung: ",format(Sys.time(),"%H:%M Uhr"),". ","Es sind <b>",stand_ch$gemeinden_abgeschlossen,"</b> von <b>",stand_ch$gemeinden_total,
                            "</b> Gemeinden ausgezählt.")
undertitel_fr <- paste0("dernière mise à jour: ",format(Sys.time(),"%Hh%M"),". ","Les résultats de <b>",stand_ch$gemeinden_abgeschlossen,"</b> des <b>",stand_ch$gemeinden_total,
                            "</b> communes sont connus.")
undertitel_it <- paste0("Ultimo aggiornamento: ",format(Sys.time(),"%H:%M"),". ","I risultati di <b>",stand_ch$gemeinden_abgeschlossen,"</b> dei <b>",stand_ch$gemeinden_total,
                            "</b> comuni sono noti.")
    
###DE###
undertitel_de_text <- paste0(undertitel_de,'<br><span style="line-height:30px">
  <a target="_self" href="https://datawrapper.dwcdn.net/3YqND/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Texte&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/9qTxY/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Tabellen</a> &nbsp;
</span>')
dw_edit_chart(chart_id_text_de,intro = undertitel_de_text)
dw_publish_chart(chart_id_text_de)

undertitel_de_tabelle <- paste0(undertitel_de,'<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/3YqND/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;Texte&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/9qTxY/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> Tabellen</a> &nbsp;
</span>')
dw_edit_chart(chart_id_tabelle_de,intro = undertitel_de_tabelle)
dw_publish_chart(chart_id_tabelle_de)


###FR###
undertitel_fr_text <- paste0(undertitel_fr,'<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/XQ32E/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;textes&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/1PjhW/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> &nbsp;&nbsp;graphiques&nbsp;&nbsp;</a> &nbsp;
</span>')
dw_edit_chart(chart_id_text_fr,intro = undertitel_fr_text)
dw_publish_chart(chart_id_text_fr)

undertitel_fr_tabelle <- paste0(undertitel_fr,'<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/XQ32E/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;textes&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/1PjhW/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> &nbsp;&nbsp;graphiques&nbsp;&nbsp;</a> &nbsp;
</span>')
dw_edit_chart(chart_id_tabelle_fr,intro = undertitel_fr_tabelle)
dw_publish_chart(chart_id_tabelle_fr)


###IT###
undertitel_it_text <- paste0(undertitel_it,'<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/n1scm/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;testi&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/a3EW0/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> &nbsp;&nbsp;grafici&nbsp;&nbsp;</a> &nbsp;
</span>')
dw_edit_chart(chart_id_text_it,intro = undertitel_it_text)
dw_publish_chart(chart_id_text_it)

undertitel_it_tabelle <- paste0(undertitel_it,'<br>
<span style="line-height:30px">
<a target="_self" href="https://datawrapper.dwcdn.net/n1scm/" style="background:#808080; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer">&nbsp;&nbsp;testi&nbsp;&nbsp;</a> &nbsp;
<a target="_self" href="https://datawrapper.dwcdn.net/a3EW0/" style="background:#429ddd; padding:4px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;" rel="nofollow noopener noreferrer"> &nbsp;&nbsp;grafici&nbsp;&nbsp;</a> &nbsp;
</span>')
dw_edit_chart(chart_id_tabelle_it,intro = undertitel_it_tabelle)
dw_publish_chart(chart_id_tabelle_it)

