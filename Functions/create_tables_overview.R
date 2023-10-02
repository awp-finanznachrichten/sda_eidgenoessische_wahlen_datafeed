create_table_overview <- function(data_canton,
                                    language) {
  
  if (language == "de") {  
    ##Create Table DE
    tabelle <- "<table>
<tr>
<td><b>Partei</b></td>
<td><b>Sitze</b></td>
<td><b>+/-</b></td>
<td><b>Wähleranteil</b></td>
<td style='text-align: center'><b>+/-</b></td>
</tr>"
    
 for (i in 1:nrow(data_canton)) {
      tabelle <- paste0(tabelle,
                        "<tr><td>",data_canton$shortname_de[i],"</td>",
                        "<td style='text-align: center'>",data_canton$seats[i],"</td>")
      
      if (data_canton$seats_change[i] > 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: darkblue'><b>+",data_canton$seats_change[i],"</b></td>")  
      } else if (data_canton$seats_change[i] < 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: red'><b>",data_canton$seats_change[i],"</b></td>")  
      } else {
        tabelle <- paste0(tabelle,"<td style='text-align: center'>-</td>")
      }  
      tabelle <- paste0(tabelle,
                        "<td style='text-align: center'>",format(round2(data_canton$voter_share[i],1),nsmall=1),"%</td>")
      
      if (data_canton$voter_share_change[i] > 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: darkblue'><b>+",format(round2(data_canton$voter_share_change[i],1),nsmall=1),"</b></td></tr>")  
      } else if (data_canton$voter_share_change[i] < 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: red'><b>",format(round2(data_canton$voter_share_change[i],1),nsmall=1),"</b></td></tr>")  
      } else {
        tabelle <- paste0(tabelle,"<td style='text-align: center'>-</td></tr>")
      }  
      
    }      
    
  }    

  if (language == "fr") {  
    ##Create Table FR
    tabelle <- "<table>
<tr>
<td><b>Parti</b></td>
<td><b>Nombre de sièges</b></td>
<td><b>+/-</b></td>
<td><b>Wähleranteil</b></td>
<td style='text-align: center'><b>+/-</b></td>
</tr>"
    
    for (i in 1:nrow(data_canton)) {
      tabelle <- paste0(tabelle,
                        "<tr><td>",data_canton$shortname_fr[i],"</td>",
                        "<td style='text-align: center'>",data_canton$seats[i],"</td>")
      
      if (data_canton$seats_change[i] > 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: darkblue'><b>+",data_canton$seats_change[i],"</b></td>")  
      } else if (data_canton$seats_change[i] < 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: red'><b>",data_canton$seats_change[i],"</b></td>")  
      } else {
        tabelle <- paste0(tabelle,"<td style='text-align: center'>-</td>")
      }  
      tabelle <- paste0(tabelle,
                        "<td style='text-align: center'>",format(round2(data_canton$voter_share[i],1),nsmall=1),"%</td>")
      
      if (data_canton$voter_share_change[i] > 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: darkblue'><b>+",format(round2(data_canton$voter_share_change[i],1),nsmall=1),"</b></td></tr>")  
      } else if (data_canton$voter_share_change[i] < 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: red'><b>",format(round2(data_canton$voter_share_change[i],1),nsmall=1),"</b></td></tr>")  
      } else {
        tabelle <- paste0(tabelle,"<td style='text-align: center'>-</td></tr>")
      }  
      
    }    
  }
  
  
  if (language == "it") {  
    ##Create Table IT
    tabelle <- "<table>
<tr>
<td><b>Partei</b></td>
<td><b>Sitze</b></td>
<td><b>+/-</b></td>
<td><b>Wähleranteil</b></td>
<td style='text-align: center'><b>+/-</b></td>
</tr>"
    
    for (i in 1:nrow(data_canton)) {
      tabelle <- paste0(tabelle,
                        "<tr><td>",data_canton$shortname_it[i],"</td>",
                        "<td style='text-align: center'>",data_canton$seats[i],"</td>")
      
      if (data_canton$seats_change[i] > 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: darkblue'><b>+",data_canton$seats_change[i],"</b></td>")  
      } else if (data_canton$seats_change[i] < 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: red'><b>",data_canton$seats_change[i],"</b></td>")  
      } else {
        tabelle <- paste0(tabelle,"<td style='text-align: center'>-</td>")
      }  
      tabelle <- paste0(tabelle,
                        "<td style='text-align: center'>",format(round2(data_canton$voter_share[i],1),nsmall=1),"%</td>")
      
      if (data_canton$voter_share_change[i] > 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: darkblue'><b>+",format(round2(data_canton$voter_share_change[i],1),nsmall=1),"</b></td></tr>")  
      } else if (data_canton$voter_share_change[i] < 0) {
        tabelle <- paste0(tabelle,"<td style='text-align: center; color: red'><b>",format(round2(data_canton$voter_share_change[i],1),nsmall=1),"</b></td></tr>")  
      } else {
        tabelle <- paste0(tabelle,"<td style='text-align: center'>-</td></tr>")
      }  
      
    }    
  }
  
  tabelle <- paste0(tabelle,"</tbody></table>")
  return(tabelle)    
}    


