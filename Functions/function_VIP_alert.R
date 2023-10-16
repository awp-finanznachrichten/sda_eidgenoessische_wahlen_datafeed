vip_alert <- function(canton = NULL, council, recipients) {
  canton <- "ZH"
  council <- "NR"
  recipients <- "tt@awp.ch"
  ########################
  ## Function purpose: Sends Alerts if a candidate with a note (= VIP) is elected
  ## Important to know: Utils.R must be loaded
  ## 
  ## Input
  ## canton: Default NULL (don't filter), String with two-chars to indicate canton to filter results
  ## council: String "NR"/"SR" to indicate council to filter results
  ##  
  ## Return
  ## result: Sends E-Mail with alert
  ########################

  ## Filter for canton
  canton_filter <- ""
  if (!is.null(canton)) {
    canton_filter <- paste0(" AND area_id = ", quote_str(canton), " ")
  }

  ## Get Data
  con <- connectDB(db_name = 'sda_elections')
  sqlstr <- paste0("SELECT * FROM 
                  (SELECT * FROM 
                   sda_elections.candidates c
                   WHERE council = ", quote_str(council), canton_filter, ") c
                   JOIN sda_elections.people_metadata p ON c.person_id = p.id
                   WHERE not note is null AND note <> '';")
  rs <- dbGetQuery(con, sqlstr)
  vips_all <- rs
  dbDisconnectAll()
  
  ## Filter data for cantons
  cantons <- unique(vips_all$area_id)
  cantons <- cantons[order(cantons)]
  if (length(cantons) > 0) {
    alert <- ""
    for (i in 1:length(cantons)) {
      alert <- paste0(alert, cantons[i], " ")
      vips <- vips_all[vips_all$area_id == cantons[i], ]
      if (nrow(vips) > 0) {
        elected <- vips[vips$elected == 1, ]
        unelected <- vips[vips$elected == 0, ]
        if (nrow(elected) > 0) {
          alert <- paste0(alert, "ELECTED\n\n")
          alert <- paste0(alert, paste0(elected$firstname, " ", elected$lastname, 
                                        ", ", elected$shortname_de, " (", elected$note, ") ", 
                                        format(elected$votes, big.mark = "'"), " ", "votes", 
                                        collapse = "\n"), "\n\n")
        }
        if (nrow(unelected) > 0) {
          alert <- paste0(alert, "UNELECTED\n\n")
          alert <- paste0(alert, paste0(unelected$firstname, " ", unelected$lastname, 
                                        ", ", unelected$shortname_de, " (", unelected$note, ") ", 
                                        format(unelected$votes, big.mark = "'"), " ", "votes", 
                                        collapse = "\n"), "\n\n")
        }
      }
    }
    subject <- paste("Elected 'VIPS'", council, canton)

    #cat(alert)
    send_notification(subject, alert, recipients = recipients)
  }
}