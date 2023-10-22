email_elected_report_nr <- function(canton = NULL, recipients = "robot-notification@awp.ch") {
  ## canton: Default NULL (don't filter) 
  ##         String with two-chars to indicate canton to filter results
  ## recipients: mail-adresses who should receive the report, separated by comma
  
  library(dplyr)
  library(tableHTML)
  library(stringr)
  library(scales)
  
  #source("C:/Automatisierungen/sda_eidgenoessische_wahlen_2023/tools/Funktionen/Utils.R")
  
  html_table <- function(df) {
    tableHTML(df,
              rownames = FALSE,
              spacing = "5px",
              widths = rep(100, ncol(df)))  %>%
      add_css_header(css = list("text-align", "center"), 
                     headers = 1:ncol(df))  %>%
      add_css_row (css = list('text-align', 'center')) 
  }
  
  runner_ups <- function(df) {
    runnerup <- df %>%
      group_by(party) %>%
      filter(p_seats > 0) %>%
      filter(elected == 0) %>%
      filter(votes == max(votes)) %>%
      arrange(party)
    runnerup <- select(runnerup, c("party", "firstname", "lastname", "gender", "birthdate", "votes"))
    return(html_table(runnerup))
  }
  
  ##########################      
  ##  Get Data            ##                
  ##########################
  year <- 2023
  con <- connectDB(db_name = 'sda_elections')
  ## Current Data
  sqlstr <- paste0("SELECT p.id, p.firstname, p.lastname, p.gender, p.birthdate, TIMESTAMPDIFF(YEAR, p.birthdate, date) as age,
  e.council, e.year, e.canton, l.Name_d AS place, l.urban_rural_3types as urbanity,
  GROUP_CONCAT(DISTINCT(pro.title) separator ', ') AS job,
  GROUP_CONCAT(DISTINCT(pro.category) separator ', ') AS job_category,
  pa.abbreviation_de as party, pr.seats as p_seats, p.note as special,
  CASE WHEN e.status = 2 THEN 1
  ELSE 0 END as incumbent, e.elected, e.votes
  FROM 
  sda_elections.candidates_results e 
  JOIN sda_elections.people_metadata p ON p.id = e.person_id
  JOIN sda_elections.parties_metadata pa ON pa.id = e.party_id
  LEFT JOIN sda_elections.parties_results pr ON pr.election_ID = e.election_id AND pr.area_ID = e.area_id AND pr.party_ID = e.party_id
  LEFT JOIN masterdata.locations l ON e.place_id = l.BFS_nr
  LEFT JOIN sda_elections.people_profession pro ON p.id = pro.person_id 
  	AND (pro.source = 'parlament.ch' OR pro.source = CONCAT('BFS ', e.year))
  GROUP BY p.id, p.firstname, p.lastname, p.gender, p.birthdate, 
  e.council, e.year, e.canton, l.Name_d, pa.abbreviation_de, e.status, e.elected
  HAVING e.year = ", year, " and e.council = 'NR';")
  rs <- dbGetQuery(con, sqlstr)
  all_current_data <- rs
  current_data <- all_current_data[all_current_data$elected == 1, ]
  ## Comp Data
  sqlstr <- "SELECT pe.id, pe.firstname, pe.lastname, pe.gender, pe.birthdate, TIMESTAMPDIFF(YEAR, pe.birthdate, current_date()) as age,
              par.council, CAST(year(current_date()) AS CHAR) as year, par.canton, min(l.Name_d) AS place, l.urban_rural_3types as urbanity,
              GROUP_CONCAT(DISTINCT(pro.title) separator ', ') AS job,
              GROUP_CONCAT(DISTINCT(pro.category) separator ', ') AS job_category,
              pa.abbreviation_de party, 0 as p_seats,
              pe.note as special, 
              CASE WHEN e.status = 2 THEN 1
              ELSE 0 END as incumbent, 1 as elected, 0 as votes
              FROM sda_elections.parliament_members par
              JOIN sda_elections.people_metadata pe ON pe.id = par.person_id 
  				AND par.active = 1
              JOIN sda_elections.parties_metadata pa ON pa.id = par.party_id
              JOIN sda_elections.candidates_results e ON e.person_id = par.person_id AND e.council = par.council AND e.year = 2019
              LEFT JOIN masterdata.locations l ON e.place_id = l.BFS_Nr
              LEFT JOIN sda_elections.people_profession pro ON pe.id = pro.person_id 
  	          AND (pro.source = 'parlament.ch' OR pro.source = 'BFS 2023' OR pro.source = 'BFS 2019')
              GROUP BY pe.id, pe.firstname, pe.lastname, pe.gender, pe.birthdate, 
              par.council, par.canton, pa.abbreviation_de
              HAVING par.council = 'NR'"
  rs <- dbGetQuery(con, sqlstr)
  comp_data <- rs
  dbDisconnectAll()
  
  current_data$comp <- "new"
  comp_data$comp <- "old"
  if (!is.null(canton)) {
    current_data <- current_data[current_data$canton == canton, ]
    comp_data <- comp_data[comp_data$canton == canton, ]
  }
  data <- rbind(current_data, comp_data)
  
  ## Make sure only bigger cantons get a report
  if (nrow(current_data) >= 5) {
    
    ##########################      
    ##  Woman share         ##                
    ##########################
    woman <- data %>%
      group_by_at("comp") %>%
      filter(!is.na(gender)) %>%
      summarise(total = n(),
                female = sum(gender == "f"),
                share = sum(gender == "f")/n())
    woman <- rbind(woman, c("difference", "", "", paste0(round(diff(rev(woman$share))*100, 1), " PP")))
    woman[1:2, grepl("share", colnames(woman))] <- sapply(woman[1:2, grepl("share", colnames(woman))], function(x) percent(as.numeric(x), accuracy = 0.1))
    
    ##########################      
    ##  Age                 ##                
    ##########################
    age <- data %>%
      group_by_at("comp") %>%
      filter(!is.na(age)) %>%
      summarise(total = n(),
                avg = round(mean(age), 1),
                min = min(age),
                max = max(age))
    age <- rbind(age, c("difference", "", 
                        round(diff(rev(age$avg)),1), 
                        diff(rev(age$min)), 
                        diff(rev(age$max))))
    
    ##########################      
    ##  Incumbents          ##                
    ##########################
    incumbents <- data %>%
      group_by_at("comp") %>%
      summarise(total = n(),
                incumbent = sum(incumbent == 1),
                share = incumbent/n())
    incumbents <- rbind(incumbents, c("difference", "", "", 
                                      paste0(round(diff(rev(incumbents$share))*100, 1), " PP")))
    incumbents[1:2, grepl("share", colnames(incumbents))] <- sapply(incumbents[1:2, grepl("share", colnames(incumbents))], function(x) percent(as.numeric(x), accuracy = 0.1))
    
    ##########################      
    ##  Urbanity            ##                
    ##########################
    data$urbanity <- unlist(lapply(as.character(data$urbanity), 
                                   function(x) switch(x, "1" = "urban", 
                                                      "2" = "intermediary", 
                                                      "3" = "rural", "unknown")))
    urbanity <- data %>%
      group_by_at("comp") %>%
      summarise(
        total = n(),
        urban = sum(urbanity == "urban")/total,
        intermediary = (sum(urbanity == "intermediary")/total),
        rural = (sum(urbanity == "rural")/total),
        unknown = (sum(urbanity == "unknown")/total))
    urbanity <- rbind(urbanity, c("difference", "", 
                                  paste0(round(diff(rev(urbanity$urban))*100,1), " PP"), 
                                  paste0(round(diff(rev(urbanity$intermediary))*100,1), " PP"), 
                                  paste0(round(diff(rev(urbanity$rural))*100,1), " PP"), 
                                  paste0(round(diff(rev(urbanity$unknown))*100,1), " PP")))
    urbanity[1:2, 3:6] <- sapply(urbanity[1:2, 3:6], 
                                 function(x) percent(as.numeric(x), accuracy = 0.1))
    
    ##########################      
    ##  Top-Jobs            ##                
    ##########################
    current_total <- nrow(current_data)
    comp_total <- nrow(comp_data)
    
    ## Split by categories
    current_data <- current_data %>% 
      tidyr::separate_rows(job_category, sep = ", ")
    comp_data <- comp_data %>% 
      tidyr::separate_rows(job_category, sep = ", ") 
    
    top_jobs_cur <- current_data %>%
      filter(!is.na(job_category)) %>%
      group_by_at("job_category") %>%
      summarise(count = n(),
                share = percent(n()/current_total)) %>%
      arrange(desc(count))  
    top_jobs_cur <- top_jobs_cur[1:5, ]
    
    top_jobs_comp <- comp_data %>%
      filter(!is.na(job_category)) %>%
      group_by_at("job_category") %>%
      summarise(count = n(),
                share = percent(n()/comp_total)) %>%
      arrange(desc(count))  
    top_jobs_comp <- top_jobs_comp[1:5, ]
    
    ##########################      
    ##  Runner-Ups          ##                
    ##########################
    runnerups <- runner_ups(all_current_data)
    
    ##########################      
    ##  Output              ##                
    ##########################
    report_woman <- html_table(woman) 
    report_age <- html_table(age)
    report_incumbents <- html_table(incumbents)
    report_urbanity <- html_table(urbanity)
    report_jobs_cur <- html_table(top_jobs_cur)
    report_jobs_comp <- html_table(top_jobs_comp)
    
    headline <- paste0("<h1>Report elected National councillors ", canton, "</h1>")
    report <- paste(headline,
                    "<h2>Share of woman</h2>",
                    report_woman,
                    "<h2>Age distribution</h2>",
                    report_age,
                    "<h2>Incumbents</h2>",
                    report_incumbents,
                    "<h2>Urbanity</h2>",
                    report_urbanity,
                    "<h2>New top jobs</h2>",
                    report_jobs_cur,
                    "<h2>Old top jobs</h2>",
                    report_jobs_comp,
                    "<h2>Runner-Ups</h2>",
                    runnerups)
    htmlbody <- paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed/Reports/email_report.html")
    subject <- paste0("Report elected National councillors ", canton)
    
    cat(report, file=file(htmlbody))
    send_html_notification(subject, htmlFilePath = htmlbody, recipients = recipients)
  } else if (nrow(current_data) > 1) {
    
    ##########################      
    ##  Runner-Ups          ##                
    ##########################
    runnerups <- runner_ups(all_current_data)
    
    ##########################      
    ##  Send Notification   ##                
    ##########################
    headline <- paste0("<h1>Runner-ups National councillors ", canton, "</h1>")
    report <- paste0(headline, "<h2>Runner-Ups</h2>", runnerups)
    htmlbody <- paste0(MAIN_PATH,"sda_eidgenoessische_wahlen_datafeed/Reports/email_report.html")
    subject <- paste0("Runner-ups elected National councillors ", canton)
    
    cat(report, file=file(htmlbody))
    send_html_notification(subject, htmlFilePath = htmlbody, recipients = recipients)
  }
  
}


