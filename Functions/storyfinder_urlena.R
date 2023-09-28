get_storyboard_urlena <- function(ergebnisse_gemeinde) {

storyboard <- c("Intro_normal","Second_normal","Third_normal")

ergebnisse_gemeinde <- ergebnisse_gemeinde %>%
  mutate(rank_current_election = rank(round2(-partei_staerke,1),ties.method = "min"),
         rank_last_election = rank(round2(-letzte_wahl_partei_staerke,1),ties.method = "min"))

party_first <- ergebnisse_gemeinde %>%
  filter(rank_current_election == 1)
party_second  <- ergebnisse_gemeinde %>%
  filter(rank_current_election == 2)
party_third  <- ergebnisse_gemeinde %>%
  filter(rank_current_election == 3)

first_part_needed <- TRUE
second_part_needed <- TRUE
third_part_needed <- TRUE
change_first <- party_first$differenz_partei_staerke[1]
change_second <- party_second$differenz_partei_staerke[1]
change_third <- party_third$differenz_partei_staerke[1]

unchanged_check_first <- FALSE
unchanged_check_second <- FALSE
unchanged_check_third <- FALSE
if (nrow(party_first) == 1) {
unchanged_check_first <- party_first$rank_current_election == party_first$rank_last_election
}
if (nrow(party_second) == 1) {
unchanged_check_second <- party_second$rank_current_election == party_second$rank_last_election
}
if (nrow(party_third) == 1) {
unchanged_check_third <- party_third$rank_current_election == party_third$rank_last_election
}

if (nrow(party_first) > 2) {
storyboard <- c("Spec_three_parties_same","","")
first_part_needed <- FALSE
second_part_needed <- FALSE
third_part_needed <- FALSE
} else if (ergebnisse_gemeinde$kanton_nummer[1] == 16 ) {
storyboard <- c("Spec_small_canton_one_candidate","","")
first_part_needed <- FALSE
second_part_needed <- FALSE
third_part_needed <- FALSE
} else if (ergebnisse_gemeinde$kanton_nummer[1] == 15 ||
  ergebnisse_gemeinde$kanton_nummer[1] == 8 ||
  ergebnisse_gemeinde$kanton_nummer[1] == 7 ||
  ergebnisse_gemeinde$kanton_nummer[1] == 6 ||
  ergebnisse_gemeinde$kanton_nummer[1] == 4) {
storyboard <- c("Spec_small_canton_several_candidates","","")
first_part_needed <- FALSE
second_part_needed <- FALSE
third_part_needed <- FALSE
}  else if  (nrow(party_first) > 1) {
    storyboard[1] <- "Intro_tie_strongest"
    storyboard[2] <- ""
    first_part_needed <- FALSE
    second_part_needed <- FALSE
} else if  (nrow(party_second) > 1) {
  storyboard[2] <- "Second_tie_third"
  storyboard[3] <- ""
  second_part_needed <- FALSE
  third_part_needed <- FALSE 
} else if  (nrow(party_third) > 1) {
  storyboard[3] <- "Third_tie_fourth"
  third_part_needed <- FALSE
} else if (nrow(party_second) == 0) {
  storyboard[2] <- ""
  second_part_needed <- FALSE
  third_part_needed <- FALSE
} else if (nrow(party_third) == 0) {
  storyboard[3] <- ""
  third_part_needed <- FALSE 
} else if (party_first$partei_id == 35) {
storyboard[1] <- "Intro_Uebrige"
first_part_needed <- FALSE
} else if (party_second$partei_id == 35) {
storyboard[2] <- "Second_Uebrige"
second_part_needed <- FALSE
}  else if (party_third$partei_id == 35) {
storyboard[3] <- "Third_Uebrige"
third_part_needed <- FALSE
}  else if ((party_first$stimmen_partei[1] < 200) & (nrow(party_first) == 1) & (nrow(party_second) == 1) & (nrow(party_third) == 1) ) {
  storyboard <- c("Spec_small_community","","")
  first_part_needed <- FALSE
  second_part_needed <- FALSE
  third_part_needed <- FALSE
} 

#First Part
if (first_part_needed == TRUE) {
if (unchanged_check_first == TRUE) {
if (party_first$partei_staerke > 50) {
storyboard[1] <- "Intro_majority" 
} else {
if (change_first > 8) {
storyboard[1] <- "Intro_unchanged_big_jump_percentage"  
}
if (change_first < -8) {
storyboard[1] <- "Intro_unchanged_big_fall_percentage"  
}
if (nrow(party_second) == 1) {
if ((party_first$partei_staerke - party_second$partei_staerke)  > 20) { #10
storyboard[1] <- "Intro_unchanged_big_gap"
second_part_needed <- FALSE
storyboard[2] <- ""
}
if ((party_first$partei_staerke - party_second$partei_staerke) < 3) { #2
storyboard[1] <- "Intro_unchanged_small_gap"
second_part_needed <- FALSE
storyboard[2] <- ""
} 
}  
}

} else {
storyboard[1] <- "Intro_changed_normal"
if (change_first > 5) {
storyboard[1] <- "Intro_changed_big_jump_percentage"
}  
}  
}

#Second Part
if (second_part_needed == TRUE) {
if (change_second > 5) {
  storyboard[2] <- "Second_big_jump_percentage"  
}
if (change_second < -5) {
  storyboard[2] <- "Second_big_fall_percentage"  
}
  
}  
#Third Part
if (third_part_needed == TRUE) {
if (change_third > 5) {
    storyboard[3] <- "Third_big_jump_percentage"  
}
if (change_third < -5) {
    storyboard[3] <- "Third_big_fall_percentage"  
}
}  


if ((sum(unchanged_check_first,unchanged_check_second,unchanged_check_third) == 3) & 
  (sum(first_part_needed,second_part_needed,third_part_needed) == 3) ) {
if ((abs(change_first) < 5) &
    (abs(change_second) < 5) &
    (abs(change_third) < 5) ) {
  storyboard <- c("Spec_unchanged_1_2_3","","")  
}  
}  
return(storyboard)
}
