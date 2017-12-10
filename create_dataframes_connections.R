create_disclose_dow <- function(wireless) {
  disclose_dow <- wireless %>% group_by(wday(start_datetime)) %>% 
    summarise(total_disconnects = sum(disconnected_at_closing))
  disclose_dow$connect_dow <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                "Thursday", "Friday", "Saturday")
  for (i in 1:7) {
    disclose_dow$cgu[i] = sum(wireless[wireless$campus == "cgu" &
                                                wday(wireless$start_datetime) == i,
                                                "disconnected_at_closing"])
    disclose_dow$cmc[i] = sum(wireless[wireless$campus == "cmc" &
                                                  wday(wireless$start_datetime) == i,
                                                "disconnected_at_closing"])
    disclose_dow$hmc[i] = sum(wireless[wireless$campus == "hmc" &
                                                     wday(wireless$start_datetime) == i,
                                                   "disconnected_at_closing"])
    disclose_dow$kgi[i] = sum(wireless[wireless$campus == "kgi" &
                                                     wday(wireless$start_datetime) == i,
                                                   "disconnected_at_closing"])
    disclose_dow$pit[i] = sum(wireless[wireless$campus == "pit" &
                                                     wday(wireless$start_datetime) == i,
                                                   "disconnected_at_closing"])
    disclose_dow$pom[i] = sum(wireless[wireless$campus == "pom" &
                                                     wday(wireless$start_datetime) == i,
                                                   "disconnected_at_closing"])
    disclose_dow$scr[i] = sum(wireless[wireless$campus == "scr" &
                                                     wday(wireless$start_datetime) == i,
                                                   "disconnected_at_closing"])
    disclose_dow$total_disconnects[i] = disclose_dow$cgu[i] +
      disclose_dow$cmc[i] + disclose_dow$hmc[i] +
      disclose_dow$kgi[i] + disclose_dow$pit[i] +
      disclose_dow$pom[i] + disclose_dow$scr[i]
  }
  disclose_dow
}

create_disclose_dates <- function(wireless) {
  disclose_dates <- wireless %>% group_by(as.Date(start_datetime)) %>% 
    summarize(total_disconnections = sum(disconnected_at_closing))
}

create_conopen_dow <- function(wireless) {
  conopen_dow <- wireless %>% group_by(wday(start_datetime)) %>% 
    summarise(total_connects = sum(connected_at_opening))
  conopen_dow$connect_dow <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
                               "Thursday", "Friday", "Saturday")
  for (i in 1:7) {
    conopen_dow$cgu[i] = sum(wireless[wireless$campus == "cgu" &
                                        wday(wireless$start_datetime) == i,
                                      "connected_at_opening"])
    conopen_dow$cmc[i] = sum(wireless[wireless$campus == "cmc" &
                                        wday(wireless$start_datetime) == i,
                                      "connected_at_opening"])
    conopen_dow$hmc[i] = sum(wireless[wireless$campus == "hmc" &
                                        wday(wireless$start_datetime) == i,
                                      "connected_at_opening"])
    conopen_dow$kgi[i] = sum(wireless[wireless$campus == "kgi" &
                                        wday(wireless$start_datetime) == i,
                                      "connected_at_opening"])
    conopen_dow$pit[i] = sum(wireless[wireless$campus == "pit" &
                                        wday(wireless$start_datetime) == i,
                                      "connected_at_opening"])
    conopen_dow$pom[i] = sum(wireless[wireless$campus == "pom" &
                                        wday(wireless$start_datetime) == i,
                                      "connected_at_opening"])
    conopen_dow$scr[i] = sum(wireless[wireless$campus == "scr" &
                                        wday(wireless$start_datetime) == i,
                                      "connected_at_opening"])
    conopen_dow$total_connects[i] = conopen_dow$cgu[i] +
      conopen_dow$cmc[i] + conopen_dow$hmc[i] +
      conopen_dow$kgi[i] + conopen_dow$pit[i] +
      conopen_dow$pom[i] + conopen_dow$scr[i]
  }
  conopen_dow
}

create_conopen_date <- function(wireless) {
  disclose_dates <- wireless %>% group_by(as.Date(start_datetime)) %>% 
    summarize(total_disconnections = sum(disconnected_at_closing))
}

