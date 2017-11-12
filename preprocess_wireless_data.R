library(dplyr)
library(readr)

preprocess_wireless_data <- function(x) {
  wireless_data_path <- 'HMC\ Math\ 158/wireless/2016FallSemWireless.tsv'
  
  wireless <- read_delim(wireless_data_path, '\t', escape_double = FALSE, trim_ws=TRUE)
  
  # replace missing values with NA
  wireless[wireless == '-'] <- NA
  
  # impute OSdev NA values or set to ''
  # try replacing with manuf
  osdev_NA_indices <- is.na(wireless$OSdev)
  wireless$OSdev[osdev_NA_indices] <- wireless$manuf[osdev_NA_indices]
  # if still NA replace with ''
  wireless$OSdev[is.na(wireless$OSdev)] <- ''
  
  # drop irrelevant columns
  cols_to_drop <- 
    wireless <- wireless %>% 
    select(-c(MACID, UUID, role, WAPID, DST, avgKbps, vendor,
              radioMode, AOSdev, manuf, make, model, Osdetail))
  
  # replace folder and location missing with ''
  wireless$folder[is.na(wireless$folder)] <- ''
  wireless$folder[is.na(wireless$location)] <- ''
  
  # drop rows with NA
  wireless <- na.omit(wireless)
  
  # make disconnet_month and disconnect_minute numeric
  wireless$disconnect_month <- as.numeric(wireless$disconnect_month)
  wireless$disconnect_minute <- as.numeric(wireless$disconnect_minute)
  
  # fix duration column
  wireless <- wireless %>% mutate(
    start_datetime = paste(
      connect_day, 
      connect_month, 
      connect_year, 
      connect_hour, 
      connect_minute, 
      0,  
      sep = '-') %>% dmy_hms(),
    stop_datetime = paste(
      disconnect_day, 
      disconnect_month, 
      disconnect_year, 
      # replace midnight with 24 if disconnect at midnight
      ifelse(disconnect_hour == 0 & connect_hour != 0, 24, disconnect_hour),
      disconnect_minute, 
      0,  
      sep = '-') %>% dmy_hms()
  )
  # find problematic examples
  is_incorrect_duration <- which(wireless$stop_datetime < wireless$start_datetime)
  # increment disconnect day for examples where duration is wrong
  wireless$stop_datetime[is_incorrect_duration] <-  
    wireless$stop_datetime[is_incorrect_duration] + days(1)
  
  # create new duration_mins
  wireless <- wireless %>% mutate(
    duration_mins = as.numeric(
      difftime(stop_datetime, start_datetime, units = 'min')
    )
  )
  return(wireless)
}
