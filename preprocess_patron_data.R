library(readr)

preprocess_patron_data <- function(wireless, ezproxy) {
  patron_data_path <- "HMC\ Math\ 158/PatronsID_hashedXPatronType.csv"
  
  patron <- read.csv(patron_data_path, header = TRUE)
  
  # patron <- merge(x = wireless, y = patron, by.x = "UUID", by.y = "uuid")
  
  patron <- merge(x = ezproxy, y = patron, by = "uuid")
  
  patron
}