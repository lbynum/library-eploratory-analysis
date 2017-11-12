library(dplyr)
library(lubridate)

weekday_disconnect <- function(disconnectTime) {
  if (hour(disconnectTime) == 0 && minute(disconnectTime) >= 50) {
    return(TRUE)
  }
  else if (hour(disconnectTime) == 1) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

weekend_disconnection <- function(disconnectTime) {
  if (hour(disconnectTime) == 21 && minute(disconnectTime) >= 50) {
    return(TRUE)
  }
  else if (hour(disconnectTime) == 22) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

disconnected_at_closing <- function(disconnectTime) {
  if (2 <= wday(disconnectTime) && 6 > wday(disconnectTime)) { 
    return(weekday_disconnect(disconnectTime))
  }
  if (wday(disconnectTime) == 6) {
    return(weekday_disconnect(disconnectTime) || 
      weekend_disconnect(disconnectTime))
  }
  if (wday(disconnectTime) == 7) {
    return(weekend_disconnect(disconnectTime))
  }
}