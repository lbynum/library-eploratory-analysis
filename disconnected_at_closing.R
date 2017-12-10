library(dplyr)
library(lubridate)

disconnected_at_closing <- function(disconnectTime) {
  # vectors to store if each data point is a weekday, weekend, or should
  # be treated as both
  is_weekday <- wday(disconnectTime, label = FALSE) >= 2 &  
                wday(disconnectTime, label = FALSE) <= 5
  is_weekend <- wday(disconnectTime, label = FALSE) == 7
  is_both <- wday(disconnectTime, label = FALSE) == 6
  
  # time requirements to determine if the data point disconnected at closing
  after_1250 <- hour(disconnectTime) == 0 & minute(disconnectTime) >= 50
  at_1 <- hour(disconnectTime) == 1
  after_950 <- hour(disconnectTime) == 21 & minute(disconnectTime) >= 50
  at_10 <- hour(disconnectTime) == 22
  
  # time requirements for weekdays, weekends, and both
  weekday_either <- after_1250 | at_1
  weekend_either <- after_950 | at_10
  both_either <- FALSE
  
  # determine disconnections at closing
  weekday_disconnection <- is_weekday & weekday_either
  weekend_disconnection <- is_weekend & weekend_either
  both_disconnection <- is_both & both_either
  
  disconnection <- weekday_disconnection | weekend_disconnection | both_disconnection
  
  return(disconnection)
}

disconnected_at_closing <- function(disconnectTime) {
  # vectors to store if each data point is a weekday, weekend, or should
  # be treated as both
  is_weekday <- wday(disconnectTime, label = FALSE) >= 1 &  
    wday(disconnectTime, label = FALSE) <= 4
  is_weekend <- wday(disconnectTime, label = FALSE) == 6
  is_both <- wday(disconnectTime, label = FALSE) == 5
  
  # time requirements to determine if the data point disconnected at closing
  after_1250 <- hour(disconnectTime) == 0 & minute(disconnectTime) >= 50
  at_1 <- hour(disconnectTime) == 1
  after_950 <- hour(disconnectTime) == 21 & minute(disconnectTime) >= 50
  at_10 <- hour(disconnectTime) == 22
  
  # time requirements for weekdays, weekends, and both
  weekday_either <- after_1250 | at_1
  weekend_either <- after_950 | at_10
  both_either <- weekday_either | weekend_either
  
  # determine disconnections at closing
  weekday_disconnection <- is_weekday & weekday_either
  weekend_disconnection <- is_weekend & weekend_either
  both_disconnection <- is_both & both_either
  
  disconnection <- weekday_disconnection | weekend_disconnection | both_disconnection
  
  return(disconnection)
}
