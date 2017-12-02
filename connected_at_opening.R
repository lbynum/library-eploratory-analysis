library(dplyr)
library(lubridate)

connected_at_opening <- function(connectTime) {
  # vectors to store if each data point is a weekday, weekend, or should
  # be treated as both
  is_weekday <- 1 <= wday(connectTime, label = FALSE) & 5 >= wday(connectTime, label = FALSE)
  is_weekend <- wday(connectTime, label = FALSE) == 6 | wday(connectTime, label = FALSE) == 7 
  
  # time requirements to determine if the datapoint connected at opening
  before_810 <- hour(connectTime) == 8 & minute(connectTime) <= 10
  at_7 <- hour(connectTime) == 7
  before_910 <- hour(connectTime) == 9 & minute(connectTime) <= 10
  at_8 <- hour(connectTime) == 8
  
  # time requirements for weekdays and weekends
  weekday_either <- before_810 | at_7
  weekend_either <- before_910 | at_8
  
  # determine connections at opening
  weekday_connection <- weekday_either & is_weekday
  weekend_connection <- weekend_either & is_weekend
  
  connection <- weekday_connection | weekend_connection
  
  return(connection)
}