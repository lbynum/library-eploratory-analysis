library(reshape2)

create_disclose_barchart <- function(disclose_dow) {
  disclose_dow$total_disconnects <- NULL
  disclose_dow$`wday(start_datetime)` <- NULL
  
  disclose_dow_melt <- melt(disclose_dow, id.vars = "connect_dow")
  
  disclose_dow_melt$connect_dow <- 
    factor(disclose_dow_melt$connect_dow, levels = 
             c("Monday", "Tuesday", "Wednesday", "Thursday", 
               "Friday", "Saturday", "Sunday"))
  
  disclose_barchart <- ggplot(disclose_dow_melt, aes(x = connect_dow, y = value, fill=variable)) + 
    labs(x = "Connect day of week", y = "Disconnections at closing", title = "Disconnections at closing bar chart for 2016") +
    geom_bar(stat="identity") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  disclose_barchart
}

create_conopen_barchart <- function(conopen_dow) {
  conopen_dow$total_connects <- NULL
  conopen_dow$`wday(start_datetime)` <- NULL
  
  conopen_dow_melt <- melt(conopen_dow, id.vars = "connect_dow")
  
  conopen_dow_melt$connect_dow <- 
    factor(conopen_dow_melt$connect_dow, levels = 
             c("Monday", "Tuesday", "Wednesday", "Thursday", 
               "Friday", "Saturday", "Sunday"))
  
  conopen_barchart <- ggplot(conopen_dow_melt, aes(x = connect_dow, y = value,fill=variable)) + 
    geom_bar(stat='identity') + 
    labs(x = "Connect day of week", y = "Connections at opening", title = "Connections at opening bar chart for 2016") +
    theme(plot.title = element_text(hjust = 0.5))
  
  conopen_barchart
}

create_disclose_hist <- function(disclose_dates) {
  disclose_hist <- ggplot(disclose_dates, aes(connect_date, total_disconnections)) +
    geom_histogram(stat = "identity", fill = "blue") + 
    scale_x_date(date_labels = "%m-%d", date_breaks = "2 weeks") +
    labs(x = "Connect date", y = "Disconnections at closing", title = "Disconnections at closing histogram for 2016") +
    theme(plot.title = element_text(hjust = 0.5))
  disclose_hist
}

create_disclose_plot <- function(disclose_dates) {
  disclose_plot <- ggplot(disclose_dates, aes(connect_date, total_disconnections)) + 
    geom_point(col = "blue") + scale_x_date(date_labels = "%m-%d", date_breaks = "2 weeks") + 
    labs(x = "Connect date", y = "Disconnections at closing", title = "Disconnections at closing plot for 2016") + 
    theme(plot.title = element_text(hjust = 0.5))
}

create_conopen_hist <- function(conopen_dates) {
  conopen_hist <- ggplot(conopen_dates, aes(connect_date, total_connections)) + 
    geom_histogram(stat = "identity", fill = "blue") + 
    scale_x_date(date_labels = "%m-%d", date_breaks = "2 weeks") + 
    labs(x = "Connect date", y = "Connections at opening", title = "Connections at opening histogram for 2016") + 
    theme(plot.title = element_text(hjust = 0.5))
}

create_conopen_plot <- function(conopen_dates) {
  conopen_plot <- ggplot(conopen_dates, aes(connect_date, total_connections)) + 
    geom_point(col = "blue") + scale_x_date(date_labels = "%m-%d", date_breaks = "2 weeks") + 
    labs(x = "Connect date", y = "Connections at opening", title = "Connections at opening plot for 2016") +
    theme(plot.title = element_text(hjust = 0.5))
}
