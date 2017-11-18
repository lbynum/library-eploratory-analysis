library(lubridate)
library(dplyr)

preprocess_ezproxy_data <- function(x) {
  ezproxy_data_path <- 'HMC\ Math\ 158/ezproxy/'
  
  # read in all files
  filelist = paste0(ezproxy_data_path, list.files(ezproxy_data_path, '*.tsv'))
  is_a_ccl_file <- endsWith(filelist, 'hashed.tsv')
  ccl_files <- filelist[is_a_ccl_file]
  spu_files <- filelist[!is_a_ccl_file]
  spu <- lapply(spu_files, function(x) read.delim(x, header = TRUE, sep = '\t'))
  ccl <- lapply(ccl_files, function(x) read.delim(x, header = TRUE, sep = '\t'))
  
  # concatenate files while aligning columns properly
  spu <- do.call(rbind, 
                 lapply(spu, function(x) x[match(names(spu[[1]]), names(x))]))
  ccl <- do.call(rbind, 
                 lapply(ccl, function(x) x[match(names(ccl[[1]]), names(x))]))
  
  # fix ccl datetime column (split into two columns and convert to date obj.)
  ccl$session_start <- substr(ccl$datetime, start = 1, stop = 19)
  ccl$session_end <- substr(ccl$datetime, start = 21, stop = 40)
  ccl <- ccl %>% select(-datetime)
  ccl$session_duration_mins <- difftime(ymd_hms(ccl$session_end), 
                                   ymd_hms(ccl$session_start), 
                                   units = 'mins') %>% as.numeric()
  
  # join by session
  ezproxy <- inner_join(spu, ccl, by = 'session')

  return(ezproxy)
}
