
tsibble_cansim <- function(vector, ... , name = NA){
  
  # load required packages
  require(cansim)
  require(lubridate)
  require(tsibble)
  
  xx <- get_cansim_vector(vector, ...)
 
  time <- as.Date(xx$REF_DATE)
  series <- xx$VALUE  
  
  ## Determine Frequency
  
  time.diff <- time[2] - time[1]
  
  # weekly frequency
  if (time.diff == 7){
    time <- yearweek(time)
  }
  
  # monthly frequency 
  if (25 < time.diff & time.diff <= 33){
    time <- yearmonth(time) 
  }
  
  # quarterly frequency
  if (70 < time.diff & time.diff <= 100){
    time <- yearquarter(time)
  }
  
  # yearly frequency
  if (300 < time.diff & time.diff <= 400){
   time <- year(time)
  }
  
  # Create tsibble
  
  x <- tsibble(date = time, series = series, index = date)
  if(!is.na(name)){
    names(x)[2] <- name
  }
    
  return(x)  
}   
    
# usage:

# x <- tsibble_cansim("v65201210", name = "GDP", start_time = as.Date("2010-01-01"))
# x <- tsibble_cansim("v65201210")