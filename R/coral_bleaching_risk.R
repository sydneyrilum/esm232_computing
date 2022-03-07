#' Risk of Coral Bleaching 
#'
#' Corals are very sensitive to warm temperatures and when they experience heat stress, they're vulnerable to bleaching. This function computes level of risk for coral bleaching based on sea surface temperature and other stress parameters of salinity and water turbidity/clarity. 
#' 
#' @param temp sea surface temperature (C)
#' @param temp_threshold sea surface temperature (SST) threshold for corals (C) (default 30 C)
#' @param salinity of seawater (ppt)
#' @param salinity_threshold_low of seawater (ppt) (default 32 ppt)
#' @param salinity_threshold_high of seawater (ppt) (default 42 ppt)
#' @param turbidity of seawater, i.e. water clarity (NTU)
#' @param turbidity_threshold of seawater, i.e. water clarity (NTU) (default 5 NTU)
#' @return risk (low, medium, high, very high) of coral bleaching
#' @return number_of_extremes, number of extreme temperature events
#' @author Sydney Rilum

coral_bleaching_risk = function(temp, temp_threshold = 30,
                                salinity, salinity_threshold_low = 32, salinity_threshold_high = 42,
                                turbidity, turbidity_threshold = 5) {
 
   # error checking
  if (length(temp) < 5)
    return("Not enough temperature measurements, at least 5 are needed")
  
  if (length(salinity) < 5)
    return("Not enough salinity measurements, at least 5 are needed")
  
  if (length(turbidity) < 5)
    return("Not enough turbidity measurements, at least 5 are needed")
  
    
  # while loop, end looping any time we get more than 5 days with SST greater than the threshold temp
  num_days = 0
  i = 1
  
  while ( (num_days < 5) && (i <= length(temp))) {
    if (temp[i] > temp_threshold)
      # we have another day with SST greater than temp threshold, add another day to num_days
      num_days = num_days + 1
    else
      # we have to start over
      num_days = 0
    # increment our counter with while loops
    i = i + 1
  }
  
  # compute the mean salinity & turbidity
  mean_salinity = mean(salinity)
  mean_turbidity = mean(turbidity)
  
  # only high or med bleaching risk if temperature has been 30 C for more than 5 days in a row
  if (num_days >= 5) {
    risk = case_when (mean_salinity < salinity_threshold_low ~ "low",
                      mean_salinity >= salinity_threshold_low &
                        mean_salinity < salinity_threshold_high ~ "medium",
                      mean_salinity >= salinity_threshold_high ~ "high",
                      mean_salinity >= salinity_threshold_high &
                        mean_turbidity >= turbidity_threshold ~ "very high")
    # otherwise risk of bleaching is low
  } else
    risk = "low"  
    
  return(list(risk = risk, 
              number_of_extremes = num_days))
}
