#' Almond Yield Function
#' Returns the following (in a list, in this order)
#' [1] The yield anomaly (in tons / acre) for each year of input data, as a vector in the same order as the input data
#' [2] The maximum yield anomaly value
#' [3] The minimum yield anomaly value
#'
#' @param min_temp the minimum recorded temperature
#' @param precip the amount of precipitation
#' @param min_temp_coeff the first coefficient of the minimum temperature variable
#' @param min_temp_coeff2 the coefficent of the minimum temperature variable when the varible is squared
#' @param precip_coeff the first coefficient of the precipitation variable
#' @param precip_coeff2 the coefficent of the precipitation variable when the varible is squared
#' @param intercept The intercept of the equation to find the yield anomaly as described in Lobell et al. 2006
#' @return the yeild anomaly as a list, the maximum yield anomaly and minimum yield anomaly values.
#' @references Lobell et al. 2006
#' @author Trevor Romich
#' @author Robert Saldivar
#' 
#'
#'TODO:
#'
#'If time, adjust so that it can work for any of the crops - this will require either:
#'	The function itself correctly gets the data from the right month depending on the crop
#'	The user is required to sort the data in advance so that it is fed the correct monthly data for the desired crop

Almond_Yield <- function(min_temp, precip, min_temp_coeff = -0.015, min_temp_coeff2 = -0.0046, precip_coeff = -0.07, precip_coeff2 = 0.0043, intercept = 0.28) {
  
  #Adding error checking to the function
  #make sure the temperature is within a possible range
  
  min_temp = ifelse((min_temp > 57), return("minimum temperature cannot be above 57 degrees celsius. Check units."), min_temp)
  
  min_temp = ifelse((min_temp < -90), return("Minimum temperature cannot be below -90 degrees celsius. Check units."), min_temp)
  
  #make sure the precipitation is within a possible range
  
  precip = ifelse((precip < 0), return("Precipitation cannot be less than 0"), precip)
  
  yield = min_temp_coeff*min_temp + min_temp_coeff2*(min_temp)^2 + precip_coeff*precip + precip_coeff2*(precip)^2 + intercept
  
  highest_yield = max(yield)
  lowest_yield = min(yield)
  
  mean_yield = mean(yield)
  
  
  results = list(yield = yield, maxyield = highest_yield, minyield = lowest_yield, meanyield = mean_yield)
  
  
  
  
  
  return(results)
  
}
