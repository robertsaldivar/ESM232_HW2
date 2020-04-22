#' Crop Yield Function
#' Returns the following (in a list, in this order)
#' [1] The yield anomaly (in tons / acre) for each year of input data, as a vector in the same order as the input data
#' [2] The maximum yield anomaly value
#' [3] The minimum yield anomaly value
#' [4] The mean yield anomaly across all years in the input data
#'
#' User can manually enter coefficients, which are numbered in the order they appear in the equation (i.e. min_temp_coeff2
#' is the 2nd min temp coefficient from the left)
#'
#' Default coeff values are for Almonds. To use other values, change the input value of the coefficients when calling.
#'
#' To manually enter coefficients for the other crops, change the input use_defaults to something other than "y."
#' If you do this you must enter values for ALL the coefficients that crop uses.
#'
#' The user is responsible for ensuring that input data is formatted correctly. In particular, for crops that require some
#' of the climate data to be from the previous year, the user must ensure that these values have the previous year's data
#' associated with each year.

library(tidyverse)

Crop_Yield <- function(min_temp, precip, crop = "Almond", use_defaults = "y", min_temp_coeff = -0.015, min_temp_coeff2 = -0.0046, precip_coeff = -0.07, precip_coeff2 = 0.0043, intercept = 0.28, min_temp_coeff3 = 0, min_temp_coeff4 = 0, precip_coeff3 = 0, precip_coeff4 = 0, max_temp_coeff = 0, max_temp_coeff2 = 0) {
  
  #Adding error checking to the function
  #make sure the temperature is within a possible range
  
  min_temp = ifelse((min_temp > 57), return("minimum temperature cannot be above 57 degrees celsius. Check units."), min_temp)
  
  min_temp = ifelse((min_temp < -90), return("Minimum temperature cannot be below -90 degrees celsius. Check units."), min_temp)
  
  #make sure the precipitation is within a possible range
  
  precip = ifelse((precip < 0), return("Precipitation cannot be less than 0"), precip)
  
  
  if(crop == "Almond") {
    yield = min_temp_coeff*min_temp + min_temp_coeff2*(min_temp)^2 + precip_coeff*precip + precip_coeff2*(precip)^2 + intercept
  }
  
  if(crop == "Orange") {
    if(use_defaults == "Yes" | use_defaults == "yes" | use_defaults == "y" | use_defaults == "Y") {
      min_temp_coeff = 1.08
      min_temp_coeff2 = -0.20
      precip_coeff = 4.99
      precip_coeff2 = -1.97
      intercept = -2.47
    }
    yield = min_temp_coeff*min_temp + min_temp_coeff2*min_temp^2 + precip_coeff*precip + precip_coeff2*precip^2 + intercept
  }
  
  if(crop == "Walnut") {
    if(use_defaults == "Yes" | use_defaults == "yes" | use_defaults == "y" | use_defaults == "Y") {
      precip_coeff = 0.038
      precip_coeff2 = -0.0051
      intercept = -5.83
      max_temp_coeff = 0.68
      max_temp_coeff2 = -0.020
    }
    yield = max_temp_coeff*max_temp + max_temp_coeff2*max_temp^2 + precip_coeff*precip + precip_coeff2*precip^2 + intercept
  }
  if(crop == "Avocado") {
    if(use_defaults == "Yes" | use_defaults == "yes" | use_defaults == "y" | use_defaults == "Y") {
      min_temp_coeff = 3.25
      min_temp_coeff2 = -0.14
      precip_coeff = 1.00
      precip_coeff2 = -0.31
      intercept = -288.09
      max_temp_coeff = 17.71
      max_temp_coeff2 = -0.29
    }
    yield = max_temp_coeff*max_temp + max_temp_coeff2*max_temp^2 + min_temp_coeff*min_temp + min_temp_coeff2*min_temp^2 + precip_coeff*precip + precip_coeff2*precip^2 + intercept
  }
  if(crop == "Wine Grapes") {
    if(use_defaults == "Yes" | use_defaults == "yes" | use_defaults == "y" | use_defaults == "Y") {
      min_temp_coeff = 2.65
      min_temp_coeff2 = -0.177
      precip_coeff = 4.78
      precip_coeff2 = -4.93
      precip_coeff3 = -2.24
      precip_coeff4 = 1.54
      intercept = -10.50
    }
    yield = min_temp_coeff*min_temp + min_temp_coeff2*min_temp^2 + precip_coeff*precip + precip_coeff2*precip^2 + precip_coeff3*precip + precip_coeff4*precip^2 + intercept  
  }
  if(crop == "Table Grapes") {
    if(use_defaults == "Yes" | use_defaults == "yes" | use_defaults == "y" | use_defaults == "Y") {
     min_temp_coeff = 6.93
     min_temp_coeff2 = - 0.19
     min_temp_coeff3 = 2.61
     min_temp_coeff4 = -0.15
     precip_coeff = 0.035
     precip_coeff2 = 0.024
     precip_coeff3 = 1.71
     precip_coeff4 = -0.673
     intercept = -73.89
    }
    yield = min_temp_coeff*min_temp + min_temp_coeff2*min_temp^2 + min_temp_coeff3*min_temp + min_temp_coeff4*min_temp^2 + precip_coeff*precip + precip_coeff2*precip^2 + precip_coeff3*precip + precip_coeff4*precip^2 + intercept
  }
  
  highest_yield = max(yield)
  lowest_yield = min(yield)
  
  mean_yield = mean(yield)
  
  
  results = list(yield = yield, maxyield = highest_yield, minyield = lowest_yield, meanyield = mean_yield)
  
  
  
  
  
  return(results)
  
}


