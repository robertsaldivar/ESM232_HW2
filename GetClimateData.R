---
  title: "almond yield"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```


## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
  
  ```{r start}
#Setup
source("General_Yield.R") #the function

source("GetClimateData.R") #used to make testing for multiple crops easier



```

```{r clim}
####NEED TO CHANGE THIS EACH TIME YOU CHANGE THE CROP ->
#estimate table grape yield anomalies from clim.txt climate data

crop_est = "Avocado"

#get the data we care about - for now that's the February minimum temperature and the (total) January precipitation for each year

#the last value in the dataset is for 2010-06-07
#years are 1988 to 2010, but 1988 has no data for Jan or Feb


#enter the index (1 = 1988, 23 = 2010) of the year to start and end plotting in. Must be the years as they are to be plotted!
startyearindex = 3
endyearindex = 21 #avocado is missing data in the last year for one


data = GetClimateData("clim.txt",crop_est) #get the climate data

#actually run the model
#user needs to be smart about telling what index to use for each data type
#in particular with the previous year's XYZ stuff
result_clim = Crop_Yield(min_temp = data$mintemp[startyearindex:endyearindex,1],precip = data$precip[(startyearindex-1):(endyearindex-1),1], max_temp = data$maxtemp[(startyearindex-1):(endyearindex-1)])
####<- NEED TO CHANGE THIS EACH TIME YOU CHANGE THE CROP


#restructure to make it plottable
result_clim
result_clim_plottable = data.frame(data$years[startyearindex:endyearindex],result_clim$yield) #this is why the start and end year indices need to be for the years as they get plotted!
colnames(result_clim_plottable) = c("year","yield")




#graph the results

ggplot(result_clim_plottable,aes(year,yield, fill = yield))+
  geom_point()+
  labs(x = "Year", y = "Yield Anomaly (tons/acre)", title = "Yearly modeled yield anomaly, for default data") +
  theme_bw()


```

```{r clim +2}
#only wants for if the last 20 years had been 2 deg C warmer, but there are 22 years with data in the dataset

#increase the min temp value for the last 20 years in the dataset by 2:
data2 = data



####NEED TO CHANGE THIS EACH TIME YOU CHANGE THE CROP ->

#these numbers will depend on the crop you're actually doing!
#it should add 2 to the temperature data for the last 20 years, ending in 2010
data2$mintemp[4:23,1] = sapply(data$mintemp[4:23,1],function(input_){newdata = input_ + 2; return(newdata)})

if(exists("data$maxtemp")){
  data2$maxtemp[4:22] = sapply(data$maxtemp[4:22],function(input_){newdata = input_ + 2; return(newdata)})
}

##ONLY NEEDED IF THERE IS A 2ND COLUMN OF MINTEMP DATA:
#data2$mintemp[4:22,2] = sapply(data$mintemp[4:22,2],function(input_){newdata = input_ + 2; return(newdata)})



#actually run the model:
result_clim2 = Crop_Yield(min_temp = data2$mintemp[startyearindex:endyearindex,1],precip = data2$precip[(startyearindex-1):(endyearindex-1),1], max_temp = data2$maxtemp[(startyearindex-1):(endyearindex-1)])
####<- NEED TO CHANGE THIS EACH TIME YOU CHANGE THE CROP



#graph the results

result_clim2
result_clim2_plottable = data.frame(data2$years[startyearindex:endyearindex],result_clim2$yield) #makes this a plottable dataframe; perhaps the function should do this if provided with a vector of the years?
colnames(result_clim2_plottable) = c("year","yield")



ggplot(result_clim2_plottable,aes(year,yield, fill = yield))+
  geom_point()+
  labs(x = "Year", y = "Yield Anomaly (tons/acre)", title = "Yearly modeled yield anomaly, for 2 degrees C warmer min temperature \nfor the last 20 years of data") +
  theme_bw()

```

``` {r clim +2 show mean}
#what this part actually wants is the MEAN annual yield anomaly under these conditions
#so we will display the means for each run

c("+2 deg C","Default")
c(result_clim2$meanyield,result_clim$meanyield) #they are also different


```


``` {r param vary}
#Vary one of the parameter by sampling from a normal distribution with mean the value from Lobell et al., (2006) and standard deviation 10% of the mean -  create a box plot of  mean annual yield anomaly for baseline and 2C climate (where boxes show variation due to uncertanty in the parameter)

#ultimately should be creating a boxplot with 2 boxes: baseline case and +2 deg C case

#which parameter should we vary?
#inputs are what it's a function of (min temp and precip); parameters are all the other stuff

#let's vary the first coefficient (3.25 for avocado in -0.015Tn,2)
var_times = 50


####NEED TO CHANGE THIS EACH TIME YOU CHANGE THE CROP ->
param_vary = rnorm(mean=3.25,sd = 0.325, n=var_times) #need to include the sign if it's negative!

#now we want to run the model for each parameter value, for both climate conditions
#however we specifically want the mean anomaly

#default
result_pvary = sapply(param_vary,function(f) {Crop_Yield(min_temp = data$mintemp[startyearindex:endyearindex,1],precip = data$precip[(startyearindex-1):(endyearindex-1),1], max_temp = data$maxtemp[(startyearindex-1):(endyearindex-1)],min_temp_coeff = f)})[4,] #we only want the timeseries from the model for each of the 50 runs

#plus 2 deg C
result_pvary2 = sapply(param_vary,function(f) {Crop_Yield(min_temp = data2$mintemp[startyearindex:endyearindex,1],precip = data2$precip[(startyearindex-1):(endyearindex-1),1], max_temp = data2$maxtemp[(startyearindex-1):(endyearindex-1)],min_temp_coeff = f)})[4,] #we only want the timeseries from the model for each of the 50 runs

####<- NEED TO CHANGE THIS EACH TIME YOU CHANGE THE CROP

#get just the numbers
result_pvary = unname(unlist(result_pvary))
result_pvary2 = unname(unlist(result_pvary2))

result_pvary = append(result_pvary,result_pvary2)

#create a data frame marking each number as being either default or plus2c
categ = c("Default")
categ[1:50] = c("Default")
categ[51:100] = c("+ 2 deg C")

result_pvary_plottable = data.frame(categ,result_pvary)

colnames(result_pvary_plottable) = c("Category","Mean_Anomaly")

ggplot(result_pvary_plottable,aes(Category,Mean_Anomaly))+
  geom_boxplot()+
  labs(x = " ",y = "Mean Annual Yield Anomaly (tons/acre)", title = "Variation in mean yield anomaly for default and elevated temperature,\nwith variation in the linear temperature coefficient") +
  theme_bw()


```



