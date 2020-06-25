---
title: "Blue Book for Bulldozers Kaggle Competition"
output:
   prettydoc::html_pretty:
    keep_md: true
    theme: architect
---

In this notebook I work through the Blue Book for Bulldozers dataset.

Data for and information on this Kaggle competition can be found [here.](https://www.kaggle.com/c/bluebook-for-bulldozers/overview)


```r
library(tidyverse)
library(lubridate)
library(visdat)
library(randomForest)
library(caret)
library(e1071)
```
Read in the data and inspect the various data types. As the saldedate variable is represented as a string, we convert it to date-time format using lubridates mdy_hm.  

```r
PATH = "data/bulldozers/"

data_raw = read_csv(str_c(PATH, "Train.csv"))
data_raw$saledate = mdy_hm(data_raw$saledate)
```
It is apparent that there may be a lot of missing values in our data. To visualize the extent of these missing values we will use vis_miss, then display the proportion of missing values for each column.

```r
require(scales)
vis_miss(data_raw, show_perc_col = FALSE, warn_large_data = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank()) + 
   scale_y_continuous(labels = comma)
```

![](bulldozers_files/figure-html/Missing Values Viz-1.png)<!-- -->


```r
na_prop = sort(round(colMeans(is.na(data_raw)), 2), decreasing = TRUE)

max_str = max(str_length(names(na_prop)))

for (nm in names(na_prop)) {
    cat(
      str_c(str_pad(str_c(nm, ":"), max_str + 1L, side = "right"),
            na_prop[[nm]],
            sep = " "),
      "\n")
}
```

```
## Blade_Extension:          0.94 
## Blade_Width:              0.94 
## Enclosure_Type:           0.94 
## Engine_Horsepower:        0.94 
## Pushblock:                0.94 
## Scarifier:                0.94 
## Tip_Control:              0.94 
## Coupler_System:           0.89 
## Grouser_Tracks:           0.89 
## Hydraulics_Flow:          0.89 
## fiModelSeries:            0.86 
## UsageBand:                0.83 
## Differential_Type:        0.83 
## Steering_Controls:        0.83 
## fiModelDescriptor:        0.82 
## Pad_Type:                 0.8 
## Stick:                    0.8 
## Turbocharged:             0.8 
## Backhoe_Mounting:         0.8 
## Blade_Type:               0.8 
## Travel_Controls:          0.8 
## Tire_Size:                0.76 
## Track_Type:               0.75 
## Undercarriage_Pad_Width:  0.75 
## Stick_Length:             0.75 
## Thumb:                    0.75 
## Pattern_Changer:          0.75 
## Grouser_Type:             0.75 
## Drive_System:             0.74 
## Ripper:                   0.74 
## MachineHoursCurrentMeter: 0.64 
## Ride_Control:             0.63 
## Transmission:             0.54 
## ProductSize:              0.53 
## Forks:                    0.52 
## Coupler:                  0.47 
## fiSecondaryDesc:          0.34 
## Hydraulics:               0.2 
## auctioneerID:             0.05 
## SalesID:                  0 
## SalePrice:                0 
## MachineID:                0 
## ModelID:                  0 
## datasource:               0 
## YearMade:                 0 
## saledate:                 0 
## fiModelDesc:              0 
## fiBaseModel:              0 
## fiProductClassDesc:       0 
## state:                    0 
## ProductGroup:             0 
## ProductGroupDesc:         0 
## Enclosure:                0
```



```r
data_raw %>%
  select_if(is.numeric) %>%
  summary()
```

```
##     SalesID          SalePrice        MachineID          ModelID     
##  Min.   :1139246   Min.   :  4750   Min.   :      0   Min.   :   28  
##  1st Qu.:1418371   1st Qu.: 14500   1st Qu.:1088697   1st Qu.: 3259  
##  Median :1639422   Median : 24000   Median :1279490   Median : 4604  
##  Mean   :1919713   Mean   : 31100   Mean   :1217903   Mean   : 6890  
##  3rd Qu.:2242707   3rd Qu.: 40000   3rd Qu.:1468067   3rd Qu.: 8724  
##  Max.   :6333342   Max.   :142000   Max.   :2486330   Max.   :37198  
##                                                                      
##    datasource     auctioneerID       YearMade    MachineHoursCurrentMeter
##  Min.   :121.0   Min.   : 0.000   Min.   :1000   Min.   :      0         
##  1st Qu.:132.0   1st Qu.: 1.000   1st Qu.:1985   1st Qu.:      0         
##  Median :132.0   Median : 2.000   Median :1995   Median :      0         
##  Mean   :134.7   Mean   : 6.556   Mean   :1899   Mean   :   3458         
##  3rd Qu.:136.0   3rd Qu.: 4.000   3rd Qu.:2000   3rd Qu.:   3025         
##  Max.   :172.0   Max.   :99.000   Max.   :2013   Max.   :2483300         
##                  NA's   :20136                   NA's   :258360
```

Upon inspection of the YearMade column, we can see there are some suspicious results. A minimum YearMade of 1000, and a mean YearMade of 1899. We will explore this further by plotting a histogram of YearMade.


```r
ggplot(data_raw, aes(YearMade)) +
  geom_histogram(fill = "#63A0E1") +
   scale_y_continuous(labels = comma)
```

![](bulldozers_files/figure-html/YearMade plot-1.png)<!-- -->




It appears that missing values for YearMade in this data set have been assigned the year 1000. We will set these values to NA and deal with them later.


```r
data_raw$YearMade[data_raw$YearMade < 1900] = NA
ggplot(data_raw, aes(YearMade)) +
  geom_histogram(fill = "#63A0E1", binwidth = 1) +
   scale_y_continuous(labels = comma)
```

![](bulldozers_files/figure-html/Fix YearMade-1.png)<!-- -->

```r
data_raw %>%
  select_if(is.numeric) %>%
  summary()
```

```
##     SalesID          SalePrice        MachineID          ModelID     
##  Min.   :1139246   Min.   :  4750   Min.   :      0   Min.   :   28  
##  1st Qu.:1418371   1st Qu.: 14500   1st Qu.:1088697   1st Qu.: 3259  
##  Median :1639422   Median : 24000   Median :1279490   Median : 4604  
##  Mean   :1919713   Mean   : 31100   Mean   :1217903   Mean   : 6890  
##  3rd Qu.:2242707   3rd Qu.: 40000   3rd Qu.:1468067   3rd Qu.: 8724  
##  Max.   :6333342   Max.   :142000   Max.   :2486330   Max.   :37198  
##                                                                      
##    datasource     auctioneerID       YearMade     MachineHoursCurrentMeter
##  Min.   :121.0   Min.   : 0.000   Min.   :1919    Min.   :      0         
##  1st Qu.:132.0   1st Qu.: 1.000   1st Qu.:1988    1st Qu.:      0         
##  Median :132.0   Median : 2.000   Median :1996    Median :      0         
##  Mean   :134.7   Mean   : 6.556   Mean   :1994    Mean   :   3458         
##  3rd Qu.:136.0   3rd Qu.: 4.000   3rd Qu.:2001    3rd Qu.:   3025         
##  Max.   :172.0   Max.   :99.000   Max.   :2013    Max.   :2483300         
##                  NA's   :20136    NA's   :38185   NA's   :258360
```
A minimum YearMade of 1919 and a mean YearMade of 1994 seem much more reasonable. We now perform feature extraction on the sale date, label encode the categorical variables, and address the missing continuous values in the data set. Missing continuous values will be replaced with the median value of that feature, and a new feature will be created to indicate which values were initially missing.



```r
data_raw_2 = data_raw %>%
  mutate("Sale_Year" = year(saledate), "Sale_Month" = month(saledate), 
         "Sale_Week" = week(saledate), "Sale_yday" = yday(saledate), 
         "Sale_mday" = mday(saledate), "Sale_wday" = wday(saledate),
         "Sale_Quarter" = quarter(saledate), "Sale_qday" = qday(saledate))
```



```r
label_encoder = function(df) {
  for (nm in names(df)) {
    if (typeof(df[[nm]]) == "character") {
      df[[nm]][is.na(df[[nm]])] = "NA"
    }
  }
  for (nm in names(df)) {
    if (typeof(df[[nm]]) == "character") {
      df[[nm]] = as.numeric(as.factor(df[[nm]]))
    }
  }
  return(df)
}

data_raw_3 = label_encoder(data_raw_2)
```


```r
data_raw_4 = data_raw_3 %>%
  mutate("auctioneerID_NA" = as.numeric(is.na(data_raw_3$auctioneerID)), 
         "Year_Made_NA" = as.numeric(is.na(data_raw_3$YearMade)),
         "Machine__hours_NA" = as.numeric(is.na(data_raw_3$MachineHoursCurrentMeter))) %>%
  mutate("auctioneerID" = replace(auctioneerID,
                                is.na(auctioneerID),
                                median(auctioneerID, na.rm = TRUE)),
         "YearMade" = replace(YearMade,
                                is.na(YearMade),
                                median(YearMade, na.rm = TRUE)),
         "MachineHoursCurrentMeter" = replace(MachineHoursCurrentMeter,
                                is.na(MachineHoursCurrentMeter),
                                median(MachineHoursCurrentMeter, na.rm = TRUE)))

cat(str_c("There are ", as.character(sum(is.na(data_raw_4))), " missing values remaining."))
```

```
## There are 0 missing values remaining.
```
We split the data into a train and validation set. As we are predicting future sales, the validation set must be comprised of examples of which the sale date is more recent than any example found in the training set. If we randomly select a validation set, then the accuracy on this validation set will not be indicative of the accuracies we may get on future examples, as the validation set will have sale dates that the model has already been trained on.

It is very important that the validation set and the test set come from the same distribution of data, in this case that distribution is a range of dates. We then split off the independent variable, and take its log, as the evaluation metric for this competition is the RMSLE (root mean squared log error) between the actual and predicted auction prices. We will therefore use this metric to score our model.

```r
n_valid = 20000
n_train = nrow(data_raw_4) - n_valid

train = data_raw_4 %>%
  arrange(saledate) %>%
  head(n_train)
  
valid = data_raw_4 %>%
  arrange(saledate) %>%
  tail(n_valid)

features_train = train %>%
  select(-SalePrice)

labels_train = train %>%
  select(SalePrice) %>%
  log()

features_valid = valid %>%
  select(-SalePrice)

labels_valid = valid %>%
  select(SalePrice) %>%
  log()
```


```r
# model = randomForest(SalePrice~., data = train, ntree = 150, mtry = 6, importance = TRUE)
```


```r
# saveRDS(model, file = "model.Rdata")
```


```r
model_load = readRDS("model.Rdata")
```



```r
preds = predict(model_load, valid)
```



```r
cat(str_c("The error on the validation set is " , as.character(format(RMSE(log(preds), 
                                   unlist(labels_valid)),digits = 3, format = "f"))))
```

```
## The error on the validation set is 0.281
```

A RMSLE of 0.281 places us in the top 16% of the leaderboard for this competition. 
