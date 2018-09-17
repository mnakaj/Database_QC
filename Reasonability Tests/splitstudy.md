---
title: "Split studies"
author: "Mia Nakajima"
date: "9/9/2018"
output: 
  rmarkdown::html_document:
    theme: paper
    keep_md: true
---







```r
#read in data 
df <- read.csv(here("Database-II_20180702.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
```

#### Split database II by study so we can check each study for reasonability test to check if reasonability test is working as expected. 


```r
# We want to split the data frame into 39 different data frames based on contributor. 
names <- unique(df$`Data contributor`)
```


```r
for (i in 1:39){
  df_split <- df[which(df$`Data contributor` == names[i]),]
  label <- paste("name", as.character(i), sep = "")
  assign(label, df_split)
}
```

#### Check function

##### Function returns of the 39 studies, which functions would not pass the reasonability test. 


```r
check_studies <- function(func){
  outputs <- c()
  frames <- c()
  for(i in 1:39){
    label <- paste("name", as.character(i), sep = "")
    value <- func(eval(parse(text = label)))
    
    outputs <- c(outputs, value )
    if(!(is.na(value)) && value == FALSE){
      frames <- c(frames, unique((eval(parse(text = label)))$`Data contributor`))
    }
  }
  total <- length(which(outputs))
  applicable_studies <- length( which(!(is.na(outputs))))
  
  init <- paste(paste(as.character(100*total/applicable_studies), "%", sep = ""), "passing.", "Out of", as.character(applicable_studies), "applicable studies. Studies with non passing:", "\n")
  
  str <- paste(frames, collapse = "\n")
  
  cat(paste(init, str))
}
```



#### Thermal sensation 


```r
thermal_sensation_R <- function(df){
  ### Test reasonability of thermal sensation votes. 
  ### Takes linear relationship between sensation and tempearture to determine reasonability.
  
  #take the average sensation at each air temperature 
  avg <- df %>% 
    group_by(`Air temperature (°F)`) %>% 
    summarise(averagetemp = mean(`Thermal sensation`, na.rm = T)) %>%
    as.data.frame
  
  #condition for linear model to be fit. 
  if(length(avg$`Air temperature (°F)`) > 1 && all(is.na(avg$averagetemp)) == FALSE && all(is.na(avg$`Air temperature (°F)`) == FALSE)){
    fit <- lm(averagetemp~`Air temperature (°F)`, data = avg)
    #want slope to be positive. As temperature increases, thermal sensation value should increase. 
    if (fit$coefficients[2]>0){
      
      
      return(T)
    }else{
      return(F)
    }
  }else{
      return(NA)
    }
}
```


```r
check_studies(thermal_sensation_R)
```

```
## 100% passing. Out of 26 applicable studies. Studies with non passing: 
## 
```

#### Thermal acceptibility


```r
test_thermal_acceptibility_R <- function(df){
  ### Tests reasonability of thermal acceptability votes
  ### Returns true if the number of votes in "acceptable" or "1" is quadratic (concave down)
  ### with thermal sensation. 
  
  #count the number of votes as acceptable or unacceptable for each thermal sensation 
  accept <- count(df, vars = `Thermal sensation`, wt_var = `Thermal sensation acceptability`)
  
  #filter to a dataframe that has votes for acceptable
  accept_1 <- accept %>% 
    filter(wt_var == 1)
  if(all(is.na(accept_1$vars)) == FALSE && all(is.na(accept_1$n)) == FALSE ){
  #fit a function that is quadratic with votes for acceptable
  fit <- lm(n ~ I(vars^2), data = accept_1)
  
  if(fit$coefficients[2] < 0){
    return(T)
  }else{
    return(F)
  }
  } else{
    return(NA)
  }
  
}
```


```r
check_studies(test_thermal_acceptibility_R)
```

```
## 100% passing. Out of 20 applicable studies. Studies with non passing: 
## 
```

#### Thermal preference 


```r
test_thermal_preference_R <- function(df){
  ### Tests reasonability of thermal preference votes (quick check)
  ### Returns true if proportion of people who want warmer decrease with increase in thermal sensation 
  ### and if proportion of people who want cooler increase with thermal sensation. 
  
  #count each thermal preference in each thermal sensation bin
  
  #remove NA values 
  sensation <- df$`Thermal sensation`[which(!is.na(df$`Thermal preference`))]
  pref <- df$`Thermal preference`[which(!is.na(df$`Thermal preference`))]

  df <- data.frame(sensation, pref)

  
  pref <- count(df, vars = `sensation`, wt_vars = pref)
  
  #filter to find proportions in each thermal sensation bin that voted "cooler"
  cooler <- pref %>% 
    group_by(vars) %>%
    mutate(total = sum(n, na.rm = T)) %>% 
    mutate(prop = n/total) %>%
    filter(wt_vars == "cooler")
 # print(cooler)
  #filter to find proportions in each thermal sensation bin that voted "warmer"
  warmer <- pref %>% 
    group_by(vars) %>%
    mutate(total = sum(n, na.rm = T)) %>% 
    mutate(prop = n/total) %>%
    filter(wt_vars == "warmer")
  #print(warmer)
  if(all(is.na(cooler$prop)) == F && all(is.na(cooler$vars)) == F && all(is.na(warmer$prop)) == F && all(is.na(warmer$vars)) == F  ){
    #fit linear functions to check that people who want cooler or warmer are increasing 
    #or decreasing with respect to thermal sensation 
    fit_cooler <- lm(prop ~ vars, data = cooler)
    fit_warmer <- lm(prop ~ vars, data = warmer)
    
    if(fit_cooler$coefficients[2] > 0 && fit_warmer$coefficients[2] <0){
      
      return(T)
      
    }else{
      return(F)
    }
   }else{
      return(NA)
    }
}
```


```r
check_studies(test_thermal_preference_R)
```

```
## 89.2857142857143% passing. Out of 28 applicable studies. Studies with non passing: 
##  Djamila
## Sanyogita Manu
## Michael Adebamowo
```
Djamila, Sanyogita Manu and Michael Adebamowo were studies that were found as questionable in powerpoint. 

#### Thermal Comfort


```r
test_thermal_comfort_R <- function(df){
  ### Tests reasonability of thermal comfort votes. 
  ### Fits average thermal comfort vs. sensation to quadratic function to determine reasonability. 
  
  # condition to be able to fit a quadratic function
  if(length(unique(round(df$`Thermal sensation`))) > 2 && all(is.na(df$`Thermal sensation`)) == F && all(is.na(df$`Thermal comfort`)) == F){
    sum_comfort <- df %>% group_by(round(`Thermal sensation`)) %>%
      summarise(avg = mean(as.numeric(`Thermal comfort`), na.rm = TRUE)) %>%
      as.data.frame
    
    names(sum_comfort) <- c("sensation", "comfort")
   
    
    
    if(all(is.na(sum_comfort$comfort)) == FALSE){
      #fit quadratic function to 6 points 
      fit_comfort <- lm(comfort ~  + sensation + I(sensation^2), data = subset(sum_comfort, !(is.na(comfort))))
      
      coeff <- fit_comfort$coefficients[3]
     
      
      if(coeff < 0){
        return(T)
      }else{
        return(F)
      }
      }else{        
        return(NA)
        }
  }else{
    return(NA)
  }
}
```


```r
check_studies(test_thermal_comfort_R)
```

```
## Warning in mean(as.numeric(`Thermal comfort`), na.rm = TRUE): NAs
## introduced by coercion
```

```
## 88.2352941176471% passing. Out of 17 applicable studies. Studies with non passing: 
##  Hyojin Kim
## Michael Adebamowo
```

#### Air movement preference


```r
test_air_preference_R <- function(df){
  ### Function to check reasonability of air preference votes
  ### Fits a linear model to average air preference at each temperature. 
  ### Checks if on average, people want more air movement at higher temperatures, which is to be expected.
  
  num_temp <- length(unique(df$`Air temperature (°F)`))
  
  #Condition to be able to fit a linear model. Also check if there are more than 6 unique temperatures. 
  #If there are less than 6, we can use thermal sensation values instead. 
  if(num_temp > 6 && all(is.na(df$`Air movement preference`)) == FALSE){
   
    #Take average air preference by mapping less, no change and more to -1, 0, and 1. 
    sum_pref <- df %>% group_by(round(`Air temperature (°F)`)) %>%
      mutate(num_pref = factor(`Air movement preference`, levels = c("less", "no change", "more"), labels = c(-1, 0, 1))) %>%
      summarise(avg_pref = mean(as.numeric(num_pref), na.rm = T)) 
    
    names(sum_pref) <- c("Temperature", "Preference")
    
    model <- lm(Preference ~ Temperature, data = sum_pref )
    coeff <- model$coefficients[2]
    
    if(coeff > 0){
      
      return(T)
    }else{
      
      return(F)
    }
  
    
  }else if(all(is.na(df$`Thermal sensation` )) == FALSE && all(is.na(df$`Air movement preference`)) == FALSE){
    
    sum_pref <- df %>% group_by(round(`Thermal sensation`)) %>%
      mutate(num_pref = factor(`Air movement preference`, levels = c("less", "no change", "more"), labels = c(-1, 0, 1))) %>%
      summarise(avg_pref = mean(as.numeric(num_pref), na.rm = T)) 
    
    names(sum_pref) <- c("Sensation", "Preference")
    
    model <- lm(Preference ~ Sensation, data = sum_pref) 
    coeff <- model$coefficients[2]
    
    if(coeff > 0){
      
      return(T)
    }else{
      
      return(F)
    }
    
    
  }else{
    
    return(NA)
  }
  
}
```


```r
check_studies(test_air_preference_R)
```

```
## 94.4444444444444% passing. Out of 18 applicable studies. Studies with non passing: 
##  Stoops, J. L
```

#### Air movement acceptability


```r
test_air_acceptibility_R <- function(df){
  ### Function checking reasonability of air movement acceptability votes. 
  ### Breifly checks by confirming that the proportion of people who find air movement acceptable is highest 
  ### in the category of prefering "no change" for air movement. 
  
  #condition for this test being applicable 
  if(!(all(is.na(df$`Air movement acceptability`))) && !(all(is.na(df$`Air movement preference`)))){
    
    
    #claim order of preferences so that division occurs properly
    df$`Air movement preference` <- factor(df$`Air movement preference`, levels = c("less", "no change", "more"))
    
    totals_pref <- df %>% group_by(`Air movement preference`) %>% 
      filter(!(is.na(`Air movement preference`))) %>%
      summarise(total = n()) 
    
    accept <- df %>% group_by(`Air movement preference`, `Air movement acceptability`) %>% 
      filter(!(is.na(`Air movement preference`))) %>%
      filter(round(`Air movement acceptability`) == 1) %>%
      summarise(count = n())

    proportions <- accept$count/totals_pref$total
    
    index_max <- which.max(proportions)
    
    if(totals_pref$`Air movement preference`[index_max] == "no change"){
      return(T)
    }else{
      return(F)
    }
        
    
  }else{
    return(NA)
  }
  
}
```

```r
check_studies(test_air_acceptibility_R)
```

```
## 87.5% passing. Out of 8 applicable studies. Studies with non passing: 
##  Paliaga
```

#### Humidity sensation


```r
test_humidity_sensation_R <- function(df){
  ### Function testing reasonability of humidity sensation. 
  ### Check by fitting humidity sensation and relativy humidity percentage to a linear model. 
  ### We expect that for lower humidity sensation be correlated to higher humidity percentage. (-3 --> very humid)
  
  #conditions needed to fit a linear model 
  num_humidity <- length(unique(df$`Relative humidity (%)`))
  if(num_humidity > 1 && !(all(is.na(df$`Relative humidity (%)`))) && !(all(is.na(df$`Humidity sensation`)))){
    
    #find average humidity sensation at each humidity level
    avg_sensation <- df %>% 
      mutate(rounded_humidity = round(`Relative humidity (%)`)) %>%
      group_by(rounded_humidity) %>%
      filter(!is.na(`Humidity sensation`)) %>% 
      summarise(sensation = mean(`Humidity sensation`))
    
    humidity_lm <- lm(sensation ~ rounded_humidity, data = avg_sensation)
    coeff <- humidity_lm$coefficients[2]
  
    if(coeff < 0 ){
      return(T)
    }else{
      return(F)
    }
    
  }else{
    return(NA)
  }
  
}
```


```r
check_studies(test_humidity_sensation_R)
```

```
## 100% passing. Out of 8 applicable studies. Studies with non passing: 
## 
```

#### Humidity preference


```r
test_humidity_preference_R <- function(df){
  ### Function that checks reasonability of humidity preference votes. 
  ### Checks in a similar way as the temperature preference by checking if people who want drier or people who want more humid
  ### increase or decrease as expected in comparison to humidity sensation. 
  
  
  #count each humidity preference in each humidity sensation bin
  
  #remove NA values 
  sensation <- df$`Humidity sensation`[which(!is.na(df$`Humidity preference`))]
  pref <- df$`Humidity preference`[which(!is.na(df$`Humidity preference`))]

  df2 <- data.frame(sensation, pref)

  
  pref <- count(df2, vars = `sensation`, wt_vars = pref)
  
  #filter to find proportions in each humidity sensation bin that voted "drier"
  drier <- pref %>% 
    group_by(vars) %>%
    mutate(total = sum(n, na.rm = T)) %>% 
    mutate(prop = n/total) %>%
    filter(wt_vars == "drier")
  #print(drier)
  #filter to find proportions in each humidity sensation bin that voted "more humid"
  more_humid <- pref %>% 
    group_by(vars) %>%
    mutate(total = sum(n, na.rm = T)) %>% 
    mutate(prop = n/total) %>%
    filter(wt_vars == "more humid")
  #print(more_humid)
  if(all(is.na(drier$prop)) == F && all(is.na(drier$vars)) == F && all(is.na(more_humid$prop)) == F && all(is.na(more_humid$vars)) == F  ){
    #fit linear functions to check that people who want drier or more humid are increasing 
    #or decreasing with respect to humidity sensation 
    fit_drier <- lm(prop ~ vars, data = drier)
    fit_humid <- lm(prop ~ vars, data = more_humid)
    
    if(fit_drier$coefficients[2] < 0 && fit_humid$coefficients[2] > 0){
      #print(unique(df$`Data contributor`))
      return(T)
      
    }else{
      return(F)
    }
   }else{
      return(NA)
    }
}
```


```r
check_studies(test_humidity_preference_R)
```

```
## 100% passing. Out of 6 applicable studies. Studies with non passing: 
## 
```

