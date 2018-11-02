library(rvest)
library(RJSONIO)

##########
# Function with a _R at the end indicate that they are functions to test reasonability. 
# All other functions to check correct formatting for submission. 
##########

###Publication (Citation)

###Data Contributor
check_contributor <- function(df){
  contributor <- as.character(df$`Data contributor`)
  
  if (length(which(grepl("[[:digit:]]", contributor))) > 0){
    index <- which(grepl("[[:digit:]]", contributor))
    contributors <- contributor[index]
    new_df <- data.frame(index, contributors)
    names(new_df) <- c('Index', 'Contributor')
    return(new_df)
  }else{
    return(TRUE)
  }
  
}

###Year
check_year <- function(df){
  
  year <- as.character(df$Year)
  val <- TRUE
  year_vec <- grepl("[^0-9]", year)
  if (length(which(year_vec)) > 0){
    #print("There seems to be a year with a non-numeric character. Please check and then proceed.")
    val <-FALSE 
    index<- which(year_vec)
     
  }
  
  
  #check that there are only 4 digits
  
  num_year <- nchar(gsub(" ", "", year))
  
  if (length(num_year[num_year != 4]) > 0){
    #print("There seems to be a date that is not formatted correctly. Please check and then proceed.")
    val <- FALSE
    index <- c(index, which(ifelse(num_year !=4, TRUE, FALSE)))
    
   
    
  } 
  
  
  if(val == FALSE) {
    index <- unique(sort(index))
    wrong_year <- df$Year[index]
    new_df <- data.frame(index, wrong_year)
    names(new_df) <- c("Index", "Year")
    return(new_df)
    
  }else{
    return(TRUE)
  }
  
}

### Season

check_season <- function(df){
  seasons <- factor(df$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))
  
  if (length(which(is.na(seasons))) > 0){
    index <- which(is.na(seasons))
    wrong_seasons <- df$Season[index]
    new_df <- data.frame(index, wrong_seasons)
    names(new_df) <- c("Index", "Season")
    return(new_df)
  } else{
    return(TRUE)
  }
}

### Koppen Climate Classification 

check_koppen <- function(df){
  #climate classifications from Google and documented in a csv file 
  climate_names <- read.csv("./Data/kfc_climates.csv", header = TRUE)
  codes <- factor(df$`Koppen climate classification`, levels = levels(climate_names$Code))
  index <- which(is.na(codes))
  if (length(index)>0) {
    wrong_codes <- df$`Koppen climate classification`[index]
    new_df <- data.frame(index, wrong_codes)
    names(new_df) <- c("Index", "Koppen climate classification")
    return(new_df)

  }else{
    return(TRUE)
  }
}


### Climate

check_climate <- function(df){
  #climate classifications from Google and documented in a csv file 
  climate_names <- read.csv("./Data/kfc_climates.csv", header = TRUE, check.names = F)
  names <- factor(df$`Climate`, levels = levels(climate_names$`Climate Name`))
  index <- which(is.na(names))
  if (length(index)>0) {
    wrong_names <- df$`Climate`[index]
    new_df <- data.frame(index, wrong_names)
    names(new_df) <- c("Index", "Climate")
    return(new_df)
    
  }else{
    return(TRUE)
  }
  
}

### City






### Country

check_country <- function(df){
  
  #webscrape countrycodes table 
  
  url <- "https://wiki.openstreetmap.org/wiki/Nominatim/Country_Codes"
  
  
  countrycodes <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
    html_table() 
  
  countrycodes <- countrycodes[[1]]
  
  countrycodes <- countrycodes[, 1:2]
  names(countrycodes) <- c("CountryCode", "CountryName")
  
  
  #countrycodes$CountryCode <- factor(countrycodes$CountryCode)
  countrycodes$CountryName <- factor(countrycodes$CountryName)
  
  #need to make sure blanks get read in as "" instead of NA
  
  countries <- factor(df$Country, levels = levels(countrycodes$CountryName), order = TRUE)
  index <- which(is.na(countries))
  if (length(index) > 0){
    wrong_countries <- df$Country[index]
    new_df <- data.frame(index, wrong_countries)
    names(new_df) <- c("Index", "Country")
    return(new_df)
  } else{
    return(TRUE)
  }
  
  
}

check_city <- function(df){
  countrycodes <- read.csv("./Data/CountryCodes.csv", header = TRUE, check.names = F, stringsAsFactors = F)
  #get country code for countries in df to query
  code <- c()
  for (country in df$Country){
    
    index <- which(countrycodes$CountryName == country)
    code <- c(code, countrycodes$CountryCode[index])
   
    
  }
  
  #create new data frame with city
  #only need to search distinct cities; make searching faster
  test_codes <- data.frame(code, df$City) %>% distinct()
  names(test_codes) <- c("CountryCode", "CityName")
  
  #code adapted from https://stackoverflow.com/questions/13905098/how-to-get-the-longitude-and-latitude-coordinates-from-a-city-name-and-country-i
  
  nrow <- nrow(test_codes)
  counter <- 1
  test_codes$lon[counter] <- 0
  test_codes$lat[counter] <- 0
  while (counter <= nrow){
    CityName <- gsub(' ','%20',test_codes$CityName[counter]) #remove space for URLs
    CountryCode <- test_codes$CountryCode[counter]
    url <- paste(
      "http://nominatim.openstreetmap.org/search?city="
      , CityName
      , "&countrycodes="
      , CountryCode
      , "&limit=9&format=json"
      , sep="")
    x <- fromJSON(url)
    if(is.vector(x)){
      test_codes$lon[counter] <- x[[1]]$lon
      test_codes$lat[counter] <- x[[1]]$lat    
    }
    counter <- counter + 1
    Sys.sleep(1.1) #to comply with website usage guidelines 
  }
  
  index <- which(test_codes$lon == 0 & test_codes$lat == 0)
  length_wrong <- length(index)
  
  if(length_wrong > 0){
    wrong_cities <- test_codes$CityName[index]
    
    wrong_df <- subset(df, df$City %in% wrong_cities)
    wrong_df <- wrong_df %>% select(City)
    return(wrong_df)
    
  }else{
    return(T)
  }
  
}

### Building type

check_building <- function(df){
  
  buildings <- factor(df$`Building type`, levels = c("Classroom", "Multifamily housing", "Office", "Senior Center", "Others"))
  
  index <- which(is.na(buildings))
  if (length(index) > 0){
    wrong_buildings <- df$`Building type`[index]
    new_df <- data.frame(index, wrong_buildings)
    names(new_df) <- c("Index", "Building type")
    return(new_df)
  } else {
    return(TRUE)
  }
}

### Cooling strategy building level

check_cooling <- function(df){
  
  buildings <- factor(df$`Cooling startegy_building level`, levels = c("Air Conditioned", "Mixed Mode", "Naturally Ventilated", "Mechanically Ventilated"))
  
  index <- intersect(which(is.na(buildings)), which(!is.na(df$`Cooling startegy_building level`)))
  if (length(index) > 0){
    wrong_cooling <- df$`Cooling startegy_building level`[index]
    new_df <- data.frame(index, wrong_cooling)
    names(new_df) <- c("Index", "Cooling Strategy")
    return(new_df)
  } else {
    return(TRUE)
  }
}



### Cooling strategy for MM buildings

check_cooling_mm <- function(df){
  
  buildings <- factor(df$`Cooling startegy_operation mode for MM buildings`, levels = c("Air Conditioned", "Naturally Ventilated", "Unknown"))
  
  index <- intersect(which(is.na(buildings)), which(!is.na(df$`Cooling startegy_operation mode for MM buildings`)))
  if (length(index) > 0){
    wrong_cooling <- df$`Cooling startegy_operation mode for MM buildings`[index]
    new_df <- data.frame(index, wrong_cooling)
    names(new_df) <- c("Index", "Cooling Strategy MM")
    return(new_df)
  } else {
    return(TRUE)
  }
}



### Heating strategy building level 

check_heating <- function(df){
  
  buildings <- factor(df$`Heating strategy_building level`, levels = c("Mechanical Heating"))
  
  index <- intersect(which(is.na(buildings)), which(!is.na(df$`Heating strategy_building level`)))
  if (length(index) > 0){
    wrong_heating <- df$`Heating strategy_building level`[index]
    new_df <- data.frame(index, wrong_heating)
    names(new_df) <- c("Index", "Heating Strategy")
    return(new_df)
  } else {
    return(TRUE)
  }
}



### Age

check_age <- function(df){
  print(unique(df$`Data contributor`))
  age <- df$`Age`
  is_int_age <- sapply(age, is.numeric)

  age_range <- ifelse( (age <= 100 || age >= 0 ) & abs(round(age)- age) == 0, FALSE, TRUE)
  
  if (length(which(age_range)) > 0 || length(which(!is_int_age)) > 0 ){
    index <- unique(union(which(!is_int_age), which(age_range)))
    wrong_age <- df$Age[index]
    new_df <- data.frame(index, wrong_age)
    
    names(new_df) <- c("Index", "Age")
    return(new_df)
  }else{
    return(T)
  }
  
}

### Sex


check_sex <- function(df){
  
  sex <- factor(df$`Sex`, levels = c("Male", "Female", "Undefined"))
  
  index <- intersect(which(is.na(sex)), which(!is.na(df$`Sex`)))
  if (length(index) > 0){
    wrong_sex <- df$`Sex`[index]
    new_df <- data.frame(index, wrong_sex)
    names(new_df) <- c("Index", "Sex")
    return(new_df)
  } else {
    return(TRUE)
  }
}

### Thermal Sensation

thermal_sensation_R <- function(df){
  ### Test reasonability of thermal sensation votes. 
  ### Takes linear relationship between sensation and tempearture to determine reasonability.
  
  #rename variable names. Shiny and dplyr combination does not like spaces or symbols in names. 
  #index 32 for temperature in Fahrenheit
  #index 16 for thermal sensation 
  
  names(df) <- as.character(1:length(names(df)))
  
  #take the average sensation at each air temperature 
  avg <- df %>% 
    group_by(`32`) %>% 
    summarise(averagetemp = mean(`16`, na.rm = T)) %>%
    as.data.frame
  names(avg) <- c("temp", "sensation")
  #condition for linear model to be fit. Need columns to not be all NA values.
  if(length(avg$`temp`) > 1 && all(is.na(avg$temp)) == FALSE && all(is.na(avg$`sensation`) == FALSE)){
    fit <- lm(sensation~temp, data = avg)
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

### Thermal acceptibility

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



### Thermal preference

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


### Thermal Comfort

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

### Air Movement Preference 

test_air_preference_R <- function(df){
  ### Function to check reasonability of air preference votes
  ### Fits a linear model to average air preference at each velocity. 
  ### Checks if on average, people want less air movement at higher velocities, which is to be expected.
  
  num_temp <- length(unique(df$`Air velocity (m/s)`))
  
  #Condition to be able to fit a linear model. Also check if there are more than 6 unique temperatures. 
  #If there are less than 6, we can use thermal sensation values instead. 
  if(all(is.na(df$`Air velocity (m/s)`)) == FALSE && all(is.na(df$`Air movement preference`)) == FALSE){
    
    #Take average air preference by mapping less, no change and more to -1, 0, and 1. 
    sum_pref <- df %>% group_by(`Air velocity (m/s)`) %>%
      mutate(num_pref = factor(`Air movement preference`, levels = c("less", "no change", "more"), labels = c(-1, 0, 1))) %>%
      summarise(avg_pref = mean(as.numeric(num_pref), na.rm = T)) 
    
    names(sum_pref) <- c("Temperature", "Preference")
    
    model <- lm(Preference ~ Temperature, data = sum_pref )
    coeff <- model$coefficients[2]
    
    if(coeff < 0){
      
      return(T)
    }else{
      return(F)
    }
    
    
    
    
  }else{
    
    return(NA)
  }
  
}

### Air Movement Acceptability

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

### Humidity Sensation

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

### Humidity Preference

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


###### Note: only need one function for temperature, clo, met, velocity, weight, height age, etc. 
############ Make a range check function. Then one function for blind, fan, curtain, heater etc. 

### Use for checking PMV, PPD, %Humidity

check_range_R <- function(col, min, max, name = ""){
  #want to check that temperatures are all integers and fall within a reasonable range
  #enter the column to be checked instead of the whole data frame 
  
  
  ## First check for any typos of non-numeric values
  col1 <- as.character(col)
  index_alpha <- c()
  ## Checks for any typos that are not digits (also period for decimal points)
  col_alpha <- grepl("[^0-9|\\.|-]", col1)
  if (length(which(col_alpha)) > 0){
    index_alpha <- which(col_alpha)
    
  }
  
  ## Check range
  col1 <- as.numeric(col)
  
  under <- ifelse(col1 < min, TRUE, FALSE)
  over <- ifelse(col1 > max, TRUE, FALSE)
  
  index_range <- c(which(under), which(over))
  
  index <- sort(unique(c(index_alpha, index_range)))
  
  
  if (length(index) > 0){
    
    unreason_col <- col[index]
    new_df <- data.frame(index, unreason_col)
    names(new_df) <- c("Index", name)
    return(new_df)
  } else{
    return(TRUE)
  }
  
  
}


### Temperatures

check_temp_R <- function(temperatures, min, max, name){
  #want to check that temperatures are all integers and fall within a reasonable range
  #enter the column to be checked instead of the whole data frame 
  
  temperatures1 <- as.character(temperatures)
  index_alpha <- c()
  temp_grep <- grepl("[^0-9\\.]", temperatures1)
  if (length(which(temp_grep)) > 0){
    index_alpha <- which(temp_grep)
    
  }

  temperatures1 <- as.numeric(temperatures)
  under <- ifelse(temperatures1 < min, TRUE, FALSE)
  over <- ifelse(temperatures1 > max, TRUE, FALSE)
  
  index_range <- c(which(under), which(over))
  
  
  index <- unique(c(index_alpha, index_range))
  if (length(index) > 0){
    
    unreason_temp <- temperatures[index]
    new_df <- data.frame(index, unreason_temp)
    names(new_df) <- c("Index", name)
    return(new_df)
  } else{
    return(TRUE)
  }
  

}

### Clo

check_clo_R <- function(df){
  #want to check that clo values are all positive values and fall within a reasonable range
  #assume it is reasonable for clo values to be between 0 and 5. 
  
  clo <- df$`Clo`
  under <- ifelse(clo < 0, TRUE, FALSE)
  over <- ifelse(clo > 5, TRUE, FALSE)
  
  index <- c(which(under), which(over))
  
  if (length(index) > 0){
    
    unreason_clo <- clo[index]
    new_df <- data.frame(index, unreason_clo)
    names(new_df) <- c("Index", "Clo")
    return(new_df)
  } else{
    return(TRUE)
  }
  
  
}

### Met 

check_met_R <- function(met){
  #want to check that metabolic rate values fall within a reasonable range
  #assume it is reasonable for metabolic rate values to be between 0 and 10. 
  
  
  under <- ifelse(met < 0, TRUE, FALSE)
  over <- ifelse(met > 10, TRUE, FALSE)
  
  index <- c(which(under), which(over))
  
  if (length(index) > 0){
    
    unreason_met <- met[index]
    new_df <- data.frame(index, unreason_met)
    names(new_df) <- c("Index", "Metabolic Rate")
    return(new_df)
  } else{
    return(TRUE)
  }
  
  
}

### Velocity 

#input the column that needs to be checked
#also min/max depended on units
#would want ~0-5m/s
check_velocity <- function(vel, min, max){
  
  
  is_int_vel <- sapply(vel, is.numeric)
  
  #check for a reasonable weight range
  vel_range <- ifelse( vel <= max || vel >= min  , FALSE, TRUE)
  
  if (length(which(vel_range)) > 0 || length(which(!is_int_vel)) > 0 ){
    index <- unique(union(which(!is_int_vel), which(vel_range)))
    wrong_vel <- vel[index]
    new_df <- data.frame(index, wrong_vel)
    
    names(new_df) <- c("Index", "Velocity")
    return(new_df)
  }else{
    return(T)
  }
  
}

### Weight 

check_weight <- function(df){
  
  weight <- df$`Subject´s weight (kg)`
  is_int_weight <- sapply(weight, is.numeric)
  
  #check for a reasonable weight range
  weight_range <- ifelse( weight <= 150 || weight >= 15  , FALSE, TRUE)
  
  if (length(which(weight_range)) > 0 || length(which(!is_int_weight)) > 0 ){
    index <- unique(union(which(!is_int_weight), which(weight_range)))
    wrong_weight <- weight[index]
    new_df <- data.frame(index, wrong_weight)
    
    names(new_df) <- c("Index", "Weight")
    return(new_df)
  }else{
    return(T)
  }
  
}

### Height
check_height <- function(df){
  
  height <- df$`Subject´s height (kg)`
  is_int_height <- sapply(height, is.numeric)
  
  #check for a reasonable height range
  height_range <- ifelse( height <= 250 || height >= 60  , FALSE, TRUE)
  
  if (length(which(height_range)) > 0 || length(which(!is_int_height)) > 0 ){
    index <- unique(union(which(!is_int_height), which(height_range)))
    wrong_height <- height[index]
    new_df <- data.frame(index, wrong_height)
    
    names(new_df) <- c("Index", "Height")
    return(new_df)
  }else{
    return(T)
  }
  
}





### Environmental Controls

##### Function to check format for Blinds, Fans, Windows, Doors, Heaters 
##### Input column for each above into col of function. 

check_environ_control <- function(col){
  
  output <- as.character(col)
  
  #check if column has 0, 1 or an NA value. 
  checked <- ifelse(grepl("[01]", col)|is.na(col), FALSE, TRUE)
  
  index <- which(checked)
  
  if(length(index) == 0){
    return(T)
  }else{
    wrong_environ <- col[index]
    new_df <- data.frame(index, wrong_environ)
    names(new_df) <- c("Index", "Input Value")
    return(new_df)
  }
  
  
  
}

### Check Conversions

##### Farenheit & Celsius, m/s & fpm, PMV & PPD 

check_conversion <- function(col1, col2, mult, const = 0, accuracy, names = c()){
  ## where conversion whould be col1 = const + mult*col2
  ## accuracy is how many digits past zero to round to 
  
  
  converted <- mult*col2 + const
  
  index <- which(round(col1, digits = accuracy) != round(converted, digits = accuracy))
  
  if(length(index) > 0){
    wrong_col1 <- col1[index]
    wrong_col2 <- col2[index]
    
    new_df <- data.frame(index, wrong_col1, wrong_col2)
    names(new_df) <- c("Index", names)
    return(new_df)
  }else{
    return(T)
  }
  
  
  
  
  
}


### Helper Functions

na_message <- function(col, name){
    string <- paste(col, ": All NA")
    message <- data.frame(c(string))
    names(message) <- c("")
    return(message)
}

check_factor <- function(col, factors, name){
  
  factored <- factor(col, levels = factors)
  
  #checks that NA's have been added due to factorization
  if (length(which(is.na(factored))) > 0 && length(which(is.na(factored))) >  length(which(is.na(col)))){
    
    index1 <- which(is.na(factored))
    index2 <- which(is.na(col))
    index <- subset(index1, !(index1 %in% index2))
    wrong_cols <- col[index]
    new_df <- data.frame(index, wrong_cols)
    names(new_df) <- c("Index", name)
    return(new_df)
  } else{
    return(TRUE)
  }
}
