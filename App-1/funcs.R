library(rvest)

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
    print(index)
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




### Cooling strategy for MM buildings




### Heating strategy building level 

### Age

### Sex

### Thermal Sensation

test_thermal_sensation_R <- function(df){
  #test if thermal sensation values at reported temperatures seem reasonable 
  #by performing linear regression and checking for positive correlation 
  
  fit <- lm(`Thermal sensation` ~ `Air temperature (°F)`, data=df)
  if(fit$coefficients[2] > 0){
    return(TRUE)
  }else{
    ggplot(df, aes(x = `Air temperature (°F)`, y = `Thermal sensation`)) + geom_point() + 
      xlab("Air Temperature (F)") + ylab("Thermal Sensation")
  }
  
}

### Thermal acceptibility



### Thermal preference

### Air movement acceptibility

### Air movement preference

### Thermal comfort

### Humidity sensation

### Humidity preference 

### PMV

### PPD

### Temperatures

check_temp_R <- function(temperatures, min, max){
  #want to check that temperatures are all integers and fall within a reasonable range
  #enter the column to be checked instead of the whole data frame 
  
  under <- ifelse(temperatures < min, TRUE, FALSE)
  over <- ifelse(temperatures > max, TRUE, FALSE)
  
  index <- c(which(under), which(over))
  
  if (length(index) > 0){
    
    unreason_temp <- temperatures[index]
    new_df <- data.frame(index, unreason_temp)
    names(new_df) <- c("Index", "Temperature")
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

### Weight 

### Height

### Blind (curtain)

### Fan

### Window

### Door


