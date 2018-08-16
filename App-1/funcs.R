library(rvest)

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

### Cooling strategy building level

### Cooling strategy for MM buildings

### Heating strategy building level 

### Age

### Sex

### Thermal Sensation

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

check_temp <- function(temperatures, min, max){
  #want to check that temperatures are all integers and fall within a reasonable range
  
  
  
}

### Clo

### Met 

### Velocity 

### Weight 

### Height

### Blind (curtain)

### Fan

### Window

### Door


