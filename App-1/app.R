library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
source("funcs.R")
#csv file with Koppen climate names and codes
climate_names <- read.csv("./Data/kfc_climates.csv", header = TRUE)
NUM_PAGES <- 2

# Define UI for data upload app ----
ui <- fluidPage(
  useShinyjs(),
  # App title ----
  titlePanel("Database Entry Quality Check"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Submitted Data", tableOutput("contents")),
                  tabPanel("Basic Identifiers", fluidRow(tableOutput("Publication"),
                          tableOutput("Contributor"), 
                           tableOutput("Year"), 
                           tableOutput("Season"),
                           tableOutput("Koppen"), 
                           tableOutput("Climate"), 
                           tableOutput("Country"), 
                           tableOutput("City"), 
                           tableOutput("BuildingType"), 
                           tableOutput("Cooling"), 
                           tableOutput("CoolingMM"), 
                           tableOutput("Heating"))), 
                  tabPanel("Subjects' Information", fluidRow(tableOutput("Age"),
                             tableOutput("Sex"), 
                             tableOutput("Weight"),
                             tableOutput("Height")
                             
                           )), 
                  tabPanel("Subjective Comfort Data", fluidRow(textOutput("T_sensation"),
                              plotOutput("T_sensation_g"),
                              textOutput("T_acceptability"),
                              plotOutput("T_acceptability_g"),
                              textOutput("T_preference"),
                              plotOutput("T_preference_g"),
                              textOutput("T_comfort"),
                              plotOutput("T_comfort_g"),
                              textOutput("A_preference"),
                              plotOutput("A_preference_g"),
                              textOutput("A_acceptability"),
                              plotOutput("A_acceptability_g"),
                              textOutput("H_sensation"),
                              plotOutput("H_sensation_g"),
                              textOutput("H_preference"),
                              plotOutput("H_preference_g"))), 
                  tabPanel("Instrumented Measurements", fluidRow(tableOutput("Air_temp_C"),
                              tableOutput("Air_temp_F"),
                              tableOutput("Ta_h_C"),
                              tableOutput("Ta_h_F"),
                              tableOutput("Ta_m_C"),
                              tableOutput("Ta_m_F"),
                              tableOutput("Ta_l_C"),
                              tableOutput("Ta_l_F"), 
                              tableOutput("Oper_temp_C"),
                              tableOutput("Oper_temp_F"),
                              tableOutput("Rad_temp_C"),
                              tableOutput("Rad_temp_F"),
                              tableOutput("Globe_temp_C"),
                              tableOutput("Globe_temp_F"),
                              tableOutput("Tg_h_C"),
                              tableOutput("Tg_h_F"),
                              tableOutput("Tg_m_C"),
                              tableOutput("Tg_m_F"),
                              tableOutput("Tg_l_C"), 
                              tableOutput("Tg_l_F"),
                              tableOutput("Outdoor_C"),
                              tableOutput("Outdoor_F"),
                              tableOutput("Clo"),
                              tableOutput("Met"),
                              tableOutput("act_10"),
                              tableOutput("act_20"),
                              tableOutput("act_30"),
                              tableOutput("act_60"), 
                              tableOutput("rel_hum"),
                              tableOutput("air_vel_ms"),
                              tableOutput("air_vel_fpm"),
                              tableOutput("vh_ms"),
                              tableOutput("vh_fpm"),
                              tableOutput("vm_ms"),
                              tableOutput("vm_fpm"),
                              tableOutput("vl_ms"),
                              tableOutput("vl_fpm"))), 
                  tabPanel("Calculated Indices", fluidRow(tableOutput("PMV"),
                                                          tableOutput("PPD"))),
                  tabPanel("Environmental Control", fluidRow(tableOutput("Blind"), 
                              tableOutput("Fan"), 
                              tableOutput("Window"),
                              tableOutput("Door"),
                              tableOutput("Heater"))))
                  
      
      
      
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                   header = TRUE, stringsAsFactors = FALSE, check.names = F )
    
    if(input$disp == "head") {
      return(head(df1))
    }
    else {
      return(df1)
    }
    
  })
  
  
  output$Contributor <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    cont <- check_contributor(df1)
    
    if(cont == TRUE){
      message <- data.frame(c("Contributor: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(cont)
    }
    
    
    
    
  })
  
  
  output$Year <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    yr <- check_year(df1)
    
    if(yr == TRUE){
      message <- data.frame(c("Year: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(yr)
    }
  })
  
  output$Season <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    
    check <- check_season(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Season: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
    
  output$Koppen <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_koppen(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Koppen climate classification: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Climate <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_climate(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Climate: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Country <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_country(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Country: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$BuildingType <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_building(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Building Type: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Cooling <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_cooling(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Cooling Strategy: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$CoolingMM <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_cooling_mm(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Cooling Strategy with MM: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Heating <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_heating(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Heating strategy: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Age <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_age(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Age: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  
  output$Sex <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_sex(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Sex: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Weight <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_weight(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Weight: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Height <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE )
    
    check <- check_height(df1)
    
    if(check == TRUE){
      message <- data.frame(c("Height: passed!"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  #Thermal sensation reasonability
  output$T_sensation <- renderText({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    check <- thermal_sensation_R(df1)
    
    if(check == T){
      return("Thermal sensation: Passed")
    }else if(check == F){
      return("Please check coding of thermal sensation study.")
    }else{
      return("No applicable reasonability test.")
    }
    
  })
  
  output$T_sensation_g <- renderPlot({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    df <- subset(df1, !(is.na(`Thermal sensation`)))
    names(df) <- as.character(1:length(names(df)))
    avgtemp <- df %>% group_by(`32`) %>% 
      summarise(averagetemp = mean(`16`)) %>% 
      as.data.frame
    
    print(avgtemp)
    
    ggplot(avgtemp, aes(x =`32`, y = averagetemp)) + geom_point() + 
      ylab("Average thermal sensation at recorded temperatures") + ggtitle("Thermal sensation vs. Temperature") + 
      xlab("Temperature (F)") +
      geom_smooth(method = "lm") 
    
  })
  
  output$T_acceptability <- renderText({
    
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    check <- test_thermal_acceptibility_R(df1)
    
    if(is.na(check)){
      return("No applicable reasonability test.")
    }else if(check == F){
      return("Thermal accepabtility: Study did not pass reasonability test. Please check encoding.")
    }else{
      return("Thermal acceptability: Study passes reasonability test.")
    }
  })
  
  output$T_acceptability_g <- renderPlot({
    req(input$file1)
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    ggplot(df1, aes(x =`Air temperature (°F)`)) + geom_bar(aes(fill = factor(`Thermal sensation acceptability`)))
  })
  
  output$T_comfort <- renderText({
    
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    check <- test_thermal_comfort_R(df1)
    
    if(is.na(check)){
      return("No applicable reasonability test.")
    }else if(check == F){
      return("Thermal comfort: Study did not pass reasonability test. Please check encoding.")
    }else{
      return("Thermal comfort: Study passes reasonability test.")
    }
    
  })
  
  output$T_comfort_g <- renderPlot({
    
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    df <- subset(df1, !(is.na(`Thermal comfort`)))
    ggplot(df, aes(x = round(`Thermal sensation`), y = rep(1, times = nrow(df)))) + 
      #geom_point()
      geom_bar(aes(fill =  factor(cut(as.numeric(`Thermal comfort`), breaks = c(0,3.5, 6)))), position = "fill", stat = 'identity') + 
      xlab("Thermal sensation") + 
      ylab("Proportion Thermal comfort") + 
      scale_fill_discrete(name = "Key Thermal Comfort") 
    
    
  })
  
  
  
  output$T_preference <- renderText({
    
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    check <- test_thermal_preference_R(df1)
    
    if(is.na(check)){
      return("Thermal preference: No applicable reasonability test.")
    }else if(check == F){
      return("Thermal preference: Study did not pass reasonability test. Please check encoding.")
    }else{
      return("Thermal preference: Study passes reasonability test.")
    }
    
  })
    
    output$T_preference_g <- renderPlot({
      
      req(input$file1)
      
      df1 <- read.csv(input$file1$datapath, sep = ",",
                      header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
      names(df1)[1] <- "Index"
      
      df <- subset(df1, !(is.na(`Thermal preference`)))
      
      ggplot(df, aes(x = round(`Thermal sensation`), y = rep(1, times = nrow(df)))) + 
        geom_bar(aes(fill = factor(`Thermal preference`, levels = c("warmer", "no change", "cooler"))), position = "fill", stat = "identity") + 
        xlab("Rounded Thermal Sensation") + 
        ylab("Proportion - Thermal Preference") + 
        scale_fill_discrete(name = "Thermal preference legend") 
      
      
    })
    
    
 
  
  output$A_preference <- renderText({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    check <- test_air_preference_R(df1)
    
    if(is.na(check)){
      return("Air movement preference: No applicable reasonability test.")
    }else if(check == F){
      return("Air movement preference: Study did not pass reasonability test. Please check encoding.")
    }else{
      return("Air movement preference: Study passes reasonability test.")
    }
    
  })
  
  output$A_preference_g <- renderPlot({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    df<- subset(df1, !is.na(`Air movement preference`))
    
    num_pref <- factor(df$`Air movement preference`, levels = c("less", "no change", "more"), labels = c(-1, 0, 1))
    
    test2_rev <- data.frame(as.numeric(as.character(num_pref)), df$`Air velocity (m/s)`, df$`Air temperature (°F)`)
    names(test2_rev) <- c("num_pref", "velocity", "temperature")
    
    sums <- test2_rev %>% group_by(temperature) %>%
      summarise(mean = mean(num_pref, na.rm = T))
    
    sums %>% ggplot(aes(x=temperature, y = mean)) + geom_point() + geom_smooth(method = 'lm') + 
      ylab("Mean Air Movement Preference") + 
      xlab("Air temperature (F)") + 
      ggtitle("Example expected air movement preference vs. temperature")
    
  })
  
  output$A_acceptability <- renderText({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    check <- test_air_acceptibility_R(df1)
    
    if(is.na(check)){
      return("Air movement acceptability: No applicable reasonability test.")
    }else if(check == F){
      return("Air movement acceptability: Study did not pass reasonability test. Please check encoding.")
    }else{
      return("Air movement acceptability: Study passes reasonability test.")
    }
    
  })
  
  output$A_acceptability_g <- renderPlot({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    df<- subset(df1, !is.na(`Air movement acceptability`))
    if(nrow(df) == 0){
      return(NULL)
    }
    ggplot(df, aes(x = factor(`Air movement preference`, levels = c("less", "no change", "more")), y = rep(1, times = nrow(df)))) + 
      geom_bar(aes(fill = factor(round((`Air movement acceptability`)))), stat = 'identity', position = 'fill') + 
      xlab("Air movement preference") + 
      ylab("Air movement acceptability proportion") + 
      scale_fill_discrete(name = "Acceptability") + 
      ggtitle("Expected Air Movement Acceptability") 
    
  })
  
  output$H_sensation <- renderText({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    check <- test_humidity_sensation_R(df1)
    
    if(is.na(check)){
      return("Humidity Sensation: No applicable reasonability test.")
    }else if(check == F){
      return("Humidity Sensation: Study did not pass reasonability test. Please check encoding.")
    }else{
      return("Humidity Sensation: Study passes reasonability test.")
    }
    
  })
  
  output$H_sensation_g <- renderPlot({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    df <- subset(df1, !(is.na(`Humidity sensation`)))
    
    if(nrow(df) == 0){
      return(NULL)
    }
    
    avg_sensation <- df %>% 
      mutate(rounded_humidity = round(`Relative humidity (%)`)) %>%
      group_by(rounded_humidity) %>%
      filter(!is.na(`Humidity sensation`)) %>% 
      summarise(sensation = mean(`Humidity sensation`))
    
    ggplot(avg_sensation, aes(x= rounded_humidity, y = sensation )) + geom_point() + 
      geom_smooth(method = 'lm') + 
      xlab("Rounded humidity") + 
      ylab("Humidity sensation") + 
      ggtitle("Expected humidity sensation vs. humidity")
    
  })
  
  output$H_preference <- renderText({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    check <- test_humidity_preference_R(df1)
    
    if(is.na(check)){
      return("Humidity Preference: No applicable reasonability test.")
    }else if(check == F){
      return("Humidity Preference: Study did not pass reasonability test. Please check encoding.")
    }else{
      return("Humidity Preference: Study passes reasonability test.")
    }
    
  })
  
  output$H_preference_g <- renderPlot({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    df <- subset(df1, !(is.na(`Humidity preference`)))
    
    if(nrow(df) == 0){
      return(NULL)
    }
    ggplot(df, aes(x = `Humidity sensation`, y = rep(1, times = nrow(df)))) + 
      geom_bar(aes(fill = factor(`Humidity preference`, levels = c("drier", "no change", "more humid"))), stat = 'identity', position = 'fill') + 
      xlab("Humidity sensation") + 
      ylab("Humidity preference proportion") + 
      scale_fill_discrete(name = "Preference") + 
      ggtitle("Example Humidity Sensation")
    
    
    
    
  })
  
  output$Air_temp_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F )
    
    
    temps <- df1$`Air temperature (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Air Temperature (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -17, 50)
    
  
    
    if(check == TRUE){
      message <- data.frame(c("Air Temperature (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Air_temp_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Air temperature (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Air Temperature (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Air Temperature (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Ta_h_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Ta_h (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Ta_h (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -17, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Ta_h (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Ta_h_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Ta_h (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Ta_h (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Ta_h (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Ta_m_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Ta_m (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Ta_m (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -17, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Ta_m (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Ta_l_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Ta_l (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Ta_l (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -17, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Ta_l (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Ta_m_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Ta_m (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Ta_m (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Ta_m (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Ta_l_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Ta_l (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Ta_l (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Ta_l (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  
  
  output$Tg_m_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Tg_m (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Tg_m (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -17, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Tg_m (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Tg_l_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Tg_l (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Tg_l (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -17, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Tg_l (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  
  output$Tg_h_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Tg_h (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Tg_h (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -17, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Tg_h (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Tg_m_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Tg_m (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Tg_m (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Tg_m (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Tg_l_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Tg_l (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Tg_l (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Tg_l (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Tg_h_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Tg_h (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Tg_h (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Tg_h (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Outdoor_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Outdoor monthly air temperature (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Outdoor monthly air temperature (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -22, 140)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Outdoor monthly air temperature (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Outdoor_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Outdoor monthly air temperature (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Outdoor monthly air temperature (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -30, 60)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Outdoor monthly air temperature (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Oper_temp_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Operative temperature (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Operative temperature (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Operative temperature (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  
  output$Oper_temp_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Operative temperature (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Operative temperature (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -7, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Operative temperature (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  output$Rad_temp_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Radiant temperature (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Radiant temperature (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Radiant temperature (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  
  output$Rad_temp_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Radiant temperature (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Radiant temperature (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -7, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Radiant temperature (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Globe_temp_F <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Globe temperature (°F)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Globe temperature (°F) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, 0, 120)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Globe temperature (°F) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  
  output$Globe_temp_C <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`Globe temperature (°C)`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("Globe temperature (°C) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -7, 50)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Globe temperature (°C) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Clo <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    clo <- df1$`Clo`
    
    if(all(is.na(clo)) == T){
      message <- data.frame(c("Clo : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_clo_R(df1)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Clo : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Met <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    met <- df1$`Met`
    
    if(all(is.na(met)) == T){
      message <- data.frame(c("Met : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_met_R(met)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Met : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$act_10 <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    met <- df1$`activity_10`
    
    if(all(is.na(met)) == T){
      message <- data.frame(c("activity_10 : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_met_R(met)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("activity_10 : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$act_20 <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    met <- df1$`activity_20`
    
    if(all(is.na(met)) == T){
      message <- data.frame(c("activity_20 : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_met_R(met)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("activity_20 : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$act_30 <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    met <- df1$`activity_30`
    
    if(all(is.na(met)) == T){
      message <- data.frame(c("activity_30 : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_met_R(met)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("activity_30 : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$act_60 <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    met <- df1$`activity_60`
    
    if(all(is.na(met)) == T){
      message <- data.frame(c("activity_60 : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_met_R(met)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("activity_60 : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$air_vel_ms <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    vel <- df1$`Air velocity (m/s)`
    
    if(all(is.na(vel)) == T){
      message <- data.frame(c("Air velocity (m/s) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_velocity(vel, 0, 5)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Air velocity (m/s) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$air_vel_fpm <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    vel <- df1$`Air velocity (fpm)`
    
    if(all(is.na(vel)) == T){
      message <- data.frame(c("Air velocity (fpm) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_velocity(vel, 0, 985)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Air velocity (fpm) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$vh_ms <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    vel <- df1$`Velocity_h (m/s)`
    
    if(all(is.na(vel)) == T){
      message <- data.frame(c("Velocity_h (m/s) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_velocity(vel, 0, 5)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Velocity_h (m/s) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$vh_fpm <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    vel <- df1$`Velocity_h (fpm)`
    
    if(all(is.na(vel)) == T){
      message <- data.frame(c("Velocity_h (fpm) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_velocity(vel, 0, 985)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Velocity_h (fpm) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$vm_ms <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    vel <- df1$`Velocity_m (m/s)`
    
    if(all(is.na(vel)) == T){
      message <- data.frame(c("Velocity_m (m/s) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_velocity(vel, 0, 5)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Velocity_m (m/s) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$vm_fpm <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    vel <- df1$`Velocity_m (fpm)`
    
    if(all(is.na(vel)) == T){
      message <- data.frame(c("Velocity_m (fpm) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_velocity(vel, 0, 985)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Velocity_m (fpm) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$vl_ms <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    vel <- df1$`Velocity_l (m/s)`
    
    if(all(is.na(vel)) == T){
      message <- data.frame(c("Velocity_l (m/s) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_velocity(vel, 0, 5)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Velocity_l (m/s) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$vl_fpm <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    vel <- df1$`Velocity_l (fpm)`
    
    if(all(is.na(vel)) == T){
      message <- data.frame(c("Velocity_l (fpm) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_velocity(vel, 0, 985)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Velocity_l (fpm) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Blind <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    environ <- df1$`Blind (curtain)`
    
    if(all(is.na(environ)) == T){
      message <- data.frame(c("Blind (curtain) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_environ_control(environ)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Blind (curtain) : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Fan <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    environ <- df1$`Fan`
    
    if(all(is.na(environ)) == T){
      message <- data.frame(c("Fan : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_environ_control(environ)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Fan : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Window <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    environ <- df1$`Window`
    
    if(all(is.na(environ)) == T){
      message <- data.frame(c("Window : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_environ_control(environ)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Window : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Heater <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    environ <- df1$`Heater`
    
    if(all(is.na(environ)) == T){
      message <- data.frame(c("Heater : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_environ_control(environ)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Heater : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  output$Door <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    environ <- df1$`Door`
    
    if(all(is.na(environ)) == T){
      message <- data.frame(c("Door : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_environ_control(environ)
    
    
    
    if(check == TRUE){
      message <- data.frame(c("Door : passed"))
      names(message) <- c("")
      return(message)
    }else{
      return(check)
    }
  })
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)