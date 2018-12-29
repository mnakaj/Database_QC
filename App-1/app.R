library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(shinydashboard)
library(tidyverse)
source("funcs.R")
library(here)
#csv file with Koppen climate names and codes
climate_names <- read.csv("./Data/kfc_climates.csv", header = TRUE)


# Define UI for data upload app ----
ui <- fluidPage(
  useShinyjs(),
  # App title ----
  titlePanel("Database Entry Quality Check"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      p("In order to check data submission: "),
      tags$ol(
        tags$li("Submit csv file for data submission below"), 
        tags$li("Use tabs to the right to check formatting and reasonability results"),
        tags$li("If there are any formatting or reasonability errors found, please check your data and re-submit.")
      ),
      
      
      tags$hr(),
      
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
                   selected = "head"),
      
      tags$hr(),
      
      strong("To download template csv file for submission, please download below: "),
      
      
      downloadButton("downloadTemp", "Template CSV File"), 
      
      em("Note:"),
      p("If there is any trouble with symbols not being found, (ie. °C or % etc), please save csv file with UTF-8 encoding. This can 
        easily be done in Excel by going to File --> Save As --> File Format --> CSV UTF-8.")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Submitted Data", h3("Submission Formatting"),
                           p("Please ensure all formatting is correct before continuing with quality check."),
                           textOutput("formatting"),
                           tableOutput("names"),
                           hr(),
                           h3("Submitted Data"),
                           tableOutput("contents")),
                  tabPanel("Basic Identifiers", h3("Formatting checks for Basic Identifiers"),
                           p("If there are formatting errors, errors will be listed in a table. Please be patient as functions load."),
                           wellPanel(h3("Checking Criteria"),
                                     p("All values should be NA or:"),
                                     tags$li(tags$b("Data contributor:"), "Non-numeric entry, only letters"),
                                     tags$li(tags$b("Year:"), "Four-digit number"),
                                     tags$li(tags$b("Season"), "Spring, Summer, Autumn or Winter"),
                                     tags$li(tags$b("Koppen:"), "Must be a 3-letter code"),
                                     tags$li(tags$b("Country:"), "Must be a country listed in the file `CountryCodes.csv`"),
                                     tags$li(tags$b("City:"), "Must be a city found in openstreetmap.org"),
                                     tags$li(tags$b("Building Type:"), "Classroom, Multifamily Housing, Office, Senior Center or Others"), 
                                     tags$li(tags$b("Cooling strategy:") ,"Air Conditioned, Mixed Mode, Naturally Ventilated or Mechanically Ventilated"),
                                     tags$li(tags$b("Cooling strategy with MM:"), "Air Conditioned, Naturally Ventilated or Unknown"),
                                     tags$li(tags$b("Heating strategy:"), "Mechanical Heating"),
                                     style = "background: aliceblue"),
                          fluidRow(
                          tableOutput("Contributor"), 
                           tableOutput("Year"), 
                           tableOutput("Season"),
                           tableOutput("Koppen"), 
                           tableOutput("Climate"), 
                           tableOutput("Country"), 
                           em("Note: If country is not correct, city will automatically error."),
                           tableOutput("City"), 
                           tableOutput("BuildingType"), 
                           tableOutput("Cooling"), 
                           tableOutput("CoolingMM"), 
                           tableOutput("Heating"))), 
                  tabPanel("Subjects' Information", h3("Formatting checks for Subjects' Information"), 
                           wellPanel(h3("Checking Criteria"),
                                     p("All values should be NA or:"),
                                     tags$li(tags$b("Age:"), "Integer between 0 and 100"),
                                     tags$li(tags$b("Sex:"), "Male, Female or Undefined"), 
                                     tags$li(tags$b("Weight:"), "Numeric value between 15kg and 150kg"),
                                     tags$li(tags$b("Height:"), "Numeric value between 30cm and 250cm"),
                                     style = "background: aliceblue"),
                             fluidRow(tableOutput("Age"),
                             tableOutput("Sex"), 
                             tableOutput("Weight"),
                             tableOutput("Height")
                             
                           )), 
                  tabPanel("Subjective Comfort Data", fluidRow(h3("Formatting and resonability checks for Comfort Data"), 
                              p("Below we show results for reasonability test on data. This is to quickly check for typos and correct 
                                encodings of data. Plots are shown regardless of passing or not passing reasonability tests."),  
                              h4("Thermal comfort data"),
                              strong(em("Thermal sensation")),
                              wellPanel(tags$b("Checking Criteria"),
                                        tags$li(em("Format:"), "Numeric value between -3 and 3, or NA"),
                                        tags$li(em("Reasonability:"), "Average thermal sensation is expected to increase with respect to indoor air temperature"),
                                        style = "background: aliceblue"),
                              textOutput("T_sensation"),
                              plotOutput("T_sensation_g"),
                              tableOutput("T_sensation_f"),
                              strong(em("Thermal acceptability")),
                              wellPanel(tags$b("Checking Criteria"),
                                        tags$li(em("Format:"), "0, 1 or NA"),
                                        tags$li(em("Reasonability:"), "Proportion of people voting acceptable (1) is expected to be negatively quadratic with respect to thermal sensation, ie. proportion increases and then decreases with peak at thermal sensation of 0."),
                                        style = "background: aliceblue"),
                              textOutput("T_acceptability"),
                              plotOutput("T_acceptability_g"),
                              tableOutput("T_acceptability_f"),
                              strong(em("Thermal preference")),
                              wellPanel(tags$b("Checking Criteria"),
                                        tags$li(em("Format:"), "warmer, no change, cooler, or NA"),
                                        tags$li(em("Reasonability:"), "Proportion of people voting warmer is expected to go down with increased thermal sensation. Proportion of people voting cooler is expected to go up with increased thermal sensation."),
                                        style = "background: aliceblue"),
                              textOutput("T_preference"),
                              plotOutput("T_preference_g"),
                              tableOutput("T_preference_f"),
                              strong(em("Thermal comfort")),
                              wellPanel(tags$b("Checking Criteria"),
                                        tags$li(em("Format:"), "Numeric value between 0 and 6, or NA"),
                                        tags$li(em("Reasonability:"), "Average thermal comfort value at each thermal sensation is expected to be negatively quadratic with respect to thermal sensation."),
                                        style = "background: aliceblue"),
                              textOutput("T_comfort"),
                              plotOutput("T_comfort_g"),
                              tableOutput("T_comfort_f"),
                              h4("Air movement comfort data"),
                              strong(em("Air movement preference")),
                              wellPanel(tags$b("Checking Criteria"),
                                        tags$li(em("Format:"), "less, no change, more, or NA"),
                                        tags$li(em("Reasonability:"), "On average, proportion of people wanting more air movement is expected to decrease as air velocities increase."),
                                        style = "background: aliceblue"),
                              textOutput("A_preference"),
                              plotOutput("A_preference_g"),
                              tableOutput("A_preference_f"),
                              strong(em("Air movement acceptability")),
                              wellPanel(tags$b("Checking Criteria"),
                                        tags$li(em("Format:"), "0, 1 or NA"),
                                        tags$li(em("Reasonability:"), "Proportion of people who find air movement acceptable is expected to be highest in category `no change`."),
                                        style = "background: aliceblue"),
                              textOutput("A_acceptability"),
                              plotOutput("A_acceptability_g"),
                              tableOutput("A_acceptability_f"),
                              h4("Humidity Comfort Data"),
                              strong(em("Humidity sensation")),
                              wellPanel(tags$b("Checking Criteria"),
                                        tags$li(em("Format:"), "Numeric value between -3 and 3, or NA"),
                                        tags$li(em("Reasonability:"), "Humidity sensation is expected to decrease with respect to relative humidity percentage"),
                                        style = "background: aliceblue"),
                              textOutput("H_sensation"),
                              plotOutput("H_sensation_g"),
                              tableOutput("H_sensation_f"),
                              strong(em("Humidity preference")),
                              wellPanel(tags$b("Checking Criteria"),
                                        tags$li(em("Format:"), "drier, more humid, no change, or NA"),
                                        tags$li(em("Reasonability:"), "Proportion of people wanting drier is expected to decrease with humidity sensation. Proportion of people wanting more humid is expected to increase with humidity sensation. "),
                                        style = "background: aliceblue"),
                              textOutput("H_preference"),
                              plotOutput("H_preference_g"),
                              tableOutput("H_preference_f"))), 
                  tabPanel("Instrumented Measurements", h3("Formatting checks for Instrumented Measurements"),
                           wellPanel(tags$b("Checking Criteria"),
                                     p("All measured values should be NA or:"),
                                     tags$li(tags$b("Measured indoor air temperature values:"), "Numeric value between 0°F and 120°F or between -17°C and 50°C"),
                                     tags$li(tags$b("Measured monthly outdoor air temperature values:"), "Numeric value between -60°F and 140°F or between -50°C and 60°C"),
                                     tags$li(tags$b("Relative Humidity:"), "Numeric value between 0 and 100"), 
                                     tags$li(tags$b("Clo:"), "Numeric value between 0 and 5"), 
                                     tags$li(tags$b("Measured metabolic rate values:"), "Numeric value between 0 and 10"),
                                     tags$li(tags$b("Measured velocity values:"), "Numeric value between 0 m/s and 5 m/s or between 0 fpm and 985 fpm"),
                                     style = "background: aliceblue"),
                           fluidRow(h4("Temperature"), tableOutput("Air_temp_C"),
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
                              h4("Humidity"),
                              tableOutput("Relative_humidity"),
                              h4("Clo"),
                              tableOutput("Clo"),
                              h4("Metabolic Rate"),
                              tableOutput("Met"),
                              tableOutput("act_10"),
                              tableOutput("act_20"),
                              tableOutput("act_30"),
                              tableOutput("act_60"), 
                              h4("Air Velocity"),
                              tableOutput("air_vel_ms"),
                              tableOutput("air_vel_fpm"),
                              tableOutput("vh_ms"),
                              tableOutput("vh_fpm"),
                              tableOutput("vm_ms"),
                              tableOutput("vm_fpm"),
                              tableOutput("vl_ms"),
                              tableOutput("vl_fpm"))), 
                  tabPanel("Unit Conversions",
                           wellPanel(tags$b("Checking Criteria"),
                                     tags$li(tags$b("Temperature:"), "checks °F = 32 + (°C x (9/5))"), 
                                     tags$li(tags$b("Velocity:"), "checks fpm = 196.85*(m/s)"),
                                     style = "background: aliceblue"),
                           h4("Temperature"),
                           tableOutput("temp_conv"),
                           h4("Velocity"),
                           tableOutput("velocity_conv")),
                  tabPanel("Calculated Indices", h3("Formatting checks for Calculated Indices"), 
                           wellPanel(tags$b("Checking Criteria"),
                                     p("All values should be NA or:"),
                                     tags$li(tags$b("PMV:"), "Numeric value between -3 and 3"), 
                                     tags$li(tags$b("PPD:"), "Numeric value between 0 and 100"), 
                                     tags$li(tags$b("SET:"), "Numeric value between -17°C and 50°C"),
                                     style = "background: aliceblue"),
                                                          fluidRow(tableOutput("PMV"),
                                                          tableOutput("PPD"),
                                                          tableOutput("SET"))),
                  tabPanel("Environmental Control",h3("Formatting checks for Environmental Control Variables"), 
                           wellPanel(tags$b("Checking Criteria"),
                                     p("All values should be 0, 1 or NA"),
                                     style = "background: aliceblue"),
                           fluidRow(tableOutput("Blind"), 
                              tableOutput("Fan"), 
                              tableOutput("Window"),
                              tableOutput("Door"),
                              tableOutput("Heater"))))
                  
      
      
      
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  
  
  output$formatting <- renderText({
    
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F )
    
    #checks for correct number of columns
    if(ncol(df1) != 71){
      return("File does not have the correct number of columns. Please check with provided template sheet on the left.")
    }
    
    col_names <- names(df1)
    
    sub <- read_csv("./Data/submission_template.csv")
    
    if(!all(col_names == names(sub))){
      return("File does not have the correct name columns. Please check with provided template sheet on the left or check that file has UTF-8 encoding.")
      
    }
    
    return("Formatting: passed.")
    
  })
  
  output$names <- renderTable({
    
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F )
    
    col_names <- names(df1)
    
    sub <- read_csv("./Data/submission_template.csv")
    sub_names <- names(sub)
    wrong <- subset(col_names, !(col_names %in% sub_names))
  
    if (length(wrong) > 0){
      index <- which(col_names %in% wrong) 
      expected <- sub_names[index]
      
      df <- data.frame(wrong, expected)
      names(df) <- c("Entered Column Name", "Expected")
      return(df)
    }else{
      return(NULL)
    }
    
    
    
  })
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
  
  
  
  output$downloadTemp <- downloadHandler(
    
    filename <- "submission_template.csv",
    
    content <- function(file){
      file.copy("./Data/submission_template.csv", file)
    }
  )
  
  output$Contributor <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    if(all(is.na(df1$`Data contributor`)) == T){
      return(na_message("Data contributor"))
    }
    
    check <- check_contributor(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Contributor: passed"))
      names(message) <- c("")
      return(message)
    }
    
    
    
    
  })
  
  
  output$Year <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    if(all(is.na(df1$`Year`)) == T){
      return(na_message("Year"))
    }
    
    check <- check_year(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Year: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Season <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    
    if(all(is.na(df1$`Season`)) == T){
      return(na_message("Season"))
    }
    
    check <- check_season(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Season: passed"))
      names(message) <- c("")
      return(message)
    }
  })
    
  output$Koppen <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F )
    
    if(all(is.na(df1$`Koppen climate classification`)) == T){
      return(na_message("Koppen climate classification"))
    }
    
    check <- check_koppen2(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Koppen: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Climate <- renderTable({
    # Edit: no check climate
    # req(input$file1)
    # 
    # df1 <- read.csv(input$file1$datapath, sep = ",",
    #                 header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    # col <- df1$`Climate`
    # if(all(is.na(col)) == T){
    #   return(na_message("Climate"))
    # }
    # check <- check_climate(df1)
    # 
    # if(is.data.frame(check)){
    #   return(check)
    # }else{
    #   message <- data.frame(c("Climate: passed"))
    #   names(message) <- c("")
    #   return(message)
    # }
  })
  
  output$Country <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$Country
    if(all(is.na(col)) == T){
      return(na_message("Country"))
    }
    
    check <- check_country(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Country: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  #note: countries must be correct for cities to work.
  output$City <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$City
    if(all(is.na(col)) == T){
      return(na_message("City"))
    }
    
    check <- check_city(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("City: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$BuildingType <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    col <- df1$`Building type`
    if(all(is.na(col)) == T){
      return(na_message("Building Type"))
    }
    
    check <- check_building(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Building Type: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Cooling <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$`Cooling startegy_building level`
    if(all(is.na(col)) == T){
      return(na_message("Cooling Strategy"))
    }
    
    check <- check_cooling(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Cooling strategy: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$CoolingMM <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$`Cooling startegy_operation mode for MM buildings`
    if(all(is.na(col)) == T){
      return(na_message("Cooling Strategy with MM"))
    }
    
    check <- check_cooling_mm(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Cooling Strategy with MM: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Heating <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$`Heating strategy_building level`
    if(all(is.na(col)) == T){
      return(na_message("Heating strategy"))
    }
    
    check <- check_heating(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Heating Strategy: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Age <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$`Age`
    print(all(is.na(col)))
    if(all(is.na(col)) == T){
      return(na_message( "Age"))
    }
    
    check <- check_age(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Age: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  
  output$Sex <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$`Sex`
    if(all(is.na(col)) == T){
      return(na_message( "Sex"))
    }
    
    check <- check_sex(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Sex: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Weight <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$`Subject´s weight (kg)`
    if(all(is.na(col)) == T){
      return(na_message("Weight"))
    }
    
    check <- check_weight(df1)
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Weight: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Height <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F  )
    
    col <- df1$`Subject´s height (cm)`
    if(all(is.na(col)) == T){
      return(na_message("Height"))
    }
    
    check <- check_height(df1)
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Height: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  #Thermal sensation
  
  output$T_sensation_f <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    
    col <- df1$`Thermal sensation`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("Thermal sensation : All NA"))
      names(message) <- c("")
      return(message)
    }
    #check thermal sensation values are between -3 and 3
    check <- check_range_R(col, -3, 3, "Thermal sensation")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Thermal sensation formatting: passed"))
      names(message) <- c("")
      return(message)
    }
      
    
  })
  
  output$T_sensation <- renderText({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    check <- thermal_sensation_R(df1)
    
    if(is.na(check)){
      return("Thermal sesnation: No applicable reasonability test.")
    }else if(check == F){
      return("Thermal sensation: Study did not pass reasonability test. Please check encoding.")
    }else{
      return("Thermal sensation: Study passes reasonability test.")
    }
    
  })
  
  output$T_sensation_g <- renderPlot({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    
    check <- thermal_sensation_R(df1)
    
    if(is.na(check)){
      return(NULL)
    }
    
    df <- subset(df1, !(is.na(`Thermal sensation`)))
    names(df) <- as.character(1:length(names(df)))
    avgtemp <- df %>% group_by(`32`) %>% 
      summarise(averagetemp = mean(`16`)) %>% 
      as.data.frame
    
    
    
    ggplot(avgtemp, aes(x =`32`, y = averagetemp)) + geom_point() + 
      ylab("Average thermal sensation at recorded temperatures") + ggtitle("Thermal sensation vs. Temperature") + 
      xlab("Temperature (F)") +
      geom_smooth(method = "lm") 
    
  })
  
  output$T_acceptability_f <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    
    col <- df1$`Thermal sensation acceptability`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("Thermal acceptability : All NA"))
      names(message) <- c("")
      return(message)
    }
    #thermal acceptability is either 0, 1 or NA
    check <- check_factor(col, c(0, 1), "Thermal acceptability")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Thermal acceptability formatting: passed"))
      names(message) <- c("")
      return(message)
    }
    
  })
  
  output$T_acceptability <- renderText({
    
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    check <- test_thermal_acceptibility_R(df1)
    
    if(is.na(check)){
      return("Thermal acceptability: No applicable reasonability test.")
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
    names(df1)[1] <- "Index"
    check <- test_thermal_acceptibility_R(df1)
    if(is.na(check)){
      return(NULL)
    }
    
    ggplot(df1, aes(x =round(`Thermal sensation`))) + geom_bar(aes(fill = factor(`Thermal sensation acceptability`)), position = 'fill') +
       xlab("Binned Thermal Sensation") + ylab("Proportion") + scale_fill_discrete(name = "Thermal acceptability legend")
  })
  
  output$T_comfort_f <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    
    col <- df1$`Thermal comfort`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("Thermal comfort : All NA"))
      names(message) <- c("")
      return(message)
    }
    #thermal comfort values are between 1 and 6
    check <- check_range_R(col, 1, 6, "Thermal comfort")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Thermal comfort formatting: passed"))
      names(message) <- c("")
      return(message)
    }
    
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
    check <- test_thermal_comfort_R(df1)
    if(is.na(check)){
      return(NULL)
    }
    df <- subset(df1, !(is.na(`Thermal comfort`)))
    ggplot(df, aes(x = round(`Thermal sensation`), y = rep(1, times = nrow(df)))) + 
      #geom_point()
      geom_bar(aes(fill =  factor(cut(as.numeric(`Thermal comfort`), breaks = c(0,3.5, 6)))), position = "fill", stat = 'identity') + 
      xlab("Thermal sensation") + 
      ylab("Proportion Thermal comfort") + 
      scale_fill_discrete(name = "Key Thermal Comfort") 
    
    
  })
  
  
  output$T_preference_f <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    
    col <- df1$`Thermal preference`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("Thermal preference : All NA"))
      names(message) <- c("")
      return(message)
    }
    
    check <- check_factor(col, c("no change", "cooler", "warmer"), "Thermal preference")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Thermal preference formatting: passed"))
      names(message) <- c("")
      return(message)
    }
    
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
      check <- test_thermal_preference_R(df1)
      if(is.na(check)){
        return(NULL)
      }
      
      df <- subset(df1, !(is.na(`Thermal preference`)))
      
      ggplot(df, aes(x = round(`Thermal sensation`), y = rep(1, times = nrow(df)))) + 
        geom_bar(aes(fill = factor(`Thermal preference`, levels = c("warmer", "no change", "cooler"))), position = "fill", stat = "identity") + 
        xlab("Rounded Thermal Sensation") + 
        ylab("Proportion - Thermal Preference") + 
        scale_fill_discrete(name = "Thermal preference legend") 
      
      
    })
    
    
    output$A_preference_f <- renderTable({
      
      df1 <- read.csv(input$file1$datapath, sep = ",",
                      header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
      
      col <- df1$`Air movement preference`
      
      if(all(is.na(col)) == T){
        message <- data.frame(c("Air movement preference : All NA"))
        names(message) <- c("")
        return(message)
      }
      
      check <- check_factor(col, c("less", "more", "no change"), "Air movement preference")
      
      
      
      if(is.data.frame(check)){
        return(check)
      }else{
        message <- data.frame(c("Air movement preference formatting: passed"))
        names(message) <- c("")
        return(message)
      }
      
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
    
    if(nrow(df) == 0){
      return(NULL)
    }
    
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
  
  output$A_acceptability_f <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    
    col <- df1$`Air movement acceptability`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("Air movement acceptability : All NA"))
      names(message) <- c("")
      return(message)
    }
    
    check <- check_factor(col, c(0,1), "Air movement acceptability")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Air movement acceptability formatting: passed"))
      names(message) <- c("")
      return(message)
    }
    
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
  
  output$H_sensation_f <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    
    col <- df1$`Humidity sensation`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("Humidity sensation : All NA"))
      names(message) <- c("")
      return(message)
    }
    
    check <- check_range_R(col, -3,3, "Humidity sensation")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Humidity sensation formatting: passed"))
      names(message) <- c("")
      return(message)
    }
    
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
  
  output$H_preference_f <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    
    col <- df1$`Humidity preference`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("Humidity preference : All NA"))
      names(message) <- c("")
      return(message)
    }
    
    check <- check_factor(col, c("drier", "no change", "more humid"), "Humidity preference")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Humidity preference formatting: passed"))
      names(message) <- c("")
      return(message)
    }
    
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
    check <- check_temp_R(temps, -17, 50, "Air Temperature (°C)")
    
  
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Air Temperature (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Air Temperature (°F) ")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Air Temperature (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -17, 50, "Ta_h (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Ta_h (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Ta_h (°F)")
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Ta_h (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -17, 50, "Ta_m (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Ta_m (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -17, 50, "Ta_l (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Ta_l (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Ta_m (°F)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Ta_m (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Ta_l (°F)")
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Ta_l (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -17, 50, "Tg_m (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Tg_m (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -17, 50, "Tg_l (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Tg_l (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -17, 50, "Tg_h (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Tg_h (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Tg_m (°F)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Tg_m (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Tg_l (°F)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Tg_l (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Tg_h (°F)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Tg_h (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -60, 140, "Outdoor monthly air temperature (°F)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Outdoor Monthly Air Temperature (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -50, 60, "Outdoor monthly air temperature (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Outdoor Monthly Air Temperature (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Operative Temperature (°F)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Operative Temperature (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -7, 50, "Operative Temperature (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Operative Temperature (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Radiant temperature (°F)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Radiant Temperature (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -7, 50, "Radiant temperature (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Radiant Temperature (°C): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, 0, 120, "Globe temperature (°F)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Globe Temperature (°F): passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_temp_R(temps, -7, 50, "Globe Temperature (°C)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Globe Temperature (°C): passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Relative_humidity <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    col <- df1$`Relative humidity (%)`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("Relative Humidity (%) : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_range_R(col, 0, 100, "Relative Humidity (%)")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Relative Humidity (%): passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$Clo <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    clo <- df1$Clo
    
    if(all(is.na(clo)) == T){
      message <- data.frame(c("Clo : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_range_R(clo, 0, 5, "Clo")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Clo: passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_range_R(met, 0, 10, "Met")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Met: passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_range_R(met, 0, 10, "activity_10")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("activity_10 : passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_range_R(met, 0, 10, "activity_20")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("activity_20: passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_range_R(met, 0, 10, "activity_30")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("activity_30: passed"))
      names(message) <- c("")
      return(message)
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
    check <- check_range_R(met, 0, 10, "activity_60")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("activity_60: passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Air velocity (m/s): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Air velocity (fpm): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Velocity_h (m/s): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Velocity_h (fpm): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Velocity_m (m/s): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Velocity_m (fpm): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Velocity_l (m/s): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Velocity_l (fpm): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Blind (curtain): passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Fan: passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Window: passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Heater: passed"))
      names(message) <- c("")
      return(message)
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
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("Door: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$PMV <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    col <- df1$`PMV`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("PMV : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_range_R(col, -3, 3, "PMV")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("PMV: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$PPD <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    col <- df1$`PPD`
    
    if(all(is.na(col)) == T){
      message <- data.frame(c("PPD : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_range_R(col, 0, 100, "PPD")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("PPD: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$SET <- renderTable({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    temps <- df1$`SET`
    
    if(all(is.na(temps)) == T){
      message <- data.frame(c("SET : All NA"))
      names(message) <- c("")
      return(message)
    }
    check <- check_temp_R(temps, -17, 50, "SET")
    
    
    
    if(is.data.frame(check)){
      return(check)
    }else{
      message <- data.frame(c("SET: passed"))
      names(message) <- c("")
      return(message)
    }
  })
  
  output$temp_conv <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    #list all wrong temperature conversions
    t1 <- check_conversion(df1$`Air temperature (°C)`, df1$`Air temperature (°F)`, mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Air Temperature")
    t2 <- check_conversion(df1$`Ta_h (°C)`, df1$`Ta_h (°F)`, mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Ta_h")
    t3 <- check_conversion(df1$`Ta_m (°C)`, df1$`Ta_m (°F)`,  mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Ta_m")
    t4 <- check_conversion(df1$`Ta_l (°C)`, df1$`Ta_l (°F)`,  mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Ta_l")
    t5 <- check_conversion(df1$`Operative temperature (°C)`, df1$`Operative temperature (°F)`, mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Operative Temperature")
    t6 <- check_conversion(df1$`Radiant temperature (°C)`, df1$`Radiant temperature (°F)`, mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Radiant Temperature")
    t7 <- check_conversion(df1$`Globe temperature (°C)`, df1$`Globe temperature (°F)`, mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Globe Temperature")
    t8 <- check_conversion(df1$`Tg_h (°C)`, df1$`Tg_h (°F)`,  mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Tg_h")
    t9 <- check_conversion(df1$`Tg_m (°C)`, df1$`Tg_m (°F)`, mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Tg_m")
    t10 <- check_conversion(df1$`Tg_l (°C)`, df1$`Tg_l (°F)`, mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Tg_l")
    t11 <- check_conversion(df1$`Outside monthly air temperature (°C)`, df1$`Outside monthly air temperature (°F)`, mult = 9/5, const = 32, accuracy = 1, names = c("°C", "°F"), source = "Outside Monthly Air Temperature")
    
    #create a list of potentially wrong conversions
    tables <- list(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
    #if element is a dataframe, then there exists wrong conversions
    wrong <- which(unlist(lapply(tables, is.data.frame)))
    
    
    if(length(wrong) > 0){
      frame <- tables[[wrong[1]]]
    }
    
    #combine all data frames
    if(length(wrong) > 1){
      for (elem in wrong[-1]){
        frame <- rbind(frame, tables[[elem]])
        
      }
    }
    
    if(length(wrong) == 0){
      message <- data.frame(c("All temperature conversions passing."))
      names(message) <- c("")
      return(message)
    }else{
      return(frame)
    }
        
    
  })
  
  output$velocity_conv <- renderTable({
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = F)
    
    #list all wrong velocity conversions
    t1 <- check_conversion(df1$`Air velocity (m/s)`, df1$`Air velocity (fpm)`, mult = 196.85, accuracy = 2, names = c("m/s", "fpm"), source = "Air Velocity")
    t2 <- check_conversion(df1$`Velocity_h (m/s)`, df1$`Velocity_h (fpm)`, mult = 196.85, accuracy = 2, names = c("m/s", "fpm"), source = "Velocity_h")
    #create a list of potentially wrong conversions
    tables <- list(t1, t2)
    #if element is a dataframe, then there exists wrong conversions
    wrong <- which(unlist(lapply(tables, is.data.frame)))
    
    if(length(wrong) > 0){
    frame <- tables[[wrong[1]]]
    }
    
    #combine all data frames
    if(length(wrong) > 1){
      for (elem in wrong[-1]){
        frame <- rbind(frame, tables[[elem]])
        
      }
    }
    
    if(length(wrong) == 0){
      message <- data.frame(c("All velocity conversions passing."))
      names(message) <- c("")
      return(message)
    }else{
      return(frame)
    }
    
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)