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
                  tabPanel("Instrumented Measurements", fluidRow()), 
                  tabPanel("Calculated Indices", fluidRow()),
                  tabPanel("Environmental Control"))
                  
      
      
      
      
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
  
  output$A_preference <- renderText({
    req(input$file1)
    
    df1 <- read.csv(input$file1$datapath, sep = ",",
                    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE )
    names(df1)[1] <- "Index"
    
    check <- test_air_preference_R(df1)
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)