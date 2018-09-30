library(shiny)
library(shinyjs)
library(plotly)
source("funcs.R")
#csv file with Koppen climate names and codes
climate_names <- read.csv("kfc_climates.csv", header = TRUE)

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
                  tabPanel("Basic Identifiers", fluidRow(tableOutput("Contributor"), 
                           tableOutput("Year"), 
                           tableOutput("Season"),
                           tableOutput("Koppen"), 
                           tableOutput("Climate"), 
                           tableOutput("Country"), 
                           tableOutput("City"), 
                           tableOutput("Building Type"), 
                           tableOutput("Cooling Strategy"), 
                           tableOutput("Cooling Strategy with MM"), 
                           tableOutput("Heating Strategy"))), 
                  tabPanel("Subjects' Information", fluidRow(tableOutput("Age"),
                             tableOutput("Sex"), 
                             tableOutput("Weight"),
                             tableOutput("Height")
                             
                           )), 
                  tabPanel("Subjective Comfort Data", fluidRow()), 
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
                   header = TRUE, stringsAsFactors = FALSE )
    
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
  
}

# Create Shiny app ----
shinyApp(ui, server)