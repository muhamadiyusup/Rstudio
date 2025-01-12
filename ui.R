# ui.R
library(shiny)
library(shinythemes)
library(DT)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Mortality Rate Analysis - MAPE Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataSource", 
                   "Select Data Source:",
                   choices = c("HMD Database" = "hmd",
                               "Upload CSV" = "csv"),
                   selected = "hmd"),
      
      conditionalPanel(
        condition = "input.dataSource == 'hmd'",
        selectInput("countrySelect",
                    "Select Country:",
                    choices = NULL),
        radioButtons("gender", 
                     "Select Gender:",
                     choices = c("Female", "Male"),
                     selected = "Female"),
        actionButton("calculate", 
                     "Calculate HMD MAPE", 
                     class = "btn-primary",
                     style = "margin-top: 10px")
      ),
      
      conditionalPanel(
        condition = "input.dataSource == 'csv'",
        fileInput("csvFile", "Upload CSV File",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
        ),
        helpText("CSV should contain columns: Year, Age, Gender, Rate"),
        radioButtons("genderCSV", 
                     "Select Gender:",
                     choices = c("Female", "Male"),
                     selected = "Female"),
        tags$hr(),
        actionButton("calculateCSV", 
                     "Calculate CSV MAPE", 
                     class = "btn-success",
                     style = "margin-top: 10px")
      ),
      
      tags$hr(),
      tags$div(
        class = "well",
        tags$h4("Download Results:"),
        downloadButton("downloadCSV", "Download CSV"),
        tags$br(), tags$br(),
        downloadButton("downloadExcel", "Download Excel")
      ),
      
      tags$hr(),
      tags$div(
        class = "alert alert-info",
        tags$b("Note:"),
        "For HMD data source, this application uses data from the Human Mortality Database (mortality.org)"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 h4("Data Preview"),
                 DTOutput("dataPreview"),
                 tags$br(),
                 tags$div(
                   class = "alert alert-info",
                   tags$b("Data Format:"),
                   tags$ul(
                     tags$li("Age: Age in years"),
                     tags$li("Year: Calendar year"),
                     tags$li("Gender: Female/Male"),
                     tags$li("Mortality_Rate: Death rate for the specific age-year combination")
                   )
                 )),
        
        tabPanel("MAPE Results",
                 h4("Model Comparison Results"),
                 DTOutput("mapeTable"),
                 tags$hr(),
                 plotOutput("mapeBarplot", height = "400px"),
                 tags$hr(),
                 verbatimTextOutput("modelSummary")),
        
        tabPanel("Best Model Mortality Rates",
                 h4("Mortality Rates from Best Performing Model"),
                 selectInput("yearSelect", "Select Year:",
                             choices = NULL),
                 DTOutput("mortalityTable")),
        
        tabPanel("Model Information",
                 h4("About the Models"),
                 tags$div(
                   tags$h5("Basic Models:"),
                   tags$p("1. Lee-Carter (LC): A classic model that uses age-specific patterns and time trends."),
                   tags$p("2. Renshaw-Haberman (RH): Extends LC by including cohort effects."),
                   tags$p("3. Cairns-Blake-Dowd (CBD): Designed for higher age mortality modeling."),
                   tags$br(),
                   tags$h5("Tree-Enhanced Models:"),
                   tags$p("4. LCDT: LC model improved with decision trees"),
                   tags$p("5. RHDT: RH model improved with decision trees"),
                   tags$p("6. CBDDT: CBD model improved with decision trees")
                 ))
      )
    )
  )
)