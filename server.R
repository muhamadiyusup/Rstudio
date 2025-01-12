library(shiny)
library(demography)
library(StMoMo)
library(rpart)
library(ggplot2)
library(MLmetrics)
library(DT)
library(dplyr)
library(writexl)

# Tambahkan fungsi ini
preprocess_mortality_data <- function(data) {
  # Convert rates yang terlalu kecil
  min_threshold <- 0.0001  # threshold untuk nilai minimum yang masuk akal
  data$Rate <- ifelse(data$Rate < min_threshold, min_threshold, data$Rate)
  
  return(data)
}

# Tambahkan fungsi custom MAPE
calculate_custom_mape <- function(actual, predicted) {
  # Calculate MAPE with handling for small values
  mape <- mean(abs((actual - predicted) / pmax(actual, 0.0001))) * 100
  return(mape)
}

server <- function(input, output, session) {
  
  # List of available countries with their codes
  country_list <- list(
    "Australia" = "AUS",
    "Austria" = "AUT",
    "Belarus" = "BLR",
    "Belgium" = "BEL",
    "Bulgaria" = "BGR",
    "Canada" = "CAN",
    "Chile" = "CHL",
    "Croatia" = "HRV",
    "Czechia" = "CZE",
    "Denmark" = "DNK",
    "Estonia" = "EST",
    "Finland" = "FIN",
    "France" = "FRATNP",
    "Germany" = "DEUTNP",
    "Greece" = "GRC",
    "Hungary" = "HUN",
    "Iceland" = "ISL",
    "Ireland" = "IRL",
    "Israel" = "ISR",
    "Italy" = "ITA",
    "Japan" = "JPN",
    "Latvia" = "LVA",
    "Lithuania" = "LTU",
    "Luxembourg" = "LUX",
    "Netherlands" = "NLD",
    "New Zealand" = "NZL_NP",
    "Norway" = "NOR",
    "Poland" = "POL",
    "Portugal" = "PRT",
    "Russia" = "RUS",
    "Slovakia" = "SVK",
    "Slovenia" = "SVN",
    "Spain" = "ESP",
    "Sweden" = "SWE",
    "Switzerland" = "CHE",
    "Taiwan" = "TWN",
    "UK" = "GBR_NP",
    "USA" = "USA"
  )
  
  
  # Reactive values to store data and results
  rv <- reactiveValues(
    mortality_data = NULL,
    mape_results = NULL,
    best_model_rates = NULL,
    best_model_name = NULL,
    years = NULL,
    available_years = NULL
  )
  
  
  # Update country choices in UI
  observe({
    updateSelectInput(session, "countrySelect",
                      choices = names(country_list),
                      selected = "Japan")
  })
  
  
  # Update year choices once data is loaded
  observe({
    if(!is.null(rv$years)) {
      updateSelectInput(session, "yearSelect",
                        choices = rv$years)
    }
  })
  
  
  # Helper function to calculate improvements
  calculate_improvements <- function(mape_results) {
    # Convert MAPE to percentage
    mape_results$MAPE_Percentage <- mape_results$MAPE * 100
    
    
    # Calculate base model references
    lc_base <- mape_results$MAPE_Percentage[mape_results$Model == "LC"]
    rh_base <- mape_results$MAPE_Percentage[mape_results$Model == "RH"]
    cbd_base <- mape_results$MAPE_Percentage[mape_results$Model == "CBD"]
    
    
    # Calculate improvements
    mape_results$Improvement <- NA
    mape_results$Improvement[mape_results$Model == "LCDT"] <- 
      ((lc_base - mape_results$MAPE_Percentage[mape_results$Model == "LCDT"]) / lc_base) * 100
    mape_results$Improvement[mape_results$Model == "RHDT"] <- 
      ((rh_base - mape_results$MAPE_Percentage[mape_results$Model == "RHDT"]) / rh_base) * 100
    mape_results$Improvement[mape_results$Model == "CBDDT"] <- 
      ((cbd_base - mape_results$MAPE_Percentage[mape_results$Model == "CBDDT"]) / cbd_base) * 100
    
    
    return(mape_results)
  }
  
  
  # React to Calculate button click
  observeEvent(input$calculate, {
    withProgress(message = 'Processing data...', value = 0, {
      
      tryCatch({
        # Get HMD data for selected country
        incProgress(0.1, detail = paste("Accessing", input$countrySelect, "mortality data"))
        country_code <- country_list[[input$countrySelect]]
        mortality_data <- tryCatch({
          hmd.mx(country = country_code,
                 username = "muhamadiyusup@gmail.com",
                 password = "Paser001@")
        }, error = function(e) {
          showNotification(paste("Error accessing data for", input$countrySelect, ":", e$message),
                           type = "error",
                           duration = NULL)
          return(NULL)
        })
        
        if (is.null(mortality_data)) {
          return()
        }
        
        rv$mortality_data <- mortality_data
        
        # Get available years for the selected country
        available_years <- as.numeric(colnames(mortality_data$rate$female))
        rv$available_years <- available_years
        
        
        # Update year range inputs
        start_year <- min(available_years)
        end_year <- max(available_years)
        rv$years <- start_year:end_year
        
        
        updateNumericInput(session, "yearStart",
                           value = start_year,
                           min = start_year,
                           max = end_year)
        updateNumericInput(session, "yearEnd",
                           value = end_year,
                           min = start_year,
                           max = end_year)
        
        # Process based on gender selection
        
        Japan_data <- if(input$gender == "Female") {
          StMoMoData(Japan, series="female")
        } else {
          StMoMoData(Japan, series="male")
        }
        
        
        # Calculate crude rates
        crude.rates <- Japan_data$Dxt[c(1:101),c(0:71)]/Japan_data$Ext[c(1:101),c(0:71)]
        
        
        # Lee-Carter model
        incProgress(0.3, detail = "Fitting LC model")
        LC <- lc(link="log")
        LC <- fit(LC, data = Japan_data, years.fit=1947:2017, ages.fit = 0:100)
        m.LC <- fitted(LC, type="rates")
        
        
        # Renshaw-Haberman model
        incProgress(0.4, detail = "Fitting RH model")
        RH <- rh(link="log")
        RH <- fit(RH, data = Japan_data, years.fit=1947:2017, ages.fit = 0:100)
        m.RH <- fitted(RH, type="rates")
        
        
        # CBD model
        incProgress(0.5, detail = "Fitting CBD model")
        CBD <- m6(link="log")
        CBD <- fit(CBD, data = Japan_data, years.fit=1947:2017, ages.fit = 0:100)
        m.CBD <- fitted(CBD, type="rates")
        
        
        # LC with Decision Tree
        incProgress(0.6, detail = "Applying decision trees")
        dx.LC <- m.LC * Japan_data$Ext[c(1:101),c(0:71)]
        dx <- Japan_data$Dxt[c(1:101),c(0:71)]
        years <- rep(1947:2017, each=101)
        ages <- rep(0:100, 71)
        cohort <- years - ages
        
        
        tree <- rpart(cbind(dx.LC, dx) ~ ages + years + cohort,
                      data = data.frame(dx = as.vector(dx),
                                        dx.LC = as.vector(dx.LC),
                                        ages = ages,
                                        years = years,
                                        cohort = cohort),
                      method = "poisson", cp = 2e-3, control = rpart.control(minbucket = 30))
          
        mu <- predict(tree)
        m.LCDT <- matrix(mu * as.vector(m.LC), nrow=101)
        
        
        # RH with Decision Tree
        incProgress(0.7, detail = "Improving RH model")
        dx.RH <- m.RH * Japan_data$Ext[c(1:101),c(0:71)]
        tree2 <- rpart(cbind(dx.RH, dx) ~ ages + years + cohort,
                       data = data.frame(dx = as.vector(dx),
                                         dx.RH = as.vector(dx.RH),
                                         ages = ages,
                                         years = years,
                                         cohort = cohort),
                       method = "poisson", cp = 2e-3)
        mu2 <- predict(tree2)
        m.RHDT <- matrix(mu2 * as.vector(m.RH), nrow=101)
        
        
        # CBD with Decision Tree
        incProgress(0.8, detail = "Improving CBD model")
        dx.CBD <- m.CBD * Japan_data$Ext[c(1:101),c(0:71)]
        tree3 <- rpart(cbind(dx.CBD, dx) ~ ages + years + cohort,
                       data = data.frame(dx = as.vector(dx),
                                         dx.CBD = as.vector(dx.CBD),
                                         ages = ages,
                                         years = years,
                                         cohort = cohort),
                       method = "poisson", cp = 2e-3)
        mu3 <- predict(tree3)
        m.CBDDT <- matrix(mu3 * as.vector(m.CBD), nrow=101)
        
        
        # Calculate MAPE
        incProgress(0.9, detail = "Calculating MAPE values")
        MAPE_LC <- MAPE(y_pred = m.LC, y_true = crude.rates)
        MAPE_RH <- MAPE(y_pred = m.RH, y_true = crude.rates)
        MAPE_CBD <- MAPE(y_pred = m.CBD, y_true = crude.rates)
        MAPE_LCDT <- MAPE(y_pred = m.LCDT, y_true = crude.rates)
        MAPE_RHDT <- MAPE(y_pred = m.RHDT, y_true = crude.rates)
        MAPE_CBDDT <- MAPE(y_pred = m.CBDDT, y_true = crude.rates)
        
        # Store MAPE results
        rv$mape_results <- data.frame(
          Model = c("LC", "RH", "CBD", "LCDT", "RHDT", "CBDDT"),
          MAPE = c(MAPE_LC, MAPE_RH, MAPE_CBD, MAPE_LCDT, MAPE_RHDT, MAPE_CBDDT)
        )
        
        
        # Find best model and store its rates
        best_model_idx <- which.min(rv$mape_results$MAPE)
        rv$best_model_name <- rv$mape_results$Model[best_model_idx]
        
        
        # Get best model rates
        best_rates <- switch(rv$best_model_name,
                             "LC" = m.LC,
                             "RH" = m.RH,
                             "CBD" = m.CBD,
                             "LCDT" = m.LCDT,
                             "RHDT" = m.RHDT,
                             "CBDDT" = m.CBDDT)
        
        
        # Store best model rates
        rv$best_model_rates <- data.frame(
          Age = rep(0:100, ncol(best_rates)),
          Year = rep(1947:2017, each = nrow(best_rates)),
          Mortality_Rate = as.vector(best_rates)
        )
        
        
        incProgress(1.0, detail = "Complete!")
      }, error = function(e) {
        showNotification(paste("Error in processing:", e$message),
                         type = "error",
                         duration = NULL)
      })
    })
  })
  
  
  # Render MAPE table
  output$mapeTable <- renderDT({
    req(rv$mape_results)
    results_with_pct <- calculate_improvements(rv$mape_results)
    
    display_data <- data.frame(
      Model = results_with_pct$Model,
      'MAPE (%)' = sprintf("%.4f%%", results_with_pct$MAPE_Percentage), # Update format presisi
      'Improvement' = ifelse(!is.na(results_with_pct$Improvement),
                             sprintf("%.4f%%", results_with_pct$Improvement),
                             "-")
    )
    
    datatable(display_data,
              options = list(pageLength = 10,
                             searching = FALSE),
              rownames = FALSE)
  })
  
  
  # Render MAPE barplot
  output$mapeBarplot <- renderPlot({
    req(rv$mape_results)
    results_with_pct <- calculate_improvements(rv$mape_results)
    
    ggplot(results_with_pct, aes(x = Model, y = MAPE_Percentage)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = sprintf("%.2f%%", MAPE_Percentage)),
                vjust = -0.5) +
      geom_text(aes(label = ifelse(!is.na(Improvement), 
                                   sprintf("↓%.2f%%", Improvement),
                                   "")),
                vjust = -2.0,
                color = "darkgreen") +
      theme_minimal() +
      labs(title = paste("MAPE Comparison for", input$gender),
           x = "Model",
           y = "MAPE (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, max(results_with_pct$MAPE_Percentage) * 1.2)
  })
  
  
  # Render model summary
  output$modelSummary <- renderPrint({
    req(rv$mape_results)
    
    cat("Analysis Summary:\n\n")
    cat("Gender:", input$gender, "\n")
    cat("Years: 1947-2017\n")
    cat("Age Range: 0-100\n\n")
    
    best_model <- rv$mape_results[which.min(rv$mape_results$MAPE), ]
    cat("Best performing model:", best_model$Model, "\n")
    cat("Best MAPE value:", sprintf("%.2f%%", best_model$MAPE * 100), "\n\n")
    
    cat("Model Rankings:\n")
    results_ordered <- rv$mape_results[order(rv$mape_results$MAPE), ]
    results_ordered$MAPE <- sprintf("%.2f%%", results_ordered$MAPE * 100)
    print(results_ordered)
  })
  
  
  # Render mortality rates table
  output$mortalityTable <- renderDT({
    req(rv$best_model_rates, input$yearSelect)
    
    
    year_data <- rv$best_model_rates[rv$best_model_rates$Year == input$yearSelect, ]
    
    
    datatable(
      data.frame(
        Age = year_data$Age,
        'Mortality Rate' = sprintf("%.6f", year_data$Mortality_Rate)
      ),
      options = list(
        pageLength = 25,
        searching = TRUE
      ),
      rownames = FALSE
    )
  })
  
  
  # Data preview untuk CSV
  output$dataPreview <- renderDT({
    if(input$dataSource == "csv" && !is.null(input$csvFile)) {
      df <- read.csv(input$csvFile$datapath)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  
  # Function to clean CSV data
  clean_csv_data <- function(data_string) {
    # Split data by space and create matrix
    lines <- strsplit(data_string, " ")[[1]]
    # First line is header
    header <- strsplit(lines[1], ",")[[1]]
    
    # Process remaining lines
    data_lines <- lines[-1]
    data_matrix <- do.call(rbind, lapply(data_lines, function(x) {
      if(x != "") {
        as.character(strsplit(x, ",")[[1]])
      }
    }))
    
    
    # Convert to data frame
    data <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
    colnames(data) <- header
    
    # Convert types
    data$Year <- as.numeric(data$Year)
    data$Age <- as.numeric(data$Age)
    data$Rate <- as.numeric(data$Rate)
    
    # Add preprocessing
    data <- preprocess_mortality_data(data)
    
    return(data)
  }
  
  
  # Handler untuk tombol Calculate CSV MAPE
  observeEvent(input$calculateCSV, {
    req(input$csvFile)
    
    
    withProgress(message = 'Processing CSV data...', value = 0, {
      tryCatch({
        # Read raw CSV content
        raw_content <- readLines(input$csvFile$datapath, warn = FALSE)
        raw_content <- paste(raw_content, collapse = " ")
        
        # Clean and process data
        data <- clean_csv_data(raw_content)
        showNotification("CSV data cleaned and processed", type = "message")
        
        
        # Filter data berdasarkan gender yang dipilih
        data_filtered <- data[data$Gender == input$genderCSV, ]
        
        
        if(nrow(data_filtered) == 0) {
          showNotification(paste("No data found for gender:", input$genderCSV),
                           type = "error")
          return()
        }
        
        # Siapkan matriks
        years <- sort(unique(data_filtered$Year))
        ages <- sort(unique(data_filtered$Age))
        n_ages <- length(ages)
        n_years <- length(years)
        
        
        incProgress(0.2, detail = "Creating demogdata object...")
        
        
        # Initialize matrices
        rate_matrix <- matrix(0, nrow = n_ages, ncol = n_years,
                              dimnames = list(ages, years))
        exposure_matrix <- matrix(1000, nrow = n_ages, ncol = n_years,
                                  dimnames = list(ages, years))
        
        
        # Fill rate matrix
        for(i in 1:nrow(data_filtered)) {
          y_idx <- which(years == data_filtered$Year[i])
          a_idx <- which(ages == data_filtered$Age[i])
          rate_matrix[a_idx, y_idx] <- data_filtered$Rate[i]
        }
        
        
        # Create demogdata object
        demog_data <- list(
          pop = list(
            female = exposure_matrix,
            male = exposure_matrix
          ),
          rate = list(
            female = rate_matrix,
            male = rate_matrix
          ),
          years = years,
          ages = ages,
          type = "mortality",
          label = "mortality data",
          name = "custom"
        )
        class(demog_data) <- "demogdata"
        
        
        # Convert to StMoMoData
        mortality_data <- StMoMoData(demog_data, series = tolower(input$genderCSV))
        
        
        # Create mortality matrix for MAPE calculations
        mortality_matrix <- rate_matrix  # Original matrix
        exposure_matrix <- exposure_matrix
        
        # Preprocess mortality matrix for model fitting
        mortality_matrix <- pmax(mortality_matrix, 0.0001)
        
        exposure_matrix <- matrix(1000, nrow = n_ages, ncol = n_years,
                                  dimnames = list(ages, years))
        
        # Lee-Carter model
        incProgress(0.4, detail = "Fitting LC model")
        LC <- lc(link = "log")
        LC <- fit(LC, data = mortality_data, years.fit = years, ages.fit = ages)
        m.LC <- fitted(LC, type = "rates")
        
        
        # Renshaw-Haberman model
        incProgress(0.5, detail = "Fitting RH model")
        RH <- rh(link = "log")
        RH <- fit(RH, data = mortality_data, years.fit = years, ages.fit = ages)
        m.RH <- fitted(RH, type = "rates")
        
        
        # CBD model
        incProgress(0.6, detail = "Fitting CBD model")
        CBD <- m6(link = "log")
        CBD <- fit(CBD, data = mortality_data, years.fit = years, ages.fit = ages)
        m.CBD <- fitted(CBD, type = "rates")
        
        
        # Tree improvements
        incProgress(0.7, detail = "Applying decision trees")
        
        
        # LC with Decision Tree
        dx.LC <- m.LC * exposure_matrix
        dx <- mortality_matrix * exposure_matrix
        years_rep <- rep(years, each = n_ages)
        ages_rep <- rep(ages, n_years)
        cohort <- years_rep - ages_rep
        
        
        tree <- rpart(cbind(dx.LC, dx) ~ ages_rep + years_rep + cohort,
                      data = data.frame(
                        dx = as.vector(dx),
                        dx.LC = as.vector(dx.LC),
                        ages_rep = ages_rep,
                        years_rep = years_rep,
                        cohort = cohort
                      ),
                      method = "poisson", cp = 2e-3, control = rpart.control(minbucket = 30))
        mu <- predict(tree)
        m.LCDT <- matrix(mu * as.vector(m.LC), nrow = n_ages)
        
        
        # RH with Decision Tree
        dx.RH <- m.RH * exposure_matrix
        tree2 <- rpart(cbind(dx.RH, dx) ~ ages_rep + years_rep + cohort,
                       data = data.frame(
                         dx = as.vector(dx),
                         dx.RH = as.vector(dx.RH),
                         ages_rep = ages_rep,
                         years_rep = years_rep,
                         cohort = cohort
                       ),
                       method = "poisson",
                       cp = 2e-3)
        mu2 <- predict(tree2)
        m.RHDT <- matrix(mu2 * as.vector(m.RH), nrow = n_ages)
        
        # CBD with Decision Tree
        dx.CBD <- m.CBD * exposure_matrix
        tree3 <- rpart(cbind(dx.CBD, dx) ~ ages_rep + years_rep + cohort,
                       data = data.frame(
                         dx = as.vector(dx),
                         dx.CBD = as.vector(dx.CBD),
                         ages_rep = ages_rep,
                         years_rep = years_rep,
                         cohort = cohort
                       ),
                       method = "poisson",
                       cp = 2e-3)
        mu3 <- predict(tree3)
        m.CBDDT <- matrix(mu3 * as.vector(m.CBD), nrow = n_ages)
        
        # Lee-Carter model
        incProgress(0.4, detail = "Fitting LC model")
        LC <- lc(link = "log")
        LC <- fit(LC, data = mortality_data, years.fit = years, ages.fit = ages)
        m.LC <- fitted(LC, type = "rates")
        
        # Renshaw-Haberman model
        incProgress(0.5, detail = "Fitting RH model")
        RH <- rh(link = "log")
        RH <- fit(RH, data = mortality_data, years.fit = years, ages.fit = ages)
        m.RH <- fitted(RH, type = "rates")
        
        
        # CBD model
        incProgress(0.6, detail = "Fitting CBD model")
        CBD <- m6(link = "log")
        CBD <- fit(CBD, data = mortality_data, years.fit = years, ages.fit = ages)
        m.CBD <- fitted(CBD, type = "rates")
        
        # Tree improvements
        incProgress(0.7, detail = "Applying decision trees")
        
        # Prepare data for trees
        years_rep <- rep(years, each = n_ages)
        ages_rep <- rep(ages, n_years)
        cohort <- years_rep - ages_rep
        
        # LC with Decision Tree
        dx.LC <- m.LC * exposure_matrix
        dx <- mortality_matrix * exposure_matrix
        tree <- rpart(cbind(dx.LC, dx) ~ ages_rep + years_rep + cohort,
                      data = data.frame(
                        dx = as.vector(dx),
                        dx.LC = as.vector(dx.LC),
                        ages_rep = ages_rep,
                        years_rep = years_rep,
                        cohort = cohort
                      ),
                      method = "poisson", cp = 2e-3, control = rpart.control(minbucket = 30))
        mu <- predict(tree)
        m.LCDT <- matrix(mu * as.vector(m.LC), nrow = n_ages)
        
        # RH with Decision Tree
        dx.RH <- m.RH * exposure_matrix
        tree2 <- rpart(cbind(dx.RH, dx) ~ ages_rep + years_rep + cohort,
                       data = data.frame(
                         dx = as.vector(dx),
                         dx.RH = as.vector(dx.RH),
                         ages_rep = ages_rep,
                         years_rep = years_rep,
                         cohort = cohort
                       ),
                       method = "poisson",
                       cp = 2e-3)
        mu2 <- predict(tree2)
        m.RHDT <- matrix(mu2 * as.vector(m.RH), nrow = n_ages)
        
        # CBD with Decision Tree
        dx.CBD <- m.CBD * exposure_matrix
        tree3 <- rpart(cbind(dx.CBD, dx) ~ ages_rep + years_rep + cohort,
                       data = data.frame(
                         dx = as.vector(dx),
                         dx.CBD = as.vector(dx.CBD),
                         ages_rep = ages_rep,
                         years_rep = years_rep,
                         cohort = cohort
                       ),
                       method = "poisson",
                       cp = 2e-3)
        mu3 <- predict(tree3)
        m.CBDDT <- matrix(mu3 * as.vector(m.CBD), nrow = n_ages)
        
        
        # Calculate MAPE
        incProgress(0.9, detail = "Calculating MAPE")
        MAPE_LC <- calculate_custom_mape(mortality_matrix, m.LC)
        MAPE_RH <- calculate_custom_mape(mortality_matrix, m.RH)
        MAPE_CBD <- calculate_custom_mape(mortality_matrix, m.CBD)
        MAPE_LCDT <- calculate_custom_mape(mortality_matrix, m.LCDT)
        MAPE_RHDT <- calculate_custom_mape(mortality_matrix, m.RHDT)
        MAPE_CBDDT <- calculate_custom_mape(mortality_matrix, m.CBDDT)
        
        # Store results
        rv$mape_results <- data.frame(
          Model = c("LC", "RH", "CBD", "LCDT", "RHDT", "CBDDT"),
          MAPE = c(MAPE_LC, MAPE_RH, MAPE_CBD, MAPE_LCDT, MAPE_RHDT, MAPE_CBDDT)
        )
        
        
        # Find best model
        best_model_idx <- which.min(rv$mape_results$MAPE)
        rv$best_model_name <- rv$mape_results$Model[best_model_idx]
        
        # Get best model rates
        best_rates <- switch(rv$best_model_name,
                             "LC" = m.LC,
                             "RH" = m.RH,
                             "CBD" = m.CBD,
                             "LCDT" = m.LCDT,
                             "RHDT" = m.RHDT,
                             "CBDDT" = m.CBDDT)
        
        
        # Store best model rates
        rv$best_model_rates <- data.frame(
          Age = rep(ages, ncol(best_rates)),
          Year = rep(years, each = nrow(best_rates)),
          Mortality_Rate = as.vector(best_rates)
        )
        
        
        # Update year choices
        rv$years <- years
        updateSelectInput(session, "yearSelect", choices = years)
        
        
        incProgress(1.0, detail = "Complete!")
        showNotification("MAPE calculation completed!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error processing CSV:", e$message), 
                         type = "error", 
                         duration = NULL)
      })
    })
  })
  filename = function() {
    paste("mortality_rates_", input$gender, "_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
  }
  content = function(file) {
    req(rv$best_model_rates)
    write.csv(rv$best_model_rates, file, row.names = FALSE)
  }
  
  
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste("mortality_rates_", input$gender, "_", format(Sys.time(), "%Y%m%d"), ".xlsx", sep = "")
    },
    content = function(file) {
      req(rv$best_model_rates)
      write_xlsx(rv$best_model_rates, file)
    }
  )
}
