library(shiny)
library(shinyWidgets)
library(shinythemes)
library(dplyr)
library(DT)
library(readr)
library(readxl)
library(jsonlite)
library(plotly)
library(shinycssloaders)
library(caret)
library(randomForest)
library(gbm)
library(zip)
library(webshot)
library(reshape2)
library(uuid) # For unique session IDs
library(shinyjs) # Add shinyjs library

# Log interaction function
log_interaction <- function(session_id, group, button_id, timestamp = Sys.time()) {
  interaction_data <- data.frame(
    session_id = session_id,
    group = group,
    button_id = button_id,
    timestamp = timestamp
  )

  # Create log file if it doesn't exist
  if (!file.exists("button_interactions.csv")) {
    write.csv(interaction_data, "button_interactions.csv", row.names = FALSE)
  } else {
    write.table(interaction_data, "button_interactions.csv",
      sep = ",", row.names = FALSE, col.names = FALSE,
      append = TRUE
    )
  }
}

# Suppress Shiny errors and messages and upload size limit
options(shiny.suppressErrors = TRUE)
options(shiny.suppressMessages = TRUE)
options(shiny.maxRequestSize = Inf)

customCSS <- "
  body { background-color: #f7f7f7; }
  .navbar { background-color: #2c3e50 !important; }
  .navbar-default .navbar-brand { color: #ecf0f1 !important; font-size: 24px; }
  .navbar-default .navbar-nav > li > a { color: #ecf0f1 !important; font-size: 16px; }
  h4 { color: #2c3e50; }
  .well { background-color: #ecf0f1; padding: 20px; }
  .btn { background-color: #3498db; color: #fff; }
  .shiny-input-container { margin-bottom: 10px; }
  .tooltip-inner { max-width: 300px; text-align: left; }
"

ui <- navbarPage(
  id = "tabs",
  title = "Capytool",
  theme = shinytheme("flatly"),
  header = tags$head(
    tags$style(HTML(customCSS)),
    useShinyjs(),  # Initialize shinyjs
    # Google Analytics tracking code
    tags$script(HTML('
      <!-- Google tag (gtag.js) -->
      <script async src="https://www.googletagmanager.com/gtag/js?id=YOUR-GA4-ID"></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "YOUR-GA4-ID");

        // Custom event tracking for A/B test
        window.trackButtonClick = function(buttonId, group) {
          gtag("event", "button_click", {
            "button_id": buttonId,
            "test_group": group
          });
        }
      </script>
    ')),
    uiOutput("styled_buttons") # Add this line to apply styles
  ),

  # Home Tab
  tabPanel(
    "Home",
    fluidPage(
      div(
        class = "well",
        h2("Welcome to Capytool!"),
        p("Capytool is an advanced data processing and analysis tool that lets you:"),
        tags$ul(
          tags$li("Upload datasets (CSV, Excel, JSON, RDS)"),
          tags$li("Clean and preprocess data interactively – including imputation (median for numeric, mode for categorical), duplicate removal, outlier handling, type conversion, scaling, encoding, and text cleaning"),
          tags$li("Engineer new features using methods such as log transformation, polynomial features, interaction terms (single or multi‑column), binning, and custom mathematical transformations"),
          tags$li("Explore data with interactive visualizations – scatter plots, histograms (multiple side‑by‑side), box plots, pair plots, PCA (with grouping), correlation heatmaps, cluster plots, and violin plots"),
          tags$li("Split your data for modeling and run advanced models (Linear Regression, Logistic Regression, Random Forest, Gradient Boosting)")
        ),
        p("Use the navigation bar above to work through your data!")
      ),
      fluidRow(
        column(12,
          align = "left",
          actionButton("next_home", "Next")
        )
      )
    )
  ),

  # Preprocessing Tab
  tabPanel(
    "Preprocessing",
    sidebarLayout(
      sidebarPanel(
        # Sample Dataset Button and File Upload
        fileInput("files", "Upload Dataset(s)",
          accept = c(".csv", ".xlsx", ".json", ".rds"), multiple = TRUE
        ),
        actionButton("generate_sample", "Generate Sample Dataset"),
        uiOutput("dataset_selector"),
        uiOutput("column_selector"),
        tags$hr(),
        h4("Data Cleaning Options"),
        checkboxInput("remove_na", "Remove Missing Values", value = FALSE),
        checkboxInput("remove_duplicates", "Remove Duplicates", value = FALSE),
        h4("Missing Value Imputation"),
        radioButtons("impute_method", "Imputation Method",
          choices = c("None", "Median (numeric)/Mode (categorical)"),
          selected = "None", inline = TRUE
        ),
        h4("Outlier Handling"),
        selectInput("outlier_method", "Detect Outliers Using",
          choices = c("None", "Z-score", "IQR")
        ),
        conditionalPanel(
          condition = "input.outlier_method == 'Z-score'",
          sliderInput("zscore_cutoff", "Z-score Cutoff", min = 1, max = 5, value = 3, step = 0.1)
        ),
        conditionalPanel(
          condition = "input.outlier_method == 'IQR'",
          sliderInput("iqr_cutoff", "IQR Cutoff (Multiplier)", min = 1, max = 3, value = 1.5, step = 0.1)
        ),
        checkboxInput("remove_outliers", "Remove Outliers", value = FALSE),
        h4("Data Type Conversion & Scaling"),
        selectInput("convert_type", "Convert Data Type",
          choices = c("None", "To Character", "To Numeric", "To Date")
        ),
        selectInput("scale_method", "Scaling Method",
          choices = c("None", "Standardization (Z-score)", "Normalization (0-1)")
        ),
        h4("Categorical Encoding"),
        selectInput("encode_method", "Select method",
          choices = c("None", "One-Hot Encoding", "Label Encoding")
        ),
        h4("Text Cleaning"),
        checkboxInput("trim_whitespace", "Trim Whitespace", value = FALSE),
        checkboxInput("to_lowercase", "Convert to Lowercase", value = FALSE),
        checkboxInput("remove_punctuation", "Remove Punctuation", value = FALSE),
        actionButton("next_preprocessing", "Apply and Continue")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Data Preview",
            h4("Dataset Preview"),
            DTOutput("preview") %>% withSpinner(color = "#0dc5c1"),
            h4("Dataset Summary"),
            DTOutput("data_summary"),
            downloadButton("download_clean", "Download Cleaned Data")
          ),
          tabPanel(
            "Encoding Key",
            h4("Categorical Encoding Key"),
            DTOutput("encoding_key")
          ),
          tabPanel(
            "Outlier Visualization",
            h4("Outlier Distribution"),
            plotlyOutput("outlier_plot", height = "500px")
          )
        )
      )
    )
  ),

  # Feature Engineering Tab
  tabPanel(
    "Feature Engineering",
    sidebarLayout(
      sidebarPanel(
        h4("Feature Engineering Options"),
        radioButtons("fe_method", "Select Feature Engineering Method",
          choices = c(
            "None", "Log Transformation", "Polynomial Feature",
            "Interaction Term", "Multi-Column Interaction", "Binning", "Custom Transformation"
          ),
          selected = "None"
        ),
        conditionalPanel(
          condition = "input.fe_method == 'Log Transformation'",
          selectizeInput("log_cols", "Select Column(s) for Log Transformation", choices = NULL, multiple = TRUE)
        ),
        conditionalPanel(
          condition = "input.fe_method == 'Polynomial Feature'",
          selectizeInput("poly_cols", "Select Column(s) for Polynomial Feature", choices = NULL, multiple = TRUE),
          numericInput("poly_degree", "Degree", value = 2, min = 2)
        ),
        conditionalPanel(
          condition = "input.fe_method == 'Interaction Term'",
          selectInput("interact_col1", "Select First Column for Interaction", choices = NULL),
          selectInput("interact_col2", "Select Second Column for Interaction", choices = NULL)
        ),
        conditionalPanel(
          condition = "input.fe_method == 'Multi-Column Interaction'",
          selectizeInput("multi_interact_cols", "Select Multiple Columns for Interaction", choices = NULL, multiple = TRUE)
        ),
        conditionalPanel(
          condition = "input.fe_method == 'Binning'",
          selectInput("bin_col", "Select Column for Binning", choices = NULL),
          numericInput("num_bins", "Number of Bins", value = 4, min = 2)
        ),
        conditionalPanel(
          condition = "input.fe_method == 'Custom Transformation'",
          selectizeInput("custom_cols", "Select Column(s) for Custom Transformation", choices = NULL, multiple = TRUE),
          textInput("custom_expr", "Enter Transformation Expression (use 'x' as variable)", value = "log(x+1)")
        ),
        actionButton("apply_fe", "Apply Feature Engineering"),
        br(), br(),
        actionButton("next_feature", "Next")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Engineered Data",
            h4("Engineered Data Preview"),
            DTOutput("engineered_preview") %>% withSpinner(color = "#0dc5c1"),
            h4("Engineered Data Summary"),
            DTOutput("engineered_summary_dt"),
            downloadButton("download_fe", "Download Engineered Data")
          ),
          tabPanel(
            "Custom Transformation Plot",
            conditionalPanel(
              condition = "input.fe_method == 'Custom Transformation'",
              h4("Custom Transformation: Original vs Transformed"),
              plotlyOutput("custom_plot", height = "400px")
            )
          )
        )
      )
    )
  ),

  # Exploratory Data Analysis (EDA) Tab
  tabPanel(
    "Exploratory Data Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("EDA Options"),
        selectizeInput("eda_columns", "Select Columns for EDA",
          choices = NULL, multiple = TRUE,
          options = list(placeholder = "Select columns...")
        ),
        checkboxInput("select_all_eda", "Select All Columns", value = FALSE),
        radioButtons("plot_type", "Select Plot Type",
          choices = c("Scatter Plot", "Histogram", "Box Plot", "Pair Plot", "PCA Plot", "Correlation Heatmap", "Cluster Plot", "Violin Plot for Clusters"),
          selected = "Scatter Plot"
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Scatter Plot'",
          selectInput("scatter_x", "X-axis", choices = NULL),
          selectInput("scatter_y", "Y-axis", choices = NULL)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Histogram'",
          selectizeInput("hist_cols", "Select Column(s) for Histogram", choices = NULL, multiple = TRUE)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Box Plot'",
          selectizeInput("box_cols", "Select Column(s) for Box Plot", choices = NULL, multiple = TRUE)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'PCA Plot'",
          radioButtons("pca_mode", "PCA Mode", choices = c("2D", "3D"), selected = "2D", inline = TRUE),
          numericInput("pca_components", "Number of PCA Components", value = 2, min = 2),
          selectInput("pca_group", "Group by (Optional Must be Categorical)", choices = NULL)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Cluster Plot'",
          selectInput("cluster_x", "Cluster X-axis", choices = NULL),
          selectInput("cluster_y", "Cluster Y-axis", choices = NULL),
          checkboxInput("multi_cluster", "Multivariable Cluster Plot", value = FALSE)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'Violin Plot for Clusters'",
          selectInput("violin_group", "Select Grouping Variable (Must be categorical)", choices = NULL),
          selectInput("violin_value", "Select Value Variable", choices = NULL)
        ),
        br(), br(),
        actionButton("next_eda", "Next")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Plot",
            withSpinner(plotlyOutput("eda_plot", height = "500px")),
            conditionalPanel(
              condition = "input.plot_type == 'Scatter Plot' || input.plot_type == 'Cluster Plot'",
              h4("Lasso Points"),
              DTOutput("brushed_points"),
              downloadButton("download_brushed", "Download Lasso Points")
            ),
            conditionalPanel(
              condition = "input.plot_type == 'PCA Plot'",
              h4("PCA Components Table"),
              DTOutput("pca_table"),
              downloadButton("download_pca", "Download PCA Components")
            )
          ),
          tabPanel(
            "Descriptive Statistics",
            DTOutput("eda_stats"),
            downloadButton("download_stats", "Download Descriptive Stats")
          )
        )
      )
    )
  ),

  # Modeling Tab
  tabPanel(
    "Modeling",
    sidebarLayout(
      sidebarPanel(
        h4("Data Splitting"),
        sliderInput("train_ratio", "Train/Test Split Ratio", min = 0.5, max = 0.9, value = 0.7, step = 0.05),
        selectInput("target_col", "Select Target Variable", choices = NULL),
        checkboxInput("select_all_predictors", "Select All Other Columns as Predictors", value = FALSE),
        selectizeInput("predictor_cols", "Select Predictor(s)", choices = NULL, multiple = TRUE),
        actionButton("split_data", "Split Data"),
        downloadButton("download_train", "Download Training Set"),
        downloadButton("download_test", "Download Test Set"),
        hr(),
        h4("Modeling Options"),
        h5("Note that categorical variables must be encoded."),
        radioButtons("model_type", "Select Model Type",
          choices = c("Linear Regression", "Logistic Regression", "Random Forest", "Gradient Boosting"),
          selected = "Linear Regression"
        ),
        actionButton("run_model", "Run Model")
      ),
      mainPanel(
        h4("Training Data Preview (Target & Predictors)"),
        DTOutput("train_preview"),
        hr(),
        h4("Model Summary"),
        verbatimTextOutput("model_summary"),
        hr(),
        h4("Model Performance Plot"),
        plotlyOutput("model_plot", height = "400px"),
        downloadButton("download_model", "Download Model Summary")
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialize session data
  session_data <- reactiveValues(
    id = UUIDgenerate(),
    group = sample(c("A", "B", "C"), 1),
    button_style = NULL
  )

  # Print group assignment to console
  observe({
    cat("\n\n**************************\n")
    cat("User assigned to test group:", session_data$group, "\n")
    cat("**************************\n\n")
  })

  # Set button styles based on group
  observe({
    if (session_data$group == "A") {
      session_data$button_style <- "background-color: #3498db; color: white;" # Original blue
    } else if (session_data$group == "B") {
      session_data$button_style <- "background-color: #e74c3c; color: white; font-size: 18px; padding: 10px 20px;" # Enhanced red
    } else {
      session_data$button_style <- "background-color: #2ecc71; color: white;" # Green for group C
    }
  })

  # Modify existing buttons with A/B/C styles and add tooltips
  output$styled_buttons <- renderUI({
    tagList(
      tags$style(HTML(sprintf(
        "#next_home, #next_preprocessing, #apply_fe, #run_model { %s }",
        session_data$button_style
      ))),
      if (session_data$group == "B") {
        tags$style(HTML("
          #next_home::before { content: '➜'; margin-right: 5px; }
          #next_preprocessing::before { content: '✓'; margin-right: 5px; }
          #apply_fe::before { content: '⚡'; margin-right: 5px; }
          #run_model::before { content: '▶'; margin-right: 5px; }
        "))
      },
      if (session_data$group == "C") {
        tags$script(HTML('
          $(document).ready(function() {
            // File handling tooltips
            $("#files").tooltip({
              title: "Upload your dataset in CSV, Excel, JSON, or RDS format. Multiple files are supported.",
              placement: "right"
            });
            $("#generate_sample").tooltip({
              title: "Generate a sample dataset with numeric and categorical variables to try out the app features.",
              placement: "right"
            });
            
            // Preprocessing tooltips
            $("#remove_na").tooltip({
              title: "Check this to remove rows with any missing values.",
              placement: "right"
            });
            $("#remove_duplicates").tooltip({
              title: "Check this to remove duplicate rows from your dataset.",
              placement: "right"
            });
            $("#impute_method").tooltip({
              title: "Choose how to handle missing values: median for numeric columns, most frequent value for categorical.",
              placement: "right"
            });
            $("#outlier_method").tooltip({
              title: "Z-score detects outliers based on standard deviations from the mean. IQR uses the interquartile range method.",
              placement: "right"
            });
            $("#scale_method").tooltip({
              title: "Standardization centers data around 0 with SD=1. Normalization scales data between 0 and 1.",
              placement: "right"
            });
            $("#encode_method").tooltip({
              title: "One-Hot Encoding creates binary columns for categories. Label Encoding converts categories to numbers.",
              placement: "right"
            });
            
            // Feature Engineering tooltips
            $("#fe_method").tooltip({
              title: "Choose a method to create new features: Log Transform for skewed data, Polynomial for non-linear relationships, Interaction for combined effects.",
              placement: "right"
            });
            $("#custom_expr").tooltip({
              title: "Enter a mathematical expression using \'x\' as the variable, e.g., log(x+1) or x^2/10",
              placement: "right"
            });
            
            // EDA tooltips
            $("#plot_type").tooltip({
              title: "Choose visualization type: Scatter for relationships, Histogram for distributions, PCA for dimensionality reduction, etc.",
              placement: "right"
            });
            $("#select_all_eda").tooltip({
              title: "Quickly select all columns for analysis. Uncheck to select specific columns.",
              placement: "right"
            });
            
            // Modeling tooltips
            $("#train_ratio").tooltip({
              title: "Choose how much data to use for training. Higher ratio means more training data, but less testing data.",
              placement: "right"
            });
            $("#model_type").tooltip({
              title: "Linear/Logistic Regression for simple relationships, Random Forest/Gradient Boosting for complex patterns.",
              placement: "right"
            });
            $("#target_col").tooltip({
              title: "Select the variable you want to predict.",
              placement: "right"
            });
            $("#select_all_predictors").tooltip({
              title: "Automatically select all columns except the target as predictors.",
              placement: "right"
            });
          });
        '))
      }
    )
  })

  # Track button clicks
  observeEvent(input$next_home, {
    log_interaction(session_data$id, session_data$group, "next_home")
    runjs(sprintf("window.trackButtonClick('next_home', '%s');", session_data$group))
    updateNavbarPage(session, "tabs", selected = "Preprocessing")
  })

  observeEvent(input$next_preprocessing, {
    log_interaction(session_data$id, session_data$group, "next_preprocessing")
    runjs(sprintf("window.trackButtonClick('next_preprocessing', '%s');", session_data$group))
    updateNavbarPage(session, "tabs", selected = "Feature Engineering")
  })

  observeEvent(input$apply_fe, {
    log_interaction(session_data$id, session_data$group, "apply_fe")
    runjs(sprintf("window.trackButtonClick('apply_fe', '%s');", session_data$group))
  })

  observeEvent(input$run_model, {
    log_interaction(session_data$id, session_data$group, "run_model")
    runjs(sprintf("window.trackButtonClick('run_model', '%s');", session_data$group))
  })

  # Helper function to clean column names
  clean_colnames <- function(df) {
    colnames(df) <- gsub("[^[:alnum:]_]", "_", colnames(df))
    colnames(df) <- tolower(colnames(df))
    return(df)
  }

  # Reactive to load uploaded datasets or use sample dataset if generated
  datasets <- reactive({
    if (!is.null(input$files) && nrow(input$files) > 0) {
      files <- input$files
      result <- list()
      for (i in seq_along(files$name)) {
        ext <- tolower(tools::file_ext(files$name[i]))
        path <- files$datapath[i]
        data <- switch(ext,
          csv = tryCatch(
            {
              read_csv(path, col_types = cols(), guess_max = 1000)
            },
            error = function(e) {
              showNotification(paste("CSV error in", files$name[i], ":", e$message), type = "error")
              NULL
            }
          ),
          xlsx = tryCatch(
            {
              read_excel(path)
            },
            error = function(e) {
              showNotification(paste("Excel error in", files$name[i], ":", e$message), type = "error")
              NULL
            }
          ),
          json = tryCatch(
            {
              json_lines <- readLines(path, warn = FALSE)
              json_data <- lapply(json_lines, jsonlite::fromJSON)
              do.call(rbind, lapply(json_data, as.data.frame))
            },
            error = function(e) {
              showNotification(paste("JSON error in", files$name[i], ":", e$message), type = "error")
              NULL
            }
          ),
          rds = tryCatch(
            {
              readRDS(path)
            },
            error = function(e) {
              showNotification(paste("RDS error in", files$name[i], ":", e$message), type = "error")
              NULL
            }
          ),
          NULL
        )
        if (!is.null(data)) {
          result[[files$name[i]]] <- clean_colnames(as.data.frame(data))
        }
      }
      return(result)
    } else if (input$generate_sample > 0) {
      sample_df <- data.frame(
        num1 = rnorm(100),
        num2 = runif(100),
        num3 = rnorm(100, mean = 50, sd = 10),
        num4 = rpois(100, lambda = 20),
        num5 = rnorm(100, mean = 100, sd = 15),
        cat1 = sample(LETTERS[1:4], 100, replace = TRUE),
        cat2 = sample(c("A", "B", "C"), 100, replace = TRUE),
        num6 = rnorm(100, mean = 0, sd = 5),
        num7 = runif(100, min = -10, max = 10),
        cat3 = sample(c("X", "Y", "Z"), 100, replace = TRUE)
      )
      return(list("Sample Dataset" = sample_df))
    } else {
      return(NULL)
    }
  })

  # UI for dataset selector and column selector
  output$dataset_selector <- renderUI({
    req(datasets())
    selectInput("selected_dataset", "Select Dataset", choices = names(datasets()))
  })

  output$column_selector <- renderUI({
    req(datasets(), input$selected_dataset)
    checkboxGroupInput("selected_columns", "Select Columns and Process them Dynamically",
      choices = names(datasets()[[input$selected_dataset]]),
      selected = names(datasets()[[input$selected_dataset]])
    )
  })

  # Reactive for cleaned data with encoding key generation
  cleaned_data <- reactive({
    req(datasets(), input$selected_dataset)
    df <- datasets()[[input$selected_dataset]]
    if (!is.null(input$selected_columns)) {
      df <- df[, input$selected_columns, drop = FALSE]
    }
    if (input$remove_na) {
      df <- na.omit(df)
    }
    if (input$remove_duplicates) {
      df <- distinct(df)
    }

    if (input$impute_method == "Median (numeric)/Mode (categorical)") {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        } else if (is.character(df[[col]])) {
          mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
          df[[col]][is.na(df[[col]])] <- mode_val
        }
      }
    }

    if (input$outlier_method != "None") {
      numeric_cols <- df %>% select(where(is.numeric))
      if (input$outlier_method == "Z-score") {
        z_scores <- scale(numeric_cols)
        outliers <- abs(z_scores) > input$zscore_cutoff
      } else if (input$outlier_method == "IQR") {
        Q1 <- apply(numeric_cols, 2, quantile, 0.25, na.rm = TRUE)
        Q3 <- apply(numeric_cols, 2, quantile, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        outliers <- (numeric_cols < (Q1 - input$iqr_cutoff * IQR)) | (numeric_cols > (Q3 + input$iqr_cutoff * IQR))
      }
      if (input$remove_outliers) {
        df <- df[!apply(outliers, 1, any), ]
      }
    }

    if (input$convert_type == "To Character") {
      df <- df %>% mutate(across(where(is.numeric), as.character))
    } else if (input$convert_type == "To Numeric") {
      df <- df %>% mutate(across(where(is.character), as.numeric))
    } else if (input$convert_type == "To Date") {
      df <- df %>% mutate(across(where(is.character), as.Date))
    }

    if (input$scale_method == "Standardization (Z-score)") {
      df <- df %>% mutate(across(where(is.numeric), scale))
    } else if (input$scale_method == "Normalization (0-1)") {
      df <- df %>% mutate(across(where(is.numeric), ~ (. - min(.)) / (max(.) - min(.))))
    }

    # Capture original column names before encoding
    orig_cols <- names(df)

    if (input$encode_method == "One-Hot Encoding") {
      df <- as.data.frame(model.matrix(~ . - 1, data = df))
      encoded_cols <- colnames(df)
      key_df <- data.frame(Original = orig_cols, Encoded = NA, stringsAsFactors = FALSE)
      key_df$Encoded <- sapply(orig_cols, function(col) {
        paste(encoded_cols[grepl(paste0("^", col), encoded_cols)], collapse = ", ")
      })
      output$encoding_key <- renderDT({
        datatable(key_df, options = list(pageLength = 10, scrollX = TRUE))
      })
    } else if (input$encode_method == "Label Encoding") {
      key_df <- data.frame(Original = orig_cols, Encoding = NA, stringsAsFactors = FALSE)
      for (col in orig_cols) {
        if (is.character(datasets()[[input$selected_dataset]][[col]])) {
          lvls <- sort(unique(datasets()[[input$selected_dataset]][[col]]))
          key_df[key_df$Original == col, "Encoding"] <- paste("Levels:", paste(lvls, collapse = ", "))
        } else {
          key_df[key_df$Original == col, "Encoding"] <- "Not Encoded"
        }
      }
      df <- df %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(across(where(is.factor), as.numeric))
      output$encoding_key <- renderDT({
        datatable(key_df, options = list(pageLength = 10, scrollX = TRUE))
      })
    } else {
      output$encoding_key <- renderDT({
        datatable(data.frame(Message = "No encoding applied"), options = list(pageLength = 10, scrollX = TRUE))
      })
    }

    if (input$trim_whitespace) {
      df <- df %>% mutate(across(where(is.character), trimws))
    }
    if (input$to_lowercase) {
      df <- df %>% mutate(across(where(is.character), tolower))
    }
    if (input$remove_punctuation) {
      df <- df %>% mutate(across(where(is.character), ~ gsub("[[:punct:]]", "", .)))
    }
    df
  })

  output$preview <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(pageLength = 10, scrollX = TRUE))
  })

  dataset_summary <- reactive({
    req(cleaned_data())
    df <- cleaned_data()
    stat_list <- lapply(names(df), function(col) {
      if (is.numeric(df[[col]])) {
        as.character(summary(df[[col]]))
      } else {
        c("Categorical", rep("", 5))
      }
    })
    stat_df <- as.data.frame(do.call(rbind, stat_list), stringsAsFactors = FALSE)
    stat_df <- cbind(Variable = names(df), stat_df)
    colnames(stat_df)[-1] <- c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max")
    stat_df
  })

  output$data_summary <- renderDT({
    datatable(dataset_summary(),
      options = list(
        pageLength = 10, scrollX = TRUE, filter = "top",
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
          "}"
        )
      )
    )
  })

  output$download_clean <- downloadHandler(
    filename = function() {
      "cleaned_data.csv"
    },
    content = function(file) {
      write.csv(cleaned_data(), file, row.names = FALSE)
    }
  )

  output$outlier_plot <- renderPlotly({
    req(cleaned_data())
    df <- cleaned_data()
    numeric_cols <- df %>% select(where(is.numeric))
    if (input$outlier_method == "Z-score") {
      z_scores <- scale(numeric_cols)
      p <- plot_ly(x = ~z_scores, type = "histogram", name = "Z-scores", marker = list(color = "lightblue"))
      p <- p %>% layout(title = "Z-score Distribution", xaxis = list(title = "Z-score"), yaxis = list(title = "Count"))
    } else if (input$outlier_method == "IQR") {
      Q1 <- apply(numeric_cols, 2, quantile, 0.25, na.rm = TRUE)
      Q3 <- apply(numeric_cols, 2, quantile, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      p <- plot_ly(x = ~IQR, type = "box", name = "IQR", marker = list(color = "lightgreen"))
      p <- p %>% layout(title = "IQR Distribution", xaxis = list(title = "IQR"), yaxis = list(title = "Count"))
    } else {
      p <- plot_ly(x = ~ unlist(numeric_cols), type = "histogram", name = "Data Distribution", marker = list(color = "lightblue"))
      p <- p %>% layout(title = "Data Distribution", xaxis = list(title = "Value"), yaxis = list(title = "Count"))
    }
    p
  })

  observe({
    req(cleaned_data())
    numeric_vars <- names(cleaned_data()[, sapply(cleaned_data(), is.numeric), drop = FALSE])
    updateSelectizeInput(session, "log_cols", choices = numeric_vars, server = TRUE)
    updateSelectizeInput(session, "poly_cols", choices = numeric_vars, server = TRUE)
    updateSelectInput(session, "interact_col1", choices = numeric_vars)
    updateSelectInput(session, "interact_col2", choices = numeric_vars)
    updateSelectizeInput(session, "multi_interact_cols", choices = numeric_vars, server = TRUE)
    updateSelectInput(session, "bin_col", choices = numeric_vars)
    updateSelectizeInput(session, "custom_cols", choices = numeric_vars, server = TRUE)
  })

  observe({
    req(cleaned_data())
    all_cols <- names(cleaned_data())
    updateSelectizeInput(session, "eda_columns", choices = all_cols, server = TRUE)
  })

  observeEvent(input$select_all_eda, {
    req(cleaned_data())
    all_cols <- names(cleaned_data())
    if (input$select_all_eda) {
      updateSelectizeInput(session, "eda_columns", selected = all_cols)
    } else {
      updateSelectizeInput(session, "eda_columns", selected = character(0))
    }
  })

  observe({
    req(cleaned_data(), input$eda_columns)
    eda_numeric <- names(cleaned_data()[, input$eda_columns, drop = FALSE][, sapply(cleaned_data()[, input$eda_columns, drop = FALSE], is.numeric), drop = FALSE])
    updateSelectInput(session, "scatter_x", choices = eda_numeric)
    updateSelectInput(session, "scatter_y", choices = eda_numeric)
    updateSelectizeInput(session, "hist_cols", choices = eda_numeric, server = TRUE)
    updateSelectizeInput(session, "box_cols", choices = eda_numeric, server = TRUE)
    cat_cols <- names(cleaned_data())[sapply(cleaned_data(), function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "pca_group", choices = c("None", cat_cols), selected = "None")
    updateSelectInput(session, "cluster_x", choices = eda_numeric)
    updateSelectInput(session, "cluster_y", choices = eda_numeric)
    updateSelectInput(session, "violin_group", choices = cat_cols)
    updateSelectInput(session, "violin_value", choices = eda_numeric)
  })

  observe({
    req(cleaned_data())
    all_cols <- names(cleaned_data())
    updateSelectInput(session, "target_col", choices = all_cols)
    updateSelectizeInput(session, "predictor_cols", choices = all_cols, server = TRUE)
  })

  observeEvent(input$select_all_predictors, {
    req(cleaned_data())
    all_cols <- names(cleaned_data())
    if (input$select_all_predictors) {
      updateSelectizeInput(session, "predictor_cols", selected = setdiff(all_cols, input$target_col))
    } else {
      updateSelectizeInput(session, "predictor_cols", selected = character(0))
    }
  })

  engineered_data <- eventReactive(input$apply_fe, {
    req(cleaned_data())
    df <- cleaned_data()
    if (input$fe_method == "Log Transformation") {
      req(input$log_cols)
      for (col in input$log_cols) {
        new_col <- paste0("log_", col)
        df[[new_col]] <- log(df[[col]] + 1)
      }
    } else if (input$fe_method == "Polynomial Feature") {
      req(input$poly_cols, input$poly_degree)
      for (col in input$poly_cols) {
        new_col <- paste0(col, "_poly_", input$poly_degree)
        df[[new_col]] <- df[[col]]^input$poly_degree
      }
    } else if (input$fe_method == "Interaction Term") {
      req(input$interact_col1, input$interact_col2)
      new_col <- paste0(input$interact_col1, "_x_", input$interact_col2)
      df[[new_col]] <- df[[input$interact_col1]] * df[[input$interact_col2]]
    } else if (input$fe_method == "Multi-Column Interaction") {
      req(input$multi_interact_cols)
      new_col <- paste(input$multi_interact_cols, collapse = "_x_")
      df[[new_col]] <- apply(df[, input$multi_interact_cols, drop = FALSE], 1, prod, na.rm = TRUE)
    } else if (input$fe_method == "Binning") {
      req(input$bin_col, input$num_bins)
      new_col <- paste0(input$bin_col, "_binned")
      df[[new_col]] <- cut(df[[input$bin_col]], breaks = input$num_bins, include.lowest = TRUE)
    } else if (input$fe_method == "Custom Transformation") {
      req(input$custom_cols, input$custom_expr)
      for (col in input$custom_cols) {
        new_col <- paste0(col, "_custom")
        x <- df[[col]]
        transformed <- tryCatch(eval(parse(text = input$custom_expr), envir = list(x = x)),
          error = function(e) {
            showNotification("Error in custom transformation. Check your expression.", type = "error")
            rep(NA, length(x))
          }
        )
        df[[new_col]] <- transformed
      }
    }
    df
  })

  output$engineered_preview <- renderDT({
    req(engineered_data())
    datatable(engineered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })

  engineered_summary_table <- reactive({
    req(engineered_data())
    df <- engineered_data()
    stat_list <- lapply(names(df), function(col) {
      if (is.numeric(df[[col]])) {
        s <- summary(df[[col]])
        as.character(s)
      } else {
        c("Categorical", rep("", 5))
      }
    })
    stat_df <- as.data.frame(do.call(rbind, stat_list), stringsAsFactors = FALSE)
    stat_df <- cbind(Variable = names(df), stat_df)
    colnames(stat_df)[-1] <- c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max")
    stat_df
  })

  output$engineered_summary_dt <- renderDT({
    datatable(engineered_summary_table(),
      options = list(
        pageLength = 10, scrollX = TRUE, filter = "top",
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
          "}"
        )
      )
    )
  })

  output$download_fe <- downloadHandler(
    filename = function() {
      "engineered_data.csv"
    },
    content = function(file) {
      write.csv(engineered_data(), file, row.names = FALSE)
    }
  )

  output$custom_plot <- renderPlotly({
    req(cleaned_data(), input$custom_cols, input$custom_expr)
    df <- cleaned_data()
    col <- input$custom_cols[1]
    x <- df[[col]]
    transformed <- tryCatch(eval(parse(text = input$custom_expr), envir = list(x = x)),
      error = function(e) NA
    )
    if (!all(is.na(transformed))) {
      p1 <- plot_ly(x = ~x, type = "histogram", name = col, marker = list(color = "lightblue"), showlegend = TRUE)
      p2 <- plot_ly(x = ~transformed, type = "histogram", name = paste(col, "transformed"), marker = list(color = "lightgreen"), showlegend = TRUE)
      subplot(p1, p2, nrows = 1, shareX = FALSE, titleX = TRUE) %>%
        layout(title = paste("Custom Transformation for", col))
    }
  })

  output$eda_plot <- renderPlotly({
    req(cleaned_data())
    df <- cleaned_data()
    if (input$plot_type == "Scatter Plot") {
      req(input$scatter_x, input$scatter_y)
      p <- plot_ly(df,
        x = ~ df[[input$scatter_x]], y = ~ df[[input$scatter_y]],
        type = "scatter", mode = "markers", source = "scatterPlot"
      )
      p <- p %>% layout(
        title = paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y),
        xaxis = list(title = input$scatter_x),
        yaxis = list(title = input$scatter_y)
      )
      p
    } else if (input$plot_type == "Histogram") {
      req(input$hist_cols)
      overall_title <- paste("Histograms of", paste(input$hist_cols, collapse = ", "))
      plots <- lapply(input$hist_cols, function(col) {
        plot_ly(df,
          x = ~ df[[col]], type = "histogram", name = col, showlegend = TRUE,
          marker = list(color = sample(colors(), 1))
        ) %>%
          layout(title = col, xaxis = list(title = col), yaxis = list(title = "Count"))
      })
      subplot(plots, nrows = ceiling(length(plots) / 2), shareX = FALSE) %>%
        layout(title = overall_title)
    } else if (input$plot_type == "Box Plot") {
      req(input$box_cols)
      overall_title <- paste("Box Plots of", paste(input$box_cols, collapse = ", "))
      plots <- lapply(input$box_cols, function(col) {
        plot_ly(df,
          y = ~ df[[col]], type = "box", name = col, showlegend = TRUE,
          boxpoints = "all", jitter = 0.3
        ) %>%
          layout(title = col, yaxis = list(title = col))
      })
      subplot(plots, nrows = ceiling(length(plots) / 2), shareY = FALSE) %>%
        layout(title = overall_title)
    } else if (input$plot_type == "Pair Plot") {
      req(input$eda_columns)
      sub_df <- df[, input$eda_columns, drop = FALSE]
      p <- plot_ly(type = "splom", dimensions = lapply(names(sub_df), function(col) list(label = col, values = sub_df[[col]])))
      p <- p %>% layout(title = "Scatterplot Matrix")
      p
    } else if (input$plot_type == "PCA Plot") {
      req(input$eda_columns, input$pca_components)
      sub_df <- df[, input$eda_columns, drop = FALSE]
      sub_df <- sub_df[, sapply(sub_df, is.numeric), drop = FALSE]
      if (ncol(sub_df) < input$pca_components) {
        showNotification("Not enough numeric columns selected for PCA.", type = "error")
        return(NULL)
      }
      pca_model <- prcomp(sub_df, scale. = TRUE)
      pca_data <- as.data.frame(pca_model$x[, 1:input$pca_components])
      colnames(pca_data) <- paste0("PC", 1:input$pca_components)
      if (!is.null(input$pca_group) && input$pca_group != "None") {
        pca_data[[input$pca_group]] <- as.factor(df[[input$pca_group]])
      }
      if (input$pca_mode == "3D" && input$pca_components >= 3) {
        if (!is.null(input$pca_group) && input$pca_group != "None") {
          p <- plot_ly(pca_data,
            x = ~PC1, y = ~PC2, z = ~PC3, color = ~ pca_data[[input$pca_group]],
            type = "scatter3d", mode = "markers"
          )
        } else {
          p <- plot_ly(pca_data, x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d", mode = "markers")
        }
        p <- p %>% layout(title = "3D PCA Plot", scene = list(
          xaxis = list(title = "PC1"),
          yaxis = list(title = "PC2"),
          zaxis = list(title = "PC3")
        ))
      } else {
        if (!is.null(input$pca_group) && input$pca_group != "None") {
          p <- plot_ly(pca_data,
            x = ~PC1, y = ~PC2, color = ~ pca_data[[input$pca_group]],
            type = "scatter", mode = "markers"
          )
        } else {
          p <- plot_ly(pca_data, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers")
        }
        p <- p %>% layout(
          title = "PCA Plot (First Two Components)",
          xaxis = list(title = "PC1"),
          yaxis = list(title = "PC2")
        )
      }
      p
    } else if (input$plot_type == "Correlation Heatmap") {
      req(input$eda_columns)
      sub_df <- df[, input$eda_columns, drop = FALSE]
      sub_df <- sub_df[, sapply(sub_df, is.numeric), drop = FALSE]
      if (ncol(sub_df) < 2) {
        showNotification("Select at least 2 numeric columns for correlation heatmap.", type = "error")
        return(NULL)
      }
      corr_matrix <- round(cor(sub_df, use = "pairwise.complete.obs"), 2)
      plot_ly(
        x = colnames(corr_matrix),
        y = rownames(corr_matrix),
        z = as.matrix(corr_matrix),
        type = "heatmap",
        colors = colorRamp(c("blue", "white", "red"))
      ) %>% layout(title = "Interactive Correlation Heatmap")
    } else if (input$plot_type == "Cluster Plot") {
      req(input$cluster_x, input$cluster_y)
      if (input$multi_cluster) {
        sub_df <- df[, input$eda_columns, drop = FALSE]
        sub_df <- sub_df[, sapply(sub_df, is.numeric), drop = FALSE]
        set.seed(123)
        km <- kmeans(sub_df, centers = 3)
        sub_df$cluster <- as.factor(km$cluster)
        p <- plot_ly(
          type = "parcoords",
          dimensions = lapply(names(sub_df)[-ncol(sub_df)], function(col) {
            list(
              label = col,
              values = sub_df[[col]],
              range = range(sub_df[[col]], na.rm = TRUE)
            )
          }),
          line = list(color = as.numeric(sub_df$cluster), colorscale = "Portland", showscale = TRUE)
        )
        p <- p %>% layout(title = "Multivariable Cluster Plot (Parallel Coordinates)")
      } else {
        sub_df <- df[, c(input$cluster_x, input$cluster_y), drop = FALSE]
        set.seed(123)
        km <- kmeans(sub_df, centers = 3)
        sub_df$cluster <- as.factor(km$cluster)
        p <- plot_ly(sub_df,
          x = ~ sub_df[[input$cluster_x]], y = ~ sub_df[[input$cluster_y]],
          color = ~cluster, colors = "Set1", type = "scatter", mode = "markers", source = "clusterPlot"
        )
        p <- p %>% layout(
          title = paste("Cluster Plot:", input$cluster_x, "vs", input$cluster_y),
          xaxis = list(title = input$cluster_x),
          yaxis = list(title = input$cluster_y)
        )
      }
      p
    } else if (input$plot_type == "Violin Plot for Clusters") {
      req(input$violin_group, input$violin_value)
      p <- plot_ly(df,
        x = ~ df[[input$violin_group]], y = ~ df[[input$violin_value]], type = "violin",
        box = list(visible = TRUE), meanline = list(visible = TRUE),
        name = paste(input$violin_value, "by", input$violin_group), showlegend = TRUE
      )
      p <- p %>% layout(
        title = paste("Violin Plot of", input$violin_value, "by", input$violin_group),
        xaxis = list(title = input$violin_group),
        yaxis = list(title = input$violin_value)
      )
      p
    }
  })

  output$brushed_points <- renderDT({
    if (!(input$plot_type %in% c("Scatter Plot", "Cluster Plot"))) {
      return(datatable(data.frame(Message = "Brushing is not supported for this plot type.")))
    }
    if (input$plot_type == "Scatter Plot") {
      source_val <- "scatterPlot"
      x_var <- input$scatter_x
      y_var <- input$scatter_y
    } else if (input$plot_type == "Cluster Plot") {
      source_val <- "clusterPlot"
      x_var <- input$cluster_x
      y_var <- input$cluster_y
    }
    brushed <- event_data("plotly_selected", source = source_val)
    if (is.null(brushed) || nrow(brushed) == 0) {
      return(datatable(data.frame(Message = "No points selected.")))
    }
    df <- cleaned_data()
    selected <- df %>% filter(get(x_var) %in% brushed$x & get(y_var) %in% brushed$y)
    datatable(selected, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$eda_stats <- renderDT({
    req(input$eda_columns)
    df <- cleaned_data()[, input$eda_columns, drop = FALSE]
    stat_list <- lapply(names(df), function(col) {
      if (is.numeric(df[[col]])) {
        as.character(summary(df[[col]]))
      } else {
        c("Categorical", rep("", 5))
      }
    })
    stat_df <- as.data.frame(do.call(rbind, stat_list), stringsAsFactors = FALSE)
    stat_df <- cbind(Variable = names(df), stat_df)
    colnames(stat_df)[-1] <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    datatable(stat_df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_stats <- downloadHandler(
    filename = function() {
      "eda_stats.csv"
    },
    content = function(file) {
      df <- cleaned_data()[, input$eda_columns, drop = FALSE]
      stat_list <- lapply(names(df), function(col) {
        if (is.numeric(df[[col]])) {
          as.character(summary(df[[col]]))
        } else {
          c("Categorical", rep("", 5))
        }
      })
      stat_df <- as.data.frame(do.call(rbind, stat_list), stringsAsFactors = FALSE)
      stat_df <- cbind(Variable = names(df), stat_df)
      colnames(stat_df)[-1] <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
      write.csv(stat_df, file, row.names = FALSE)
    }
  )

  output$pca_table <- renderDT({
    req(input$eda_columns, input$pca_components)
    df <- cleaned_data()[, input$eda_columns, drop = FALSE]
    df <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(df) < input$pca_components) {
      return(datatable(data.frame(Message = "Not enough numeric columns for PCA.")))
    }
    pca_model <- prcomp(df, scale. = TRUE)
    comp_table <- as.data.frame(pca_model$x[, 1:input$pca_components])
    datatable(comp_table, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_pca <- downloadHandler(
    filename = function() {
      "pca_components.csv"
    },
    content = function(file) {
      req(input$eda_columns, input$pca_components)
      df <- cleaned_data()[, input$eda_columns, drop = FALSE]
      df <- df[, sapply(df, is.numeric), drop = FALSE]
      pca_model <- prcomp(df, scale. = TRUE)
      comp_table <- as.data.frame(pca_model$x[, 1:input$pca_components])
      write.csv(comp_table, file, row.names = TRUE)
    }
  )

  observe({
    req(cleaned_data())
    all_cols <- names(cleaned_data())
    updateSelectInput(session, "target_col", choices = all_cols)
    updateSelectizeInput(session, "predictor_cols", choices = all_cols, server = TRUE)
  })

  observeEvent(input$select_all_predictors, {
    req(cleaned_data())
    all_cols <- names(cleaned_data())
    if (input$select_all_predictors) {
      updateSelectizeInput(session, "predictor_cols", selected = setdiff(all_cols, input$target_col))
    } else {
      updateSelectizeInput(session, "predictor_cols", selected = character(0))
    }
  })

  split_data <- eventReactive(input$split_data, {
    req(cleaned_data(), input$target_col)
    df <- cleaned_data()
    if (input$select_all_predictors) {
      predictors <- setdiff(names(df), input$target_col)
    } else {
      predictors <- input$predictor_cols
    }
    set.seed(123)
    trainIndex <- createDataPartition(df[[input$target_col]], p = input$train_ratio, list = FALSE)
    train <- df[trainIndex, ]
    test <- df[-trainIndex, ]
    train <- train[, c(input$target_col, predictors), drop = FALSE]
    list(train = train, test = test, predictors = predictors)
  })

  output$train_preview <- renderDT({
    req(split_data())
    datatable(split_data()$train, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_train <- downloadHandler(
    filename = function() {
      "train_set.csv"
    },
    content = function(file) {
      write.csv(split_data()$train, file, row.names = FALSE)
    }
  )

  output$download_test <- downloadHandler(
    filename = function() {
      "test_set.csv"
    },
    content = function(file) {
      write.csv(split_data()$test, file, row.names = FALSE)
    }
  )

  model_result <- eventReactive(input$run_model, {
    req(split_data(), input$target_col)
    df_split <- split_data()
    train <- df_split$train
    test <- df_split$test
    predictors <- df_split$predictors
    target <- input$target_col
    formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))

    if (is.numeric(train[[target]])) {
      if (input$model_type == "Linear Regression") {
        model <- lm(formula, data = train)
      } else if (input$model_type == "Random Forest") {
        model <- randomForest(formula, data = train)
      } else if (input$model_type == "Gradient Boosting") {
        model <- gbm(formula, data = train, distribution = "gaussian", n.trees = 100)
      } else {
        model <- lm(formula, data = train)
      }
      pred <- if (input$model_type == "Gradient Boosting") {
        predict(model, newdata = test, n.trees = 100)
      } else {
        predict(model, newdata = test)
      }
      performance <- postResample(pred, test[[target]])
      list(model = model, performance = performance)
    } else {
      train[[target]] <- as.factor(train[[target]])
      if (input$model_type == "Logistic Regression") {
        model <- glm(formula, data = train, family = binomial)
      } else if (input$model_type == "Random Forest") {
        model <- randomForest(formula, data = train)
      } else if (input$model_type == "Gradient Boosting") {
        model <- gbm(formula, data = train, distribution = "bernoulli", n.trees = 100)
      } else {
        model <- glm(formula, data = train, family = binomial)
      }
      pred_prob <- if (input$model_type == "Gradient Boosting") {
        predict(model, newdata = test, type = "response", n.trees = 100)
      } else {
        predict(model, newdata = test, type = "response")
      }
      pred <- ifelse(pred_prob > 0.5, 1, 0)
      performance <- table(Predicted = pred, Actual = test[[target]])
      list(model = model, performance = performance)
    }
  })

  output$model_summary <- renderPrint({
    req(model_result())
    if (is.numeric(cleaned_data()[[input$target_col]])) {
      summary(model_result()$model)
    } else {
      model_result()$performance
    }
  })

  output$model_plot <- renderPlotly({
    req(model_result(), split_data(), input$target_col)
    if (is.numeric(cleaned_data()[[input$target_col]])) {
      test <- split_data()$test
      pred <- if (input$model_type == "Gradient Boosting") {
        predict(model_result()$model, newdata = test, n.trees = 100)
      } else {
        predict(model_result()$model, newdata = test)
      }
      p <- plot_ly(test,
        x = ~pred, y = ~ test[[input$target_col]],
        type = "scatter", mode = "markers", marker = list(color = "blue")
      )
      p <- p %>% layout(
        title = "Predicted vs Actual", xaxis = list(title = "Predicted"),
        yaxis = list(title = "Actual")
      )
      p
    } else {
      test <- split_data()$test
      pred_prob <- if (input$model_type == "Gradient Boosting") {
        predict(model_result()$model, newdata = test, type = "response", n.trees = 100)
      } else {
        predict(model_result()$model, newdata = test, type = "response")
      }
      p <- plot_ly(x = ~pred_prob, type = "histogram", marker = list(color = "red"))
      p <- p %>% layout(
        title = "Histogram of Predicted Probabilities",
        xaxis = list(title = "Predicted Probability"),
        yaxis = list(title = "Count")
      )
      p
    }
  })

  output$download_model <- downloadHandler(
    filename = function() {
      "model_summary.txt"
    },
    content = function(file) {
      sink(file)
      print(model_result()$model)
      sink()
    }
  )

  # Navigation Observers for Next Buttons in Sidebar Panels

  observeEvent(input$next_home, {
    updateNavbarPage(session, "tabs", selected = "Preprocessing")
  })

  observeEvent(input$next_preprocessing, {
    log_interaction(session_data$id, session_data$group, "next_preprocessing")
    runjs(sprintf("window.trackButtonClick('next_preprocessing', '%s');", session_data$group))
    updateNavbarPage(session, "tabs", selected = "Feature Engineering")
  })

  observeEvent(input$next_feature, {
    updateNavbarPage(session, "tabs", selected = "Exploratory Data Analysis")
  })

  observeEvent(input$next_eda, {
    updateNavbarPage(session, "tabs", selected = "Modeling")
  })
}

shinyApp(ui = ui, server = server)
