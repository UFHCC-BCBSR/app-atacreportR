# app.R
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(DT)

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f4f4; }
      .title-section { text-align: center; padding: 30px 0; background-color: #2c3e50; color: white; }
      .step-section { background-color: white; margin: 20px; padding: 30px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .btn-primary { background-color: #3498db; border-color: #3498db; }
      .btn-success { background-color: #27ae60; border-color: #27ae60; }
      .btn-danger { background-color: #e74c3c; border-color: #e74c3c; }
      .login-box {
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
      }
      .auth-status {
        padding: 10px;
        border-radius: 4px;
        margin-bottom: 15px;
      }
      .auth-success { background-color: #d4edda; border: 1px solid #c3e6cb; color: #155724; }
      .auth-needed { background-color: #fff3cd; border: 1px solid #ffeeba; color: #856404; }
      .selected-file {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        padding: 8px 12px;
        margin-top: 8px;
        font-family: monospace;
        font-size: 11px;
        color: #495057;
        max-height: 60px;
        overflow-y: auto;
      }
      .param-group {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 20px;
        background-color: #f8f9fa;
      }
      .param-group h4 {
        margin-top: 0;
        color: #495057;
        border-bottom: 1px solid #dee2e6;
        padding-bottom: 8px;
      }
      .validation-message {
        padding: 10px;
        border-radius: 4px;
        margin: 10px 0;
      }
      .validation-success { background-color: #d4edda; border: 1px solid #c3e6cb; color: #155724; }
      .validation-error { background-color: #f8d7da; border: 1px solid #f5c6cb; color: #721c24; }
      .validation-warning { background-color: #fff3cd; border: 1px solid #ffeeba; color: #856404; }
      .shinyDirectories .btn[title='Create new folder'] { display: none !important; }
      .shinyDirectories .fa-plus { display: none !important; }
      .shinyDirectories .fa-edit { display: none !important; }
      button#sF-btn-textChoice { display: none !important; }
      .glyphicon-pencil { display: none !important; }
    "))
  ),
  
  div(class = "title-section",
      h1("ATAC-seq Analysis Parameter Generator", style = "margin: 0; font-size: 48px; font-weight: 300;"),
      p("Configure parameters for differential accessibility analysis", style = "margin: 10px 0 0 0; font-size: 14px; opacity: 0.8;")
  ),
  # Add this new section right after the title section and before authentication:
  
  # Load Existing Parameters section
  div(class = "step-section",
      h3("Load Existing Parameters (Optional)", style = "text-align: center; margin-bottom: 20px;"),
      p("Start from an existing params.txt file to make modifications:", style = "text-align: center; color: #6c757d;"),
      
      # Option 1: Browse for existing params file
      div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
          h5("Browse HiPerGator for Existing params.txt"),
          conditionalPanel(
            condition = "output.authenticated",
            shinyFilesButton("browse_existing_params", "Browse Existing params.txt", "Select params file", class = "btn-secondary", multiple = FALSE),
            uiOutput("selected_existing_params_file")
          ),
          conditionalPanel(
            condition = "!output.authenticated",
            div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
                tags$i(class = "fa fa-lock"), " Login above to browse HiPerGator files"
            )
          )
      ),
      
      # Option 2: Upload params file
      div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
          h5("Upload Existing params.txt"),
          fileInput("upload_existing_params", "Upload params.txt file", accept = ".txt"),
          uiOutput("uploaded_params_status")
      ),
      
      # Load button and status
      div(style = "text-align: center; margin-top: 20px;",
          actionButton("load_existing_params", "Load Parameters", class = "btn-warning btn-lg", disabled = TRUE),
          br(), br(),
          uiOutput("params_load_status")
      )
  ),
  
  # Authentication section
  div(class = "step-section",
      h3("HiPerGator File Access", style = "text-align: center; margin-bottom: 20px;"),
      uiOutput("auth_status"),
      conditionalPanel(
        condition = "!output.authenticated",
        div(class = "login-box",
            h4("Login for HiPerGator File Access"),
            p("Enter your group credentials to browse HiPerGator files:"),
            fluidRow(
              column(6, textInput("group_name", "HiperGator Group", placeholder = "e.g., cancercenter-dept")),
              column(6, passwordInput("group_password", "Password"))
            ),
            actionButton("login_btn", "Login", class = "btn-primary")
        )
      ),
      conditionalPanel(
        condition = "output.authenticated",
        div(style = "text-align: right;",
            actionButton("logout_btn", "Logout", class = "btn-secondary btn-sm")
        )
      )
  ),
  
  # Required Parameters
  div(class = "step-section",
      h2("Required Parameters", style = "text-align: center; margin-bottom: 30px;"),
      
      div(class = "param-group",
          h4("Basic Configuration"),
          fluidRow(
            column(6, textInput("seqID", "Sequence ID (Project ID)", placeholder = "my-test")),
            column(6, textInput("report_title", "Report Title", value = "Differential Accessibility Analysis"))
          ),
          fluidRow(
            column(4, selectInput("organism", "Organism", choices = list("Mouse (mmu)" = "mmu", "Human (hsa)" = "hsa"), selected = "mmu")),
            column(4, selectInput("annotation_db", "Annotation Database", choices = list("org.Mm.eg.db" = "org.Mm.eg.db", "org.Hs.eg.db" = "org.Hs.eg.db"), selected = "org.Mm.eg.db")),
            column(4, textInput("hipergator_group", "HiPerGator Group", placeholder = "e.g., cancercenter-dept"))
          ),
          textInput("output_path", "Output Path", placeholder = "/blue/your-group/path/to/output"),
	  textInput("user_email", "Email Address", 
          placeholder = "your.email@ufl.edu (required for notification)")
      ),
	  div(class = "param-group",
	      h4("Sample Sheet"),
	      p("Choose how to provide your sample sheet CSV file:"),
	      
	      # Option 1: HiPerGator browse with download
	      div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
	          h5("Browse HiPerGator Files"),
	          p("Select a file from HiPerGator. Use the download button if you want to edit it locally first."),
	          conditionalPanel(
	            condition = "output.authenticated",
	            fluidRow(
	              column(6, 
	                     shinyFilesButton("browse_sample_sheet", "Browse HiPerGator", "Select CSV file", class = "btn-info", multiple = FALSE),
	                     uiOutput("selected_sample_sheet_browse")
	              ),
	              column(6,
	                     downloadButton("download_sample_sheet", "Download Selected File", class = "btn-secondary"),
	                     tags$br(),
	                     tags$small("Download to edit locally, then use Upload option below", style = "color: #6c757d;")
	              )
	            )
	          ),
	          conditionalPanel(
	            condition = "!output.authenticated",
	            div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
	                tags$i(class = "fa fa-lock"), " Login below to browse HiPerGator files"
	            )
	          )
	      ),
	      
	      # Option 2: Upload
	      div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
	          h5("Upload File"),
	          p("Upload a sample sheet from your computer (including files you've downloaded and edited)."),
	          fileInput("upload_sample_sheet", "Choose CSV File", accept = ".csv")
	      ),
	      
	      # Show active selection
	      uiOutput("active_sample_sheet_status")
	  ),
      
      div(class = "param-group",
          h4("Peak Files"),
          p("Select multiple peak files (.broadPeak format). Files will be matched to samples by basename:"),
          conditionalPanel(
            condition = "output.authenticated",
            shinyFilesButton("browse_peak_files", "Browse Peak Files", "Select multiple peak files", class = "btn-info", multiple = TRUE),
            uiOutput("selected_peak_files")
          ),
          conditionalPanel(
            condition = "!output.authenticated",
            div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
                tags$i(class = "fa fa-lock"), " Login above to browse HiPerGator files"
            )
          )
      ),
      
      div(class = "param-group",
          h4("BigWig Files"),
          p("Select multiple BigWig files. Files will be matched to samples by basename:"),
          conditionalPanel(
            condition = "output.authenticated",
            shinyFilesButton("browse_bigwig_files", "Browse BigWig Files", "Select multiple BigWig files", class = "btn-info", multiple = TRUE),
            uiOutput("selected_bigwig_files")
          ),
          conditionalPanel(
            condition = "!output.authenticated",
            div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
                tags$i(class = "fa fa-lock"), " Login above to browse HiPerGator files"
            )
          )
      ),
      
      div(class = "param-group",
          h4("Filtering Parameters"),
          fluidRow(
            column(6, numericInput("min_count_for_filtering", "Min Count for Filtering", value = 10, min = 0)),
            column(6, numericInput("min_prop_for_filtering", "Min Proportion for Filtering", value = 0.5, min = 0, max = 1, step = 0.05))
          )
      )
  ),
  
  # Optional Parameters
  div(class = "step-section",
      h2("Optional Parameters", style = "text-align: center; margin-bottom: 30px;"),
      
      div(class = "param-group",
          h4("Contrasts"),
          p("Specify contrasts either by uploading a file OR entering text:"),
          conditionalPanel(
            condition = "output.authenticated",
            shinyFilesButton("browse_contrasts", "Browse Contrasts File", "Select contrasts file", class = "btn-info", multiple = FALSE),
            uiOutput("selected_contrasts")
          ),
          textInput("contrasts_text", "OR Enter Contrasts (comma-separated)", placeholder = "group1_vs_group2,treatment_vs_control")
      ),
      
      div(class = "param-group",
          h4("Analysis Files (Either DDS file OR BAM files required)"),
          conditionalPanel(
            condition = "output.authenticated",
            div(
              h5("DDS File (if available):"),
              shinyFilesButton("browse_dds_file", "Browse DDS File", "Select DDS RData file", class = "btn-info", multiple = FALSE),
              uiOutput("selected_dds_file"),
              br(),
              h5("BAM Files (alternative to DDS):"),
              shinyFilesButton("browse_bam_files", "Browse BAM Files", "Select multiple BAM files", class = "btn-info", multiple = TRUE),
              uiOutput("selected_bam_files")
            )
          )
      ),
      
      div(class = "param-group",
          h4("Additional Analysis Files"),
          conditionalPanel(
            condition = "output.authenticated",
            div(
              h5("Peak Annotation:"),
              shinyFilesButton("browse_peak_annotation", "Browse Peak Annotation", "Select annotation file", class = "btn-info", multiple = FALSE),
              uiOutput("selected_peak_annotation"),
              br(),
              h5("QC Flagstat Directory:"),
              shinyDirButton("browse_qc_flagstat_dir", "Browse QC Flagstat Directory", "Select directory", class = "btn-info"),
              uiOutput("selected_qc_flagstat_dir"),
              br(),
              h5("QC FRIP File:"),
              shinyFilesButton("browse_qc_frip_file", "Browse QC FRIP File", "Select FRIP file", class = "btn-info", multiple = FALSE),
              uiOutput("selected_qc_frip_file")
            )
          )
      ),
      
      div(class = "param-group",
          h4("URLs (Optional)"),
          textInput("raw_seq_URL", "Raw Sequencing Data URL", placeholder = "https://..."),
          textInput("multiqc_url", "MultiQC Results URL", placeholder = "https://...")
      )
  ),
  # Add this new section after the Optional Parameters section and before the Validation section:
  
  # **Report Metadata**
  div(class = "step-section",
      h2("Report Metadata (Optional)", style = "text-align: center; margin-bottom: 30px;"),
      p("These fields are used to populate the report header and summary. All are optional.", 
        style = "text-align: center; color: #6c757d; margin-bottom: 20px;"),
      
      div(class = "param-group",
          h4("Project Information"),
          fluidRow(
            column(6, textInput("PI", "Principal Investigator", placeholder = "Dr. Jane Smith")),
            column(6, textInput("Institution", "Institution", placeholder = "University of Florida"))
          ),
          fluidRow(
            column(6, textInput("Department", "Department", placeholder = "Department of Medicine")),
            column(6, textInput("Study_Contact", "Study Contact", placeholder = "jane.smith@ufl.edu"))
          ),
          textInput("Project_Title", "Project Title", placeholder = "ATAC-seq analysis of...")
      ),
      
      div(class = "param-group",
          h4("Study Details"),
          textAreaInput("Study_Summary", "Study Summary", 
                        placeholder = "Brief description of the study objectives and design...",
                        rows = 3),
          fluidRow(
            column(6, textInput("Sample_Types", "Sample Type(s)", placeholder = "Cell lines, tissue samples, etc.")),
            column(6, textInput("Analysis_Goals", "Analysis Goal(s)", placeholder = "Differential accessibility, pathway analysis, etc."))
          )
      ),
      
      div(class = "param-group",
          h4("Report Credits"),
          fluidRow(
            column(6, textInput("Report_Prepared_By", "Report Prepared By", placeholder = "Analyst Name")),
            column(6, textInput("Report_Reviewed_By", "Report Reviewed By", placeholder = "Reviewer Name"))
          )
      )
  ),
  
  # Validation and Generation
  # Replace the existing "Generate Parameters" section with this:
  
  div(class = "step-section",
      h2("Generate Parameters", style = "text-align: center; margin-bottom: 30px;"),
      div(style = "text-align: center;",
          actionButton("validate_params", "Validate Parameters", class = "btn-secondary btn-lg"),
          br(), br(),
          actionButton("generate_params", "Generate params.txt", class = "btn-success btn-lg", disabled = TRUE),
          br(), br(),
          downloadButton("download_params", "Download params.txt", class = "btn-info btn-lg", disabled = TRUE),
          br(), br(),
          actionButton("submit_job", "Submit SLURM Job", class = "btn-primary btn-lg", disabled = TRUE)
      ),
      br(),
      uiOutput("validation_status"),
      br(),
      verbatimTextOutput("params_preview")
  )
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues(
    authenticated = FALSE,
    current_group = NULL,
    selected_files = list(),
    params_valid = FALSE,
    params_generated = FALSE,
    validation_messages = c(),
    params_loaded = FALSE,        # Add this
    user_made_changes = FALSE     # Add this
  )
  
  # Group passwords
  group_passwords <- list(
    "cancercenter-dept" = "Abc123",
    "licht" = "licht123"
  )
  
  # Authentication
  output$authenticated <- reactive({ values$authenticated })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)
  
  output$auth_status <- renderUI({
    if (values$authenticated) {
      div(class = "auth-status auth-success",
          tags$i(class = "fa fa-check-circle"),
          strong("Authenticated: "),
          paste("Logged in as", values$current_group),
          " - HiPerGator file browsing enabled"
      )
    } else {
      div(class = "auth-status auth-needed",
          tags$i(class = "fa fa-info-circle"),
          strong("HiPerGator Access: "),
          "Login required to browse HiPerGator files."
      )
    }
  })
  
  # Login handler
  observeEvent(input$login_btn, {
    req(input$group_name, input$group_password)
    if (input$group_name %in% names(group_passwords) &&
        input$group_password == group_passwords[[input$group_name]]) {
      values$authenticated <- TRUE
      values$current_group <- input$group_name
      
      # Set up file browser roots
      group_path <- paste0("/blue/", values$current_group)
      if (dir.exists(group_path)) {
        group_volumes <- setNames(group_path, values$current_group)
        
        # Initialize all file browsers
        shinyFileChoose(input, "browse_sample_sheet", roots = group_volumes, session = session, filetypes = c("", "csv"))
        shinyFileChoose(input, "browse_existing_params", roots = group_volumes, session = session, filetypes = c("", "txt"))
        shinyFileChoose(input, "browse_peak_files", roots = group_volumes, session = session, filetypes = c("", "broadPeak", "narrowPeak"))
        shinyFileChoose(input, "browse_bigwig_files", roots = group_volumes, session = session, filetypes = c("", "bigWig", "bw"))
        shinyFileChoose(input, "browse_contrasts", roots = group_volumes, session = session, filetypes = c("", "txt", "csv"))
        shinyFileChoose(input, "browse_dds_file", roots = group_volumes, session = session, filetypes = c("", "RData", "rda"))
        shinyFileChoose(input, "browse_bam_files", roots = group_volumes, session = session, filetypes = c("", "bam"))
        shinyFileChoose(input, "browse_peak_annotation", roots = group_volumes, session = session, filetypes = c("", "txt"))
        shinyDirChoose(input, "browse_qc_flagstat_dir", roots = group_volumes, session = session)
        shinyFileChoose(input, "browse_qc_frip_file", roots = group_volumes, session = session, filetypes = c("", "txt"))
      }
      showNotification("Login successful! HiPerGator file browsing enabled.", type = "message")
    } else {
      showNotification("Invalid credentials", type = "error")
    }
  })
  
  # Logout handler
  observeEvent(input$logout_btn, {
    values$authenticated <- FALSE
    values$current_group <- NULL
    values$selected_files <- list()
    showNotification("Logged out - file browsing disabled", type = "message")
  })
  
  # File selection handlers
  setup_file_browser <- function(browser_id, file_key, multiple = FALSE) {
    observeEvent(input[[browser_id]], {
      if (values$authenticated && !is.null(values$current_group)) {
        group_path <- paste0("/blue/", values$current_group)
        group_volumes <- setNames(group_path, values$current_group)
        
        tryCatch({
          if (!is.null(input[[browser_id]]) && length(input[[browser_id]]) > 0 && !is.integer(input[[browser_id]])) {
            if (browser_id == "browse_qc_flagstat_dir") {
              # Handle directory selection
              selected_dir <- parseDirPath(group_volumes, input[[browser_id]])
              if (length(selected_dir) > 0) {
                values$selected_files[[file_key]] <- selected_dir
                showNotification(paste("Selected directory:", basename(selected_dir)), type = "message")
              }
            } else {
              # Handle file selection
              selected_files <- parseFilePaths(group_volumes, input[[browser_id]])
              if (!is.null(selected_files) && nrow(selected_files) > 0) {
                if (multiple) {
                  values$selected_files[[file_key]] <- selected_files$datapath
                  showNotification(paste("Selected", nrow(selected_files), "files"), type = "message")
                } else {
                  values$selected_files[[file_key]] <- selected_files$datapath[1]
                  showNotification(paste("Selected:", basename(selected_files$name[1])), type = "message")
                }
              }
            }
          }
        }, error = function(e) {
          # Handle errors silently
        })
      }
    }, ignoreInit = TRUE)
  }
  
  # Set up all file browsers
  setup_file_browser("browse_sample_sheet", "sample_sheet")
  setup_file_browser("browse_existing_params", "existing_params")
  setup_file_browser("browse_peak_files", "peak_files", multiple = TRUE)
  setup_file_browser("browse_bigwig_files", "bigwig_files", multiple = TRUE)
  setup_file_browser("browse_contrasts", "contrasts")
  setup_file_browser("browse_dds_file", "dds_file")
  setup_file_browser("browse_bam_files", "bam_files", multiple = TRUE)
  setup_file_browser("browse_peak_annotation", "peak_annotation")
  setup_file_browser("browse_qc_flagstat_dir", "qc_flagstat_dir")
  setup_file_browser("browse_qc_frip_file", "qc_frip_file")
  
  # File display outputs
  # Sample sheet handling - add this to your server function
  
  # Replace the sample sheet server logic with this:
  
  # Track which option is active
  values$sample_sheet_source <- NULL
  values$sample_sheet_path <- NULL
  
  # Option 1: HiPerGator browse
  setup_file_browser("browse_sample_sheet", "sample_sheet_browse")
  
  observeEvent(values$selected_files$sample_sheet_browse, {
    if (!is.null(values$selected_files$sample_sheet_browse)) {
      values$sample_sheet_source <- "hipergator"
      values$sample_sheet_path <- values$selected_files$sample_sheet_browse
      values$selected_files$sample_sheet <- values$selected_files$sample_sheet_browse
      showNotification("HiPerGator file selected", type = "message")
    }
  })
  
  # Display selected HiPerGator file
  output$selected_sample_sheet_browse <- renderUI({
    if (!is.null(values$selected_files$sample_sheet_browse)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$sample_sheet_browse),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$sample_sheet_browse)
      )
    }
  })
  
  # Download handler for HiPerGator file
  output$download_sample_sheet <- downloadHandler(
    filename = function() {
      if (!is.null(values$selected_files$sample_sheet_browse)) {
        basename(values$selected_files$sample_sheet_browse)
      } else {
        "sample_sheet.csv"
      }
    },
    content = function(file) {
      if (!is.null(values$selected_files$sample_sheet_browse)) {
        file.copy(values$selected_files$sample_sheet_browse, file)
      }
    }
  )
  
  # Option 2: Upload
  observeEvent(input$upload_sample_sheet, {
    if (!is.null(input$upload_sample_sheet)) {
      # Create permanent directory
      permanent_dir <- file.path(dirname(getwd()), "uploaded_files")
      dir.create(permanent_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Copy to permanent location with original filename
      permanent_path <- file.path(permanent_dir, input$upload_sample_sheet$name)
      file.copy(input$upload_sample_sheet$datapath, permanent_path, overwrite = TRUE)
      
      # Store the permanent path instead of temp path
      values$sample_sheet_source <- "upload"
      values$sample_sheet_path <- permanent_path
      values$selected_files$sample_sheet <- permanent_path
      
      showNotification("File uploaded and saved permanently!", type = "message")
    }
  })
  
  # Test the upload observer
  # Fix the upload observer - copy to permanent location
  observeEvent(input$upload_existing_params, {
    cat("DEBUG: Upload observer triggered\n")
    if (!is.null(input$upload_existing_params)) {
      cat("DEBUG: File uploaded -", input$upload_existing_params$name, "\n")
      
      # Create permanent directory (same as sample sheet logic)
      permanent_dir <- file.path(dirname(getwd()), "uploaded_files")
      dir.create(permanent_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Copy to permanent location with original filename
      permanent_path <- file.path(permanent_dir, input$upload_existing_params$name)
      file.copy(input$upload_existing_params$datapath, permanent_path, overwrite = TRUE)
      
      # Store the permanent path instead of temp path
      values$existing_params_file <- permanent_path
      
      cat("DEBUG: Stored permanent file path -", values$existing_params_file, "\n")
      shinyjs::enable("load_existing_params")
      cat("DEBUG: Button enabled\n")
      showNotification("Params file uploaded - click Load Parameters to populate fields", type = "message")
    }
  })
  
  # Add this for browse selection:
  observeEvent(values$selected_files$existing_params, {
    if (!is.null(values$selected_files$existing_params)) {
      values$existing_params_file <- values$selected_files$existing_params
      shinyjs::enable("load_existing_params")
      showNotification("Params file selected - click Load Parameters to populate fields", type = "message")
    }
  })
  
  parse_params_file <- function(params_file) {
    if (!file.exists(params_file)) {
      stop("Parameters file not found")
    }
    
    lines <- readLines(params_file)
    cat("DEBUG: Read", length(lines), "lines from file\n")
    
    params <- list()
    
    for (i in seq_along(lines)) {
      line <- lines[i]
      line <- trimws(line)
      
      if (line == "" || startsWith(line, "#")) {
        next
      }
      
      # Parse --param_name "value" or --param_name value
      if (startsWith(line, "--")) {
        cat("DEBUG: Processing line:", substr(line, 1, 60), "...\n")
        
        # Find the first space after --
        space_pos <- regexpr("\\s", line)
        
        if (space_pos > 0) {
          param_name <- gsub("^--", "", substr(line, 1, space_pos - 1))
          param_value <- trimws(substr(line, space_pos + 1, nchar(line)))
          
          cat("DEBUG: param_name:", param_name, "\n")
          cat("DEBUG: param_value (before quote removal):", substr(param_value, 1, 50), "...\n")
          
          # Remove surrounding quotes if present
          if ((startsWith(param_value, '"') && endsWith(param_value, '"')) ||
              (startsWith(param_value, "'") && endsWith(param_value, "'"))) {
            param_value <- substr(param_value, 2, nchar(param_value) - 1)
            cat("DEBUG: Removed quotes, new value:", substr(param_value, 1, 50), "...\n")
          }
          
          params[[param_name]] <- param_value
          cat("DEBUG: Added parameter:", param_name, "\n")
        } else {
          cat("DEBUG: No space found in line\n")
        }
      }
    }
    
    cat("DEBUG: Final parameter count:", length(params), "\n")
    cat("DEBUG: Parameter names:", paste(names(params), collapse = ", "), "\n")
    
    return(params)
  }
  `%||%` <- function(x, y) if (is.null(x)) y else x
  # Updated load existing parameters function with better debugging
  observeEvent(input$load_existing_params, {
    req(values$existing_params_file)
    # Add this right after req(values$existing_params_file) in your load observer:
    cat("DEBUG: About to read file:", values$existing_params_file, "\n")
    raw_lines <- readLines(values$existing_params_file)
    cat("DEBUG: Raw file has", length(raw_lines), "lines\n")
    cat("DEBUG: First line raw:", raw_lines[1], "\n")
    cat("DEBUG: First line bytes:", paste(utf8ToInt(raw_lines[1]), collapse = " "), "\n")
    cat("DEBUG: Load button clicked, file path:", values$existing_params_file, "\n")
    
    tryCatch({
      # Check if file exists
      if (!file.exists(values$existing_params_file)) {
        stop("File not found: ", values$existing_params_file)
      }
      
      cat("DEBUG: File exists, parsing...\n")
      params <- parse_params_file(values$existing_params_file)
      cat("DEBUG: Parsed", length(params), "parameters\n")
      cat("DEBUG: Parameter names:", paste(names(params), collapse = ", "), "\n")
      
      # Test one parameter update
      if ("seqID" %in% names(params)) {
        cat("DEBUG: Updating seqID to:", params$seqID, "\n")
        updateTextInput(session, "seqID", value = params$seqID)
      }
      
      # Populate ALL UI text inputs
      if ("report_title" %in% names(params)) {
        cat("DEBUG: Updating report_title to:", params$report_title, "\n")
        updateTextInput(session, "report_title", value = params$report_title)
      }
      if ("organism" %in% names(params)) {
        updateSelectInput(session, "organism", selected = params$organism)
      }
      if ("annotation_db" %in% names(params)) {
        updateSelectInput(session, "annotation_db", selected = params$annotation_db)
      }
      
      hipergator_group_key <- if ("hipergator-group" %in% names(params)) "hipergator-group" else "hipergator_group"
      if (hipergator_group_key %in% names(params)) {
        updateTextInput(session, "hipergator_group", value = params[[hipergator_group_key]])
      }
      
      output_path_key <- if ("output-path" %in% names(params)) "output-path" else "output_path"
      if (output_path_key %in% names(params)) {
        updateTextInput(session, "output_path", value = params[[output_path_key]])
      }
      
      if ("user_email" %in% names(params)) {
        updateTextInput(session, "user_email", value = params$user_email)
      }
      
      # Populate numeric inputs
      if ("min_count_for_filtering" %in% names(params)) {
        updateNumericInput(session, "min_count_for_filtering", value = as.numeric(params$min_count_for_filtering))
      }
      if ("min_prop_for_filtering" %in% names(params)) {
        updateNumericInput(session, "min_prop_for_filtering", value = as.numeric(params$min_prop_for_filtering))
      }
      
      # Populate optional URLs
      if ("raw_seq_URL" %in% names(params)) {
        updateTextInput(session, "raw_seq_URL", value = params$raw_seq_URL)
      }
      if ("multiqc_url" %in% names(params)) {
        updateTextInput(session, "multiqc_url", value = params$multiqc_url)
      }
      
      # Load report metadata parameters
      if ("PI" %in% names(params)) {
        updateTextInput(session, "PI", value = params$PI)
      }
      if ("Institution" %in% names(params)) {
        updateTextInput(session, "Institution", value = params$Institution)
      }
      if ("Department" %in% names(params)) {
        updateTextInput(session, "Department", value = params$Department)
      }
      if ("Study_Contact" %in% names(params)) {
        updateTextInput(session, "Study_Contact", value = params$Study_Contact)
      }
      if ("Project_Title" %in% names(params)) {
        updateTextInput(session, "Project_Title", value = params$Project_Title)
      }
      if ("Study_Summary" %in% names(params)) {
        updateTextAreaInput(session, "Study_Summary", value = params$Study_Summary)
      }
      if ("Sample_Types" %in% names(params)) {
        updateTextInput(session, "Sample_Types", value = params$Sample_Types)
      }
      if ("Analysis_Goals" %in% names(params)) {
        updateTextInput(session, "Analysis_Goals", value = params$Analysis_Goals)
      }
      if ("Report_Prepared_By" %in% names(params)) {
        updateTextInput(session, "Report_Prepared_By", value = params$Report_Prepared_By)
      }
      if ("Report_Reviewed_By" %in% names(params)) {
        updateTextInput(session, "Report_Reviewed_By", value = params$Report_Reviewed_By)
      }
      
      # Handle contrasts
      if ("contrasts" %in% names(params)) {
        if (!file.exists(params$contrasts)) {
          updateTextInput(session, "contrasts_text", value = params$contrasts)
        } else {
          values$selected_files$contrasts <- params$contrasts
        }
      }
      
      # Store file paths
      if ("sample_sheet" %in% names(params)) {
        values$selected_files$sample_sheet <- params$sample_sheet
        values$sample_sheet_source <- "loaded_from_params"
        values$sample_sheet_path <- params$sample_sheet
      }
      
      # Parse and store file lists
      parse_file_pairs <- function(param_value) {
        if (is.null(param_value) || param_value == "") return(NULL)
        param_value <- gsub('^["\']|["\']$', '', param_value)
        pairs <- strsplit(param_value, ",")[[1]]
        sapply(pairs, function(x) {
          x <- gsub('^["\']|["\']$', '', trimws(x))
          parts <- strsplit(x, ":")[[1]]
          if (length(parts) >= 2) parts[2] else x
        })
      }
      
      if ("peak_files" %in% names(params)) {
        values$selected_files$peak_files <- parse_file_pairs(params$peak_files)
      }
      if ("bigwig_files" %in% names(params)) {
        values$selected_files$bigwig_files <- parse_file_pairs(params$bigwig_files)
      }
      if ("bam_files" %in% names(params)) {
        values$selected_files$bam_files <- parse_file_pairs(params$bam_files)
      }
      if ("dds_file" %in% names(params)) {
        values$selected_files$dds_file <- params$dds_file
      }
      if ("peak_annotation" %in% names(params)) {
        values$selected_files$peak_annotation <- params$peak_annotation
      }
      if ("qc_flagstat_dir" %in% names(params)) {
        values$selected_files$qc_flagstat_dir <- params$qc_flagstat_dir
      }
      if ("qc_frip_file" %in% names(params)) {
        values$selected_files$qc_frip_file <- params$qc_frip_file
      }
      
      cat("DEBUG: All updates completed successfully\n")
      showNotification("Parameters loaded successfully from file!", type = "message")
      
    }, error = function(e) {
      cat("DEBUG: Error in load_existing_params:", e$message, "\n")
      showNotification(paste("Error loading parameters:", e$message), type = "error")
    })
  })
  
  # Show active sample sheet status (fixes the "0.csv" issue)
  output$active_sample_sheet_status <- renderUI({
    if (!is.null(values$sample_sheet_source) && !is.null(values$sample_sheet_path)) {
      
      # Get the correct filename based on source
      filename <- if (values$sample_sheet_source == "hipergator") {
        basename(values$sample_sheet_path)
      } else if (values$sample_sheet_source == "upload") {
        input$upload_sample_sheet$name  # This fixes the "0.csv" issue
      } else {
        basename(values$sample_sheet_path)
      }
      
      source_text <- switch(values$sample_sheet_source,
                            "hipergator" = "Using HiPerGator file",
                            "upload" = "Using uploaded file",
                            "loaded_from_params" = "Loaded from params.txt"
      )
      
      div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 15px;",
          tags$i(class = "fa fa-check-circle", style = "color: #155724;"),
          strong(" Active: "), source_text,
          br(),
          strong("File: "), filename
      )
    }
  })
  

  output$selected_peak_files <- renderUI({
    if (!is.null(values$selected_files$peak_files)) {
      files <- values$selected_files$peak_files
      div(class = "selected-file",
          strong(paste("Selected", length(files), "peak files:")),
          tags$br(),
          paste(basename(files), collapse = ", ")
      )
    }
  })
  
  output$selected_bigwig_files <- renderUI({
    if (!is.null(values$selected_files$bigwig_files)) {
      files <- values$selected_files$bigwig_files
      div(class = "selected-file",
          strong(paste("Selected", length(files), "BigWig files:")),
          tags$br(),
          paste(basename(files), collapse = ", ")
      )
    }
  })
  
  output$selected_contrasts <- renderUI({
    if (!is.null(values$selected_files$contrasts)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$contrasts),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$contrasts)
      )
    }
  })
  
  output$selected_dds_file <- renderUI({
    if (!is.null(values$selected_files$dds_file)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$dds_file),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$dds_file)
      )
    }
  })
  
  output$selected_bam_files <- renderUI({
    if (!is.null(values$selected_files$bam_files)) {
      files <- values$selected_files$bam_files
      div(class = "selected-file",
          strong(paste("Selected", length(files), "BAM files:")),
          tags$br(),
          paste(basename(files), collapse = ", ")
      )
    }
  })
  
  output$selected_peak_annotation <- renderUI({
    if (!is.null(values$selected_files$peak_annotation)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$peak_annotation),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$peak_annotation)
      )
    }
  })
  output$selected_qc_flagstat_dir <- renderUI({
    if (!is.null(values$selected_files$qc_flagstat_dir)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$qc_flagstat_dir),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$qc_flagstat_dir)
      )
    }
  })
  
  output$selected_qc_frip_file <- renderUI({
    if (!is.null(values$selected_files$qc_frip_file)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$qc_frip_file),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$qc_frip_file)
      )
    }
  })
  
  # Add these output handlers
  output$selected_existing_params_file <- renderUI({
    if (!is.null(values$selected_files$existing_params)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$existing_params),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$existing_params)
      )
    }
  })
  
  output$uploaded_params_status <- renderUI({
    if (!is.null(values$existing_params_file)) {
      div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;",
          tags$i(class = "fa fa-check-circle", style = "color: #155724;"),
          strong(" File ready: "), basename(values$existing_params_file)
      )
    }
  })
  
  output$params_load_status <- renderUI({
    if (!is.null(input$load_existing_params) && input$load_existing_params > 0) {
      div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;",
          tags$i(class = "fa fa-info-circle", style = "color: #155724;"),
          strong("Parameters loaded! "), "You can now modify fields and regenerate."
      )
    }
  })
  
  # Validation function
  # Updated validation function that handles loaded params
  validate_parameters <- function() {
    messages <- c()
    
    # Check required fields
    if (is.null(input$seqID) || input$seqID == "") {
      messages <- c(messages, "❌ Sequence ID is required")
    }
    if (is.null(input$hipergator_group) || input$hipergator_group == "") {
      messages <- c(messages, "❌ HiPerGator Group is required")
    }
    if (is.null(input$output_path) || input$output_path == "") {
      messages <- c(messages, "❌ Output path is required")
    }
    if (is.null(values$selected_files$sample_sheet)) {
      messages <- c(messages, "❌ Sample sheet is required")
    }
    if (is.null(values$selected_files$peak_files)) {
      messages <- c(messages, "❌ Peak files are required")
    }
    if (is.null(values$selected_files$bigwig_files)) {
      messages <- c(messages, "❌ BigWig files are required")
    }
    
    # Check that either DDS file OR BAM files are provided
    has_dds <- !is.null(values$selected_files$dds_file)
    has_bam <- !is.null(values$selected_files$bam_files)
    if (!has_dds && !has_bam) {
      messages <- c(messages, "❌ Either DDS file OR BAM files must be provided")
    }
    
    # Validate contrasts
    has_contrasts_file <- !is.null(values$selected_files$contrasts)
    has_contrasts_text <- !is.null(input$contrasts_text) && input$contrasts_text != ""
    if (!has_contrasts_file && !has_contrasts_text) {
      messages <- c(messages, "⚠️ No contrasts specified - analysis will run without differential testing")
    }
    
    # Validate file matching if sample sheet is available
    if (!is.null(values$selected_files$sample_sheet)) {
      tryCatch({
        sample_df <- read.csv(values$selected_files$sample_sheet, stringsAsFactors = FALSE)
        if (!("sample" %in% colnames(sample_df))) {
          messages <- c(messages, "❌ Sample sheet must have a 'sample' column")
        } else {
          # Check if peak files can be matched to samples
          if (!is.null(values$selected_files$peak_files)) {
            peak_basenames <- basename(values$selected_files$peak_files)
            matched_peaks <- 0
            for (sample in sample_df$sample) {
              if (any(grepl(sample, peak_basenames, fixed = TRUE))) {
                matched_peaks <- matched_peaks + 1
              }
            }
            if (matched_peaks == 0) {
              messages <- c(messages, "❌ No peak files could be matched to sample names")
            } else if (matched_peaks < nrow(sample_df)) {
              messages <- c(messages, paste("⚠️ Only", matched_peaks, "of", nrow(sample_df), "samples matched to peak files"))
            } else {
              messages <- c(messages, paste("✅", matched_peaks, "samples successfully matched to peak files"))
            }
          }
          
          # Check BigWig files
          if (!is.null(values$selected_files$bigwig_files)) {
            bigwig_basenames <- basename(values$selected_files$bigwig_files)
            matched_bigwigs <- 0
            for (sample in sample_df$sample) {
              if (any(grepl(sample, bigwig_basenames, fixed = TRUE))) {
                matched_bigwigs <- matched_bigwigs + 1
              }
            }
            if (matched_bigwigs == 0) {
              messages <- c(messages, "❌ No BigWig files could be matched to sample names")
            } else if (matched_bigwigs < nrow(sample_df)) {
              messages <- c(messages, paste("⚠️ Only", matched_bigwigs, "of", nrow(sample_df), "samples matched to BigWig files"))
            } else {
              messages <- c(messages, paste("✅", matched_bigwigs, "samples successfully matched to BigWig files"))
            }
          }
        }
      }, error = function(e) {
        messages <- c(messages, paste("❌ Error reading sample sheet:", e$message))
      })
    }
    
    values$validation_messages <- messages
    # Check if validation passed (no error messages)
    has_errors <- any(grepl("^❌", messages))
    values$params_valid <- !has_errors
    return(messages)
  }
  # Validation event
  observeEvent(input$validate_params, {
    messages <- validate_parameters()
    
    if (values$params_valid) {
      shinyjs::enable("generate_params")
    } else {
      shinyjs::disable("generate_params")
      shinyjs::disable("submit_job")
      values$params_generated <- FALSE
    }
  })
  
  # Display validation status
  output$validation_status <- renderUI({
    if (length(values$validation_messages) > 0) {
      message_class <- if (values$params_valid) "validation-success" else "validation-error"
      div(class = paste("validation-message", message_class),
          HTML(paste(values$validation_messages, collapse = "<br/>"))
      )
    }
  })
  
  # Generate params.txt
  generate_params_content <- function() {
    lines <- c()
    
    # Required parameters
    lines <- c(lines, paste("--seqID", shQuote(input$seqID)))
    lines <- c(lines, paste("--report_title", shQuote(input$report_title)))
    lines <- c(lines, paste("--organism", shQuote(input$organism)))
    lines <- c(lines, paste("--annotation_db", shQuote(input$annotation_db)))
    lines <- c(lines, paste("--hipergator-group", shQuote(input$hipergator_group)))
    lines <- c(lines, paste("--output-path", shQuote(input$output_path)))
    lines <- c(lines, paste("--user_email", shQuote(input$user_email)))
    lines <- c(lines, paste("--min_count_for_filtering", input$min_count_for_filtering))
    lines <- c(lines, paste("--min_prop_for_filtering", input$min_prop_for_filtering))
    lines <- c(lines, paste("--sample_sheet", shQuote(values$selected_files$sample_sheet)))
    
    # Report metadata parameters (only add if not empty)
    if (!is.null(input$PI) && input$PI != "") {
      lines <- c(lines, paste("--PI", shQuote(input$PI)))
    }
    if (!is.null(input$Institution) && input$Institution != "") {
      lines <- c(lines, paste("--Institution", shQuote(input$Institution)))
    }
    if (!is.null(input$Department) && input$Department != "") {
      lines <- c(lines, paste("--Department", shQuote(input$Department)))
    }
    if (!is.null(input$Study_Contact) && input$Study_Contact != "") {
      lines <- c(lines, paste("--Study_Contact", shQuote(input$Study_Contact)))
    }
    if (!is.null(input$Project_Title) && input$Project_Title != "") {
      lines <- c(lines, paste("--Project_Title", shQuote(input$Project_Title)))
    }
    if (!is.null(input$Study_Summary) && input$Study_Summary != "") {
      lines <- c(lines, paste("--Study_Summary", shQuote(input$Study_Summary)))
    }
    if (!is.null(input$Sample_Types) && input$Sample_Types != "") {
      lines <- c(lines, paste("--Sample_Types", shQuote(input$Sample_Types)))
    }
    if (!is.null(input$Analysis_Goals) && input$Analysis_Goals != "") {
      lines <- c(lines, paste("--Analysis_Goals", shQuote(input$Analysis_Goals)))
    }
    if (!is.null(input$Report_Prepared_By) && input$Report_Prepared_By != "") {
      lines <- c(lines, paste("--Report_Prepared_By", shQuote(input$Report_Prepared_By)))
    }
    if (!is.null(input$Report_Reviewed_By) && input$Report_Reviewed_By != "") {
      lines <- c(lines, paste("--Report_Reviewed_By", shQuote(input$Report_Reviewed_By)))
    }
    # Required parameters
    lines <- c(lines, paste("--sample_sheet", shQuote(values$selected_files$sample_sheet)))
    
    # Format peak files with sample matching
    if (!is.null(values$selected_files$peak_files)) {
      peak_pairs <- c()
      sample_df <- read.csv(values$selected_files$sample_sheet, stringsAsFactors = FALSE)
      
      for (peak_file in values$selected_files$peak_files) {
        peak_basename <- basename(peak_file)
        # Find matching sample
        for (sample in sample_df$sample) {
          if (grepl(sample, peak_basename, fixed = TRUE)) {
            peak_pairs <- c(peak_pairs, paste0(sample, ":", peak_file))
            break
          }
        }
      }
      lines <- c(lines, paste("--peak_files", shQuote(paste(peak_pairs, collapse = ","))))
    }
    
    # Format BigWig files with sample matching
    if (!is.null(values$selected_files$bigwig_files)) {
      bigwig_pairs <- c()
      sample_df <- read.csv(values$selected_files$sample_sheet, stringsAsFactors = FALSE)
      
      for (bigwig_file in values$selected_files$bigwig_files) {
        bigwig_basename <- basename(bigwig_file)
        # Find matching sample
        for (sample in sample_df$sample) {
          if (grepl(sample, bigwig_basename, fixed = TRUE)) {
            bigwig_pairs <- c(bigwig_pairs, paste0(sample, ":", bigwig_file))
            break
          }
        }
      }
      lines <- c(lines, paste("--bigwig_files", shQuote(paste(bigwig_pairs, collapse = ","))))
    }
    
    # Contrasts
    if (!is.null(values$selected_files$contrasts)) {
      lines <- c(lines, paste("--contrasts", shQuote(values$selected_files$contrasts)))
    } else if (!is.null(input$contrasts_text) && input$contrasts_text != "") {
      lines <- c(lines, paste("--contrasts", shQuote(input$contrasts_text)))
    }
    
    # Optional file parameters
    if (!is.null(values$selected_files$dds_file)) {
      lines <- c(lines, paste("--dds_file", shQuote(values$selected_files$dds_file)))
    }
    
    if (!is.null(values$selected_files$bam_files)) {
      bam_pairs <- c()
      sample_df <- read.csv(values$selected_files$sample_sheet, stringsAsFactors = FALSE)
      
      for (bam_file in values$selected_files$bam_files) {
        bam_basename <- basename(bam_file)
        for (sample in sample_df$sample) {
          if (grepl(sample, bam_basename, fixed = TRUE)) {
            bam_pairs <- c(bam_pairs, paste0(sample, ":", bam_file))
            break
          }
        }
      }
      lines <- c(lines, paste("--bam_files", shQuote(paste(bam_pairs, collapse = ","))))
    }
    
    if (!is.null(values$selected_files$peak_annotation)) {
      lines <- c(lines, paste("--peak_annotation", shQuote(values$selected_files$peak_annotation)))
    }
    
    if (!is.null(values$selected_files$qc_flagstat_dir)) {
      lines <- c(lines, paste("--qc_flagstat_dir", shQuote(values$selected_files$qc_flagstat_dir)))
    }
    
    if (!is.null(values$selected_files$qc_frip_file)) {
      lines <- c(lines, paste("--qc_frip_file", shQuote(values$selected_files$qc_frip_file)))
    }
    
    # Single value parameters
    lines <- c(lines, paste("--min_count_for_filtering", input$min_count_for_filtering))
    lines <- c(lines, paste("--min_prop_for_filtering", input$min_prop_for_filtering))
    lines <- c(lines, paste("--seqID", shQuote(input$seqID)))
    lines <- c(lines, paste("--hipergator-group", shQuote(input$hipergator_group)))
    lines <- c(lines, paste("--output-path", shQuote(input$output_path)))
    lines <- c(lines, paste("--report_title", shQuote(input$report_title)))
    lines <- c(lines, paste("--organism", shQuote(input$organism)))
    lines <- c(lines, paste("--annotation_db", shQuote(input$annotation_db)))
    
    if (!is.null(input$raw_seq_URL) && input$raw_seq_URL != "") {
      lines <- c(lines, paste("--raw_seq_URL", shQuote(input$raw_seq_URL)))
    }
    
    if (!is.null(input$multiqc_url) && input$multiqc_url != "") {
      lines <- c(lines, paste("--multiqc_url", shQuote(input$multiqc_url)))
    }
    
    return(lines)
  }
  
  # Generate params file
  # Generate params file - CORRECTED VERSION
  observeEvent(input$generate_params, {
    req(values$params_valid)
    tryCatch({
      # Ensure output directory exists
      if (!dir.exists(input$output_path)) {
        dir.create(input$output_path, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Generate params file directly to HiPerGator storage (NOT tempdir!)
      params_file <- file.path(input$output_path, paste0(input$seqID, "_params.txt"))
      params_content <- generate_params_content()
      writeLines(params_content, params_file)
      
      # Store the PERMANENT path, not tempdir
      values$params_file_path <- params_file
      values$params_generated <- TRUE
      
      shinyjs::enable("submit_job")
      shinyjs::enable("download_params")
      
      showNotification(paste("Parameters file saved to HiPerGator:", params_file), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error generating params file:", e$message), type = "error")
    })
  })
  # Display params preview
  output$params_preview <- renderText({
    if (values$params_generated && !is.null(values$params_file_path)) {
      paste("Generated params.txt:\n\n", paste(readLines(values$params_file_path), collapse = "\n"))
    } else {
      "Click 'Validate Parameters' then 'Generate params.txt' to see preview"
    }
  })
  
  # Add this download handler to your server:
  
  # Download params.txt file
  output$download_params <- downloadHandler(
    filename = function() {
      paste0(input$seqID, "_params.txt")
    },
    content = function(file) {
      if (values$params_generated && !is.null(values$params_file_path)) {
        file.copy(values$params_file_path, file)
      }
    }
  )
  
  # Update the existing button enable/disable logic to include the download button:
  observe({
    if (values$params_valid && values$params_generated) {
      shinyjs::enable("submit_job")
      shinyjs::enable("download_params")  # Enable download when params are generated
    } else {
      shinyjs::disable("submit_job")
      shinyjs::disable("download_params")  # Disable download when no params
    }
  })
  
  # Submit job - CORRECTED VERSION  
  # Submit job - SIMPLIFIED VERSION with command display
  observeEvent(input$submit_job, {
    req(values$params_valid)
    tryCatch({
      # Determine which params file to use
      if (!is.null(values$existing_params_file) && file.exists(values$existing_params_file)) {
        # Using loaded params file
        params <- parse_params_file(values$existing_params_file)
        seqID <- params$seqID %||% "unknown"
        output_path <- params[["output-path"]] %||% params[["output_path"]] %||% input$output_path
        report_title <- params$report_title %||% "ATAC-seq Analysis Report"
        
        # Copy to output directory with standard name
        final_params_path <- file.path(output_path, paste0(seqID, "_params.txt"))
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        file.copy(values$existing_params_file, final_params_path, overwrite = TRUE)
      } else {
        # Using generated params
        req(values$params_generated)
        seqID <- input$seqID
        output_path <- input$output_path
        report_title <- input$report_title
        final_params_path <- file.path(output_path, paste0(seqID, "_params.txt"))
        
        if (!file.exists(final_params_path)) {
          stop("Generated params file not found at expected location: ", final_params_path)
        }
      }
      
      # Get just the basename for the sbatch argument
      params_basename <- basename(final_params_path)
      
      # Build the command arguments
      sbatch_args <- c("render-report-with-params.sbatch",
                       "--params-file", final_params_path,  # Use full path instead of basename
                       "--title", shQuote(report_title))
      
      # Create the full command string for display
      full_command <- paste("sbatch", paste(sbatch_args, collapse = " "))
      
      # Use your existing sbatch script with arguments
      result <- system2("sbatch",
                        args = sbatch_args,
                        stdout = TRUE, stderr = TRUE)
      
      if (attr(result, "status") == 0 || is.null(attr(result, "status"))) {
        job_output <- paste(result, collapse = "\n")
        job_id_match <- regexpr("Submitted batch job ([0-9]+)", job_output)
        if (job_id_match > 0) {
          job_id <- regmatches(job_output, job_id_match)
          job_id <- gsub("Submitted batch job ", "", job_id)
          showNotification(paste("SLURM job submitted successfully! Job ID:", job_id,
                                 "\nCommand:", full_command,
                                 "\nParams file:", final_params_path), 
                           type = "message", duration = 15)
        } else {
          showNotification(paste("SLURM job submitted successfully!",
                                 "\nCommand:", full_command,
                                 "\nParams file:", final_params_path), 
                           type = "message")
        }
      } else {
        error_msg <- paste("SLURM submission failed:", paste(result, collapse = "\n"),
                           "\nCommand attempted:", full_command)
        showNotification(error_msg, type = "error", duration = 15)
      }
      
    }, error = function(e) {
      showNotification(paste("Error submitting SLURM job:", e$message), type = "error")
    })
  })
  # Add this observer to detect when user makes changes after loading:
  observe({
    # List of inputs to watch for changes
    input_list <- list(input$seqID, input$report_title, input$organism, input$annotation_db,
                       input$hipergator_group, input$output_path, input$user_email,
                       input$contrasts_text, input$raw_seq_URL, input$multiqc_url,
                       input$PI, input$Institution, input$Department, input$Study_Contact,
                       input$Project_Title, input$Study_Summary, input$Sample_Types,
                       input$Analysis_Goals, input$Report_Prepared_By, input$Report_Reviewed_By)
    
    if (values$params_loaded) {
      values$user_made_changes <- TRUE
      # Reset the existing_params_file reference so new generation is used
      values$existing_params_file <- NULL
    }
  })
  # Enable/disable buttons based on validation state
  observe({
    if (values$params_valid && values$params_generated) {
      shinyjs::enable("submit_job")
    } else {
      shinyjs::disable("submit_job")
    }
  })
}

shinyApp(ui = ui, server = server)
