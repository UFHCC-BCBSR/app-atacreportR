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
  .title-section { 
    text-align: center; 
    padding: 15px 0;  /* Reduced from 30px */
    background-color: #2c3e50; 
    color: white; 
  }
  .step-section { 
    background-color: white; 
    margin: 10px;     /* Reduced from 20px */
    padding: 15px;    /* Reduced from 30px */
    border-radius: 8px; 
    box-shadow: 0 2px 4px rgba(0,0,0,0.1); 
  }
  .step-section h2, .step-section h3 {
    margin-top: 0;
    margin-bottom: 15px;  /* Add this for consistent spacing */
  }
  .btn-primary { background-color: #3498db; border-color: #3498db; }
  .btn-success { background-color: #27ae60; border-color: #27ae60; }
  .btn-danger { background-color: #e74c3c; border-color: #e74c3c; }
  .login-box {
    background: #f8f9fa;
    border: 1px solid #dee2e6;
    border-radius: 8px;
    padding: 15px;    /* Reduced from 20px */
    margin-bottom: 15px;  /* Reduced from 20px */
  }
  .auth-status {
    padding: 8px;     /* Reduced from 10px */
    border-radius: 4px;
    margin-bottom: 10px;  /* Reduced from 15px */
  }
  .auth-success { background-color: #d4edda; border: 1px solid #c3e6cb; color: #155724; }
  .auth-needed { background-color: #fff3cd; border: 1px solid #ffeeba; color: #856404; }
  .selected-file {
    background-color: #f8f9fa;
    border: 1px solid #dee2e6;
    border-radius: 4px;
    padding: 6px 10px;  /* Reduced from 8px 12px */
    margin-top: 6px;    /* Reduced from 8px */
    font-family: monospace;
    font-size: 11px;
    color: #495057;
    max-height: 60px;
    overflow-y: auto;
  }
  .param-group {
    border: 1px solid #dee2e6;
    border-radius: 8px;
    padding: 10px;      /* Reduced from 15px */
    margin-bottom: 10px; /* Reduced from 20px */
    background-color: #f8f9fa;
  }
  .param-group h4 {
    margin-top: 0;
    margin-bottom: 8px;  /* Reduced from default */
    color: #495057;
    border-bottom: 1px solid #dee2e6;
    padding-bottom: 5px; /* Reduced from 8px */
  }
  .param-group h5 {
    margin-top: 0;
    margin-bottom: 8px;  /* Add this for h5 elements */
  }
  .param-group p {
    margin-bottom: 8px;  /* Reduce paragraph spacing */
  }
  .form-group {
    margin-bottom: 10px; /* Reduce form group spacing */
  }
  .validation-message {
    padding: 8px;       /* Reduced from 10px */
    border-radius: 4px;
    margin: 8px 0;      /* Reduced from 10px */
  }
  .validation-success { background-color: #d4edda; border: 1px solid #c3e6cb; color: #155724; }
  .validation-error { background-color: #f8d7da; border: 1px solid #f5c6cb; color: #721c24; }
  .validation-warning { background-color: #fff3cd; border: 1px solid #ffeeba; color: #856404; }
  .shinyDirectories .btn[title='Create new folder'] { display: none !important; }
  .shinyDirectories .fa-plus { display: none !important; }
  .shinyDirectories .fa-edit { display: none !important; }
  button#sF-btn-textChoice { display: none !important; }
  .glyphicon-pencil { display: none !important; }
  
  /* Additional compact spacing */
  .radio { margin-bottom: 5px; }  /* Reduce radio button spacing */
  .checkbox { margin-bottom: 5px; } /* Reduce checkbox spacing */
  .well { padding: 10px; }  /* Make wells more compact */
  br + br { display: none; }  /* Remove double line breaks */
"))
  ),
  
  div(class = "title-section",
      h1("Peak Reporter", style = "margin: 0; font-size: 48px; font-weight: 300;"),
      p("Configure, run, and report a differential analysis of peak data (CUT&RUN, ChIP-seq, ATAC-seq, and more)", style = "margin: 10px 0 0 0; font-size: 14px; opacity: 0.8;")
  ),
  
  # Load Existing Parameters section
  div(class = "step-section",
      h3("Load Existing Parameters (Optional)", style = "text-align: center; margin-bottom: 20px;"),
      p("Start from an existing params.txt file to make modifications:", style = "text-align: center; color: #6c757d;"),
      # Option 1: Browse for existing params file
      div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
          h5("Browse HiPerGator for Existing params.txt"),
          conditionalPanel(
            condition = "output.authenticated",
            textInput("custom_path_existing_params", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

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
  
  # Analysis Mode Selection with Info Buttons
  div(class = "step-section",
      h3("Analysis Mode", style = "text-align: center; margin-bottom: 20px;"),
      fluidRow(
        column(12,
               div(
                 radioButtons("analysis_mode", "Select your analysis workflow:",
                              choices = c(
                                "Prepare data only (generate DDS & annotations)" = "prepare_only",
                                "Prepare data + run differential analysis (full workflow)" = "prepare_and_analyze",
                                "Run differential analysis from existing data" = "analyze_only"
                              ),
                              selected = "prepare_and_analyze"
                 ),
                 # Info buttons positioned next to each option
                 div(style = "margin-top: -120px; margin-left: 400px;",  # Adjust positioning as needed
                     div(style = "margin-bottom: 8px;",
                         actionButton("info_prepare_only", "",
                                      icon = icon("info-circle"),
                                      class = "btn-info btn-xs",
                                      style = "padding: 2px 6px; font-size: 12px;",
                                      title = "Click for more information")
                     ),
                     div(style = "margin-bottom: 8px;",
                         actionButton("info_prepare_analyze", "",
                                      icon = icon("info-circle"),
                                      class = "btn-info btn-xs",
                                      style = "padding: 2px 6px; font-size: 12px;",
                                      title = "Click for more information")
                     ),
                     div(style = "margin-bottom: 8px;",
                         actionButton("info_analyze_only", "",
                                      icon = icon("info-circle"),
                                      class = "btn-info btn-xs",
                                      style = "padding: 2px 6px; font-size: 12px;",
                                      title = "Click for more information")
                     )
                 )
               )
        )
      )
  ),
  
  # Conditional file inputs based on analysis mode
  conditionalPanel(
    condition = "input.analysis_mode != 'analyze_only'",
    
    # Required Parameters for data preparation modes
    div(class = "step-section",
        h2("Required Parameters", style = "text-align: center; margin-bottom: 30px;"),
        
        # Basic Configuration (always required)
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
            textInput("user_email", "Email Address", placeholder = "your.email@ufl.edu (required for notification)")
        ),
        
        # Sample Sheet (same for all modes)
        div(class = "param-group",
            h4("Sample Sheet"),
            p("Choose how to provide your sample sheet CSV file:"),
            div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
                h5("Browse HiPerGator Files"),
                p("Select a file from HiPerGator. Use the download button if you want to edit it locally first."),
                conditionalPanel(
                  condition = "output.authenticated",
                  fluidRow(
                    column(6,
                           textInput("custom_path_sample_sheet", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

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
            div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
                h5("Upload File"),
                p("Upload a sample sheet from your computer (including files you've downloaded and edited)."),
                fileInput("upload_sample_sheet", "Choose CSV File", accept = ".csv")
            ),
            uiOutput("active_sample_sheet_status")
        ),
        
        # Peak Files (required for data preparation)
        div(class = "param-group",
            h4("Peak Files"),
            p("Select multiple peak files (.broadPeak format). Files will be matched to samples by basename:"),
            conditionalPanel(
              condition = "output.authenticated",
              textInput("custom_path_peak_files", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

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
        
        # BAM Files (required for data preparation)  
        div(class = "param-group",
            h4("BAM Files"),
            p("Select multiple BAM files for read counting. Files will be matched to samples by basename:"),
            conditionalPanel(
              condition = "output.authenticated",
              textInput("custom_path_bam_files", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

              shinyFilesButton("browse_bam_files", "Browse BAM Files", "Select multiple BAM files", class = "btn-info", multiple = TRUE),
              uiOutput("selected_bam_files")
            ),
            conditionalPanel(
              condition = "!output.authenticated",
              div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
                  tags$i(class = "fa fa-lock"), " Login above to browse HiPerGator files"
              )
            )
        ),
        
        # BigWig Files (hidden in prepare_only mode)
        conditionalPanel(
          condition = "input.analysis_mode != 'prepare_only'",
          div(class = "param-group",
              h4("BigWig Files"),
              p("Select multiple BigWig files. Files will be matched to samples by basename:"),
              conditionalPanel(
                condition = "output.authenticated",
                textInput("custom_path_bigwig_files", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),
                shinyFilesButton("browse_bigwig_files", "Browse BigWig Files", "Select multiple BigWig files", class = "btn-info", multiple = TRUE),
                uiOutput("selected_bigwig_files")
              ),
              conditionalPanel(
                condition = "!output.authenticated",
                div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
                    tags$i(class = "fa fa-lock"), " Login above to browse HiPerGator files"
                )
              )
          )
        ),
        
        # Contrasts (required only for prepare_and_analyze mode)
        conditionalPanel(
          condition = "input.analysis_mode == 'prepare_and_analyze'",
          div(class = "param-group",
              h4("Contrasts", tags$span("*", style = "color: red;")),
              p("Specify contrasts either by uploading a file OR entering text (required for differential analysis):"),
              conditionalPanel(
                condition = "output.authenticated",
                textInput("custom_path_contrasts", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

                shinyFilesButton("browse_contrasts", "Browse Contrasts File", "Select contrasts file", class = "btn-info", multiple = FALSE),
                uiOutput("selected_contrasts")
              ),
              textInput("contrasts_text", "OR Enter Contrasts (comma-separated)", placeholder = "group1_vs_group2,treatment_vs_control")
          )
        ),
        
        # Filtering Parameters (hidden in prepare_only mode)
        conditionalPanel(
          condition = "input.analysis_mode != 'prepare_only'",
          div(class = "param-group",
              h4("Filtering Parameters"),
              fluidRow(
                column(6, numericInput("min_count_for_filtering", "Min Count for Filtering", value = 10, min = 0)),
                column(6, numericInput("min_prop_for_filtering", "Min Proportion for Filtering", value = 0.5, min = 0, max = 1, step = 0.05))
              )
          )
        )
    )
  ),
  
  # Conditional inputs for "analyze_only" mode
  conditionalPanel(
    condition = "input.analysis_mode == 'analyze_only'",
    
    # Required Parameters for analysis-only mode
    div(class = "step-section",
        h2("Required Parameters", style = "text-align: center; margin-bottom: 30px;"),
        
        # Basic Configuration (duplicate with different IDs)
        div(class = "param-group",
            h4("Basic Configuration"),
            fluidRow(
              column(6, textInput("seqID_analyze", "Sequence ID (Project ID)", placeholder = "my-test")),
              column(6, textInput("report_title_analyze", "Report Title", value = "Differential Accessibility Analysis"))
            ),
            fluidRow(
              column(4, selectInput("organism_analyze", "Organism", choices = list("Mouse (mmu)" = "mmu", "Human (hsa)" = "hsa"), selected = "mmu")),
              column(4, selectInput("annotation_db_analyze", "Annotation Database", choices = list("org.Mm.eg.db" = "org.Mm.eg.db", "org.Hs.eg.db" = "org.Hs.eg.db"), selected = "org.Mm.eg.db")),
              column(4, textInput("hipergator_group_analyze", "HiPerGator Group", placeholder = "e.g., cancercenter-dept"))
            ),
            textInput("output_path_analyze", "Output Path", placeholder = "/blue/your-group/path/to/output"),
            textInput("user_email_analyze", "Email Address", placeholder = "your.email@ufl.edu (required for notification)")
        ),
        
        # Existing Data Files (required for analyze_only)
        div(class = "param-group",
            h4("Existing Data Files"),
            
            div(style = "border: 1px solid #28a745; padding: 15px; margin: 10px 0; border-radius: 5px; background-color: #f8fff9;",
                h5("DDS Object", tags$span("*", style = "color: red;")),
                p("Select your existing DDS (.RData) file from previous analysis:"),
                conditionalPanel(
                  condition = "output.authenticated",
                  textInput("custom_path_existing_dds", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

                  shinyFilesButton("browse_existing_dds", "Browse DDS File", "Select DDS RData file", class = "btn-success", multiple = FALSE),
                  uiOutput("selected_existing_dds")
                ),
                conditionalPanel(
                  condition = "!output.authenticated",
                  div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
                      tags$i(class = "fa fa-lock"), " Login below to browse HiPerGator files"
                  )
                )
            ),
            
            div(style = "border: 1px solid #17a2b8; padding: 15px; margin: 10px 0; border-radius: 5px; background-color: #f8fdff;",
                h5("Peak Annotation File", tags$span("(Optional)", style = "color: #6c757d; font-size: 0.8em;")),
                p("Select annotated consensus peaks file. If not provided, annotation will be generated automatically:"),
                conditionalPanel(
                  condition = "output.authenticated",
                  textInput("custom_path_existing_annotation", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

                  shinyFilesButton("browse_existing_annotation", "Browse Annotation File", "Select annotation file", class = "btn-info", multiple = FALSE),
                  uiOutput("selected_existing_annotation")
                )
            )
        ),
        
        # Sample Sheet (same inputs, different IDs to avoid conflicts)
        div(class = "param-group",
            h4("Sample Sheet"),
            p("Provide sample sheet for metadata (should match samples in your DDS object):"),
            div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
                h5("Browse HiPerGator Files"),
                conditionalPanel(
                  condition = "output.authenticated",
                  fluidRow(
                    column(6,
                           textInput("custom_path_sample_sheet_analyze", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

                           shinyFilesButton("browse_sample_sheet_analyze", "Browse HiPerGator", "Select CSV file", class = "btn-info", multiple = FALSE),
                           uiOutput("selected_sample_sheet_browse_analyze")
                    ),
                    column(6,
                           downloadButton("download_sample_sheet_analyze", "Download Selected File", class = "btn-secondary"),
                           tags$br(),
                           tags$small("Download to edit locally, then use Upload option below", style = "color: #6c757d;")
                    )
                  )
                )
            ),
            div(style = "border: 1px solid #dee2e6; padding: 15px; margin: 10px 0; border-radius: 5px;",
                h5("Upload File"),
                fileInput("upload_sample_sheet_analyze", "Choose CSV File", accept = ".csv")
            ),
            uiOutput("active_sample_sheet_status_analyze")
        ),
        
        # BigWig Files (required for all modes)
        div(class = "param-group",
            h4("BigWig Files"),
            p("Select multiple BigWig files. Files will be matched to samples by basename:"),
            conditionalPanel(
              condition = "output.authenticated",
              textInput("custom_path_bigwig_files_analyze", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

              shinyFilesButton("browse_bigwig_files_analyze", "Browse BigWig Files", "Select multiple BigWig files", class = "btn-info", multiple = TRUE),
              uiOutput("selected_bigwig_files_analyze")
            ),
            conditionalPanel(
              condition = "!output.authenticated",
              div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
                  tags$i(class = "fa fa-lock"), " Login above to browse HiPerGator files"
              )
            )
        ),
        
        # Contrasts (required for analysis)
        div(class = "param-group",
            h4("Contrasts", tags$span("*", style = "color: red;")),
            p("Specify contrasts for differential analysis:"),
            conditionalPanel(
              condition = "output.authenticated",
              textInput("custom_path_contrasts_analyze", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),

              shinyFilesButton("browse_contrasts_analyze", "Browse Contrasts File", "Select contrasts file", class = "btn-info", multiple = FALSE),
              uiOutput("selected_contrasts_analyze")
            ),
            textInput("contrasts_text_analyze", "OR Enter Contrasts (comma-separated)", placeholder = "group1_vs_group2,treatment_vs_control")
        ),
        
        # Filtering Parameters (required for all modes)
        div(class = "param-group",
            h4("Filtering Parameters"),
            fluidRow(
              column(6, numericInput("min_count_for_filtering_analyze", "Min Count for Filtering", value = 10, min = 0)),
              column(6, numericInput("min_prop_for_filtering_analyze", "Min Proportion for Filtering", value = 0.5, min = 0, max = 1, step = 0.05))
            )
        )
    )
  ),
  # Optional Parameters (hidden in prepare_only mode)
  conditionalPanel(
    condition = "input.analysis_mode != 'prepare_only'",
    div(class = "step-section",
        h2("Optional Parameters", style = "text-align: center; margin-bottom: 30px;"),
        # QC Files (optional for analysis modes)
        div(class = "param-group",
            h4("Quality Control Files"),
            conditionalPanel(
              condition = "output.authenticated",
              div(
                h5("QC Flagstat Directory:"),
                shinyDirButton("browse_qc_flagstat_dir", "Browse QC Flagstat Directory", "Select directory", class = "btn-info"),
                uiOutput("selected_qc_flagstat_dir"),
                br(),
                h5("QC FRIP File:"),
                textInput("custom_path_qc_frip_file", "Directory Path:", value = "", placeholder = "Enter path relative to volume..."),
                shinyFilesButton("browse_qc_frip_file", "Browse QC FRIP File", "Select FRIP file", class = "btn-info", multiple = FALSE),
                uiOutput("selected_qc_frip_file")
              )
            ),
            conditionalPanel(
              condition = "!output.authenticated",
              div(style = "padding: 10px; text-align: center; color: #856404; font-size: 12px;",
                  tags$i(class = "fa fa-lock"), " Login above to browse HiPerGator QC files (optional)"
              )
            )
        ),
        # URLs (Optional for analysis modes)
        div(class = "param-group",
            h4("URLs (Optional)"),
            textInput("raw_seq_URL", "Raw Sequencing Data URL", placeholder = "https://..."),
            textInput("multiqc_url", "MultiQC Results URL", placeholder = "https://...")
        )
    )
  ),
  
  # Report Metadata
  # Report Metadata - hide in prepare_only mode
  conditionalPanel(
    condition = "input.analysis_mode != 'prepare_only'",
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
    )
  ),
  
# Validation and Generation
div(class = "step-section",
    h2("Generate Parameters", style = "text-align: center; margin-bottom: 30px;"),
    div(style = "text-align: center;",
        actionButton("validate_params", "Validate Parameters", class = "btn-secondary btn-lg"),
        br(), br(),
        actionButton("generate_params", "Generate params.txt", class = "btn-success btn-lg", disabled = TRUE),
        br(), br(),
        downloadButton("download_params", "Download params.txt", class = "btn-info btn-lg", disabled = TRUE),
        br(), br(),
        # Dynamic button text based on mode
        # Replace the existing conditional panels with this:
        conditionalPanel(
          condition = "input.analysis_mode == 'prepare_only'",
          actionButton("submit_job_prepare", "Submit Data Preparation Job", class = "btn-primary btn-lg", disabled = TRUE)
        ),
        conditionalPanel(
          condition = "input.analysis_mode != 'prepare_only'",
          actionButton("submit_job_analyze", "Submit Full Analysis Job", class = "btn-primary btn-lg", disabled = TRUE)
        )
    ),
    br(),
    uiOutput("validation_status"),
    br(),
    verbatimTextOutput("params_preview"),
    br(),
    # Add permanent job status outputs
    uiOutput("job_submission_status"),
    uiOutput("expected_output_files")
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
  # Extract submission logic into a reusable function
  submit_analysis_job <- function() {
    req(values$params_valid)
    tryCatch({
      # Determine which params file to use
      if (!is.null(values$existing_params_file) && file.exists(values$existing_params_file)) {
        # Using loaded params file
        params <- parse_params_file(values$existing_params_file)
        seqID <- params$seqID %||% "unknown"
        output_path <- params[["output-path"]] %||% params[["output_path"]] %||%
          (if (input$analysis_mode == "analyze_only") input$output_path_analyze else input$output_path)
        report_title <- params$report_title %||% "ATAC-seq Analysis Report"
        # Create the target path
        final_params_path <- file.path(output_path, paste0(seqID, "_params.txt"))
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        # FIX: Only copy if source and destination are different
        source_path <- normalizePath(values$existing_params_file, mustWork = TRUE)
        target_path <- normalizePath(final_params_path, mustWork = FALSE)
        if (source_path != target_path) {
          cat("DEBUG: Copying params file from", source_path, "to", target_path, "\n")
          file.copy(values$existing_params_file, final_params_path, overwrite = TRUE)
        } else {
          cat("DEBUG: Source and destination are the same, skipping copy\n")
        }
      } else {
        # Using generated params - get the correct field values based on mode
        req(values$params_generated)
        if (input$analysis_mode == "analyze_only") {
          seqID <- input$seqID_analyze
          output_path <- input$output_path_analyze
          report_title <- input$report_title_analyze
        } else {
          seqID <- input$seqID
          output_path <- input$output_path
          report_title <- input$report_title
        }
        final_params_path <- file.path(output_path, paste0(seqID, "_params.txt"))
        if (!file.exists(final_params_path)) {
          stop("Generated params file not found at expected location: ", final_params_path)
        }
      }
      
      # Choose the appropriate sbatch script based on analysis mode
      if (input$analysis_mode == "prepare_only") {
        sbatch_script <- "prepare-data-only.sbatch"
        job_description <- "Data preparation job"
      } else {
        sbatch_script <- "render-report-with-params.sbatch"
        job_description <- "Full analysis job"
      }
      
      # Build the command arguments
      sbatch_args <- c(sbatch_script, "--params-file", final_params_path)
      # Add title for full analysis jobs
      if (input$analysis_mode != "prepare_only") {
        sbatch_args <- c(sbatch_args, "--title", shQuote(report_title))
      }
      
      # Create the full command string for display
      full_command <- paste("sbatch", paste(sbatch_args, collapse = " "))
      
      # Submit the job
      result <- system2("sbatch", args = sbatch_args, stdout = TRUE, stderr = TRUE)
      
      if (attr(result, "status") == 0 || is.null(attr(result, "status"))) {
        job_output <- paste(result, collapse = "\n")
        job_id_match <- regexpr("Submitted batch job ([0-9]+)", job_output)
        job_id <- if (job_id_match > 0) {
          gsub("Submitted batch job ", "", regmatches(job_output, job_id_match))
        } else {
          "Unknown"
        }
        
        # Show temporary notification
        showNotification(paste(job_description, "submitted successfully! Job ID:", job_id),
                         type = "message", duration = 10)
        
        # Update permanent UI elements
        output$job_submission_status <- renderUI({
          div(style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 15px;",
              tags$h4("✅ Job Submitted Successfully!", style = "color: #155724; margin-top: 0;"),
              tags$p(strong("Job ID: "), job_id),
              tags$p(strong("Command: "), tags$code(full_command)),
              tags$p(strong("Parameters file: "), tags$code(final_params_path)),
              if (input$analysis_mode == "prepare_only") {
                tags$p(strong("Check job status: "), tags$code(paste0("squeue -j ", job_id)))
              } else {
                tags$p(strong("Check job status: "), tags$code(paste0("squeue -j ", job_id)))
              }
          )
        })
        
        # Show expected output files
        output$expected_output_files <- renderUI({
          if (input$analysis_mode == "prepare_only") {
            # Data preparation outputs
            div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-top: 10px;",
                tags$h4("📁 Expected Output Files", style = "color: #004085; margin-top: 0;"),
                tags$p("When the job completes, these files will be created:"),
                tags$ul(
                  tags$li(tags$strong("DDS object: "), tags$code(file.path(output_path, paste0(seqID, ".dds.RData")))),
                  tags$li(tags$strong("Consensus peaks: "), tags$code(file.path(output_path, paste0(seqID, ".consensus-peaks.txt")))),
                  tags$li(tags$strong("Annotated peaks: "), tags$code(file.path(output_path, paste0(seqID, ".annotated.consensus-peaks.txt"))))
                ),
                tags$p(style = "margin-top: 15px; color: #666;",
                       "💡 These files can be used as input for differential analysis in 'Analysis from Existing Data' mode.")
            )
          } else {
            # Analysis outputs
            report_filename <- paste0(seqID, "_Report.html")
            report_path <- file.path(output_path, report_filename)
            div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-top: 10px;",
                tags$h4("📊 Expected Output Files", style = "color: #004085; margin-top: 0;"),
                tags$p("When the job completes, the main output will be:"),
                tags$ul(
                  tags$li(tags$strong("Analysis Report: "), tags$code(report_path))
                ),
                tags$p("Additional files (results tables, plots, etc.) will be created in the same directory."),
                tags$p(style = "margin-top: 15px; color: #666;",
                       "💡 The HTML report will contain all analysis results, plots, and downloadable data tables.")
            )
          }
        })
      } else {
        error_msg <- paste("SLURM submission failed:", paste(result, collapse = "\n"))
        # Show error in permanent UI
        output$job_submission_status <- renderUI({
          div(style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; margin-top: 15px;",
              tags$h4("❌ Job Submission Failed", style = "color: #721c24; margin-top: 0;"),
              tags$p(strong("Error: "), error_msg),
              tags$p(strong("Command attempted: "), tags$code(full_command))
          )
        })
        output$expected_output_files <- renderUI({
          div() # Empty div to clear any previous content
        })
        showNotification(error_msg, type = "error", duration = 15)
      }
    }, error = function(e) {
      error_msg <- paste("Error submitting SLURM job:", e$message)
      # Show error in permanent UI
      output$job_submission_status <- renderUI({
        div(style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; margin-top: 15px;",
            tags$h4("❌ Job Submission Error", style = "color: #721c24; margin-top: 0;"),
            tags$p(strong("Error: "), error_msg)
        )
      })
      output$expected_output_files <- renderUI({
        div() # Empty div to clear any previous content
      })
      showNotification(error_msg, type = "error")
    })
  }
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
        
        # Initialize all file browsers for BOTH modes
        # Original mode buttons:
        shinyFileChoose(input, "browse_sample_sheet", roots = group_volumes, session = session, filetypes = c("", "csv"))
        shinyFileChoose(input, "browse_contrasts_optional", roots = group_volumes, session = session, filetypes = c("", "txt", "csv"))
        shinyFileChoose(input, "browse_existing_params", roots = group_volumes, session = session, filetypes = c("", "txt"))
        shinyFileChoose(input, "browse_peak_files", roots = group_volumes, session = session, filetypes = c("", "broadPeak", "narrowPeak"))
        shinyFileChoose(input, "browse_bigwig_files", roots = group_volumes, session = session, filetypes = c("", "bigWig", "bw"))
        shinyFileChoose(input, "browse_contrasts", roots = group_volumes, session = session, filetypes = c("", "txt", "csv"))
        shinyFileChoose(input, "browse_dds_file", roots = group_volumes, session = session, filetypes = c("", "RData", "rda"))
        shinyFileChoose(input, "browse_bam_files", roots = group_volumes, session = session, filetypes = c("", "bam"))
        shinyFileChoose(input, "browse_peak_annotation", roots = group_volumes, session = session, filetypes = c("", "txt"))
        shinyDirChoose(input, "browse_qc_flagstat_dir", roots = group_volumes, session = session)
        shinyFileChoose(input, "browse_qc_frip_file", roots = group_volumes, session = session, filetypes = c("", "txt"))
        # Analyze-only mode buttons:
        shinyFileChoose(input, "browse_existing_dds", roots = group_volumes, session = session, filetypes = c("", "RData", "rda"))
        shinyFileChoose(input, "browse_existing_annotation", roots = group_volumes, session = session, filetypes = c("", "txt"))
        shinyFileChoose(input, "browse_sample_sheet_analyze", roots = group_volumes, session = session, filetypes = c("", "csv"))
        shinyFileChoose(input, "browse_bigwig_files_analyze", roots = group_volumes, session = session, filetypes = c("", "bigWig", "bw"))
        shinyFileChoose(input, "browse_contrasts_analyze", roots = group_volumes, session = session, filetypes = c("", "txt", "csv"))
        
        setup_file_browser("browse_contrasts_optional", "contrasts_optional")
        # Custom path observers for all file browsers
        observe({
          if (!is.null(input$custom_path_sample_sheet) && input$custom_path_sample_sheet != "") {
            shinyFileChoose(input, "browse_sample_sheet", roots = group_volumes, session = session, filetypes = c("", "csv"), defaultPath = input$custom_path_sample_sheet)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_existing_params) && input$custom_path_existing_params != "") {
            shinyFileChoose(input, "browse_existing_params", roots = group_volumes, session = session, filetypes = c("", "txt"), defaultPath = input$custom_path_existing_params)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_peak_files) && input$custom_path_peak_files != "") {
            shinyFileChoose(input, "browse_peak_files", roots = group_volumes, session = session, filetypes = c("", "broadPeak", "narrowPeak"), defaultPath = input$custom_path_peak_files)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_bigwig_files) && input$custom_path_bigwig_files != "") {
            shinyFileChoose(input, "browse_bigwig_files", roots = group_volumes, session = session, filetypes = c("", "bigWig", "bw"), defaultPath = input$custom_path_bigwig_files)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_contrasts) && input$custom_path_contrasts != "") {
            shinyFileChoose(input, "browse_contrasts", roots = group_volumes, session = session, filetypes = c("", "txt", "csv"), defaultPath = input$custom_path_contrasts)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_dds_file) && input$custom_path_dds_file != "") {
            shinyFileChoose(input, "browse_dds_file", roots = group_volumes, session = session, filetypes = c("", "RData", "rda"), defaultPath = input$custom_path_dds_file)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_bam_files) && input$custom_path_bam_files != "") {
            shinyFileChoose(input, "browse_bam_files", roots = group_volumes, session = session, filetypes = c("", "bam"), defaultPath = input$custom_path_bam_files)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_peak_annotation) && input$custom_path_peak_annotation != "") {
            shinyFileChoose(input, "browse_peak_annotation", roots = group_volumes, session = session, filetypes = c("", "txt"), defaultPath = input$custom_path_peak_annotation)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_qc_flagstat_dir) && input$custom_path_qc_flagstat_dir != "") {
            shinyDirChoose(input, "browse_qc_flagstat_dir", roots = group_volumes, session = session, defaultPath = input$custom_path_qc_flagstat_dir)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_qc_frip_file) && input$custom_path_qc_frip_file != "") {
            shinyFileChoose(input, "browse_qc_frip_file", roots = group_volumes, session = session, filetypes = c("", "txt"), defaultPath = input$custom_path_qc_frip_file)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_existing_dds) && input$custom_path_existing_dds != "") {
            shinyFileChoose(input, "browse_existing_dds", roots = group_volumes, session = session, filetypes = c("", "RData", "rda"), defaultPath = input$custom_path_existing_dds)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_existing_annotation) && input$custom_path_existing_annotation != "") {
            shinyFileChoose(input, "browse_existing_annotation", roots = group_volumes, session = session, filetypes = c("", "txt"), defaultPath = input$custom_path_existing_annotation)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_sample_sheet_analyze) && input$custom_path_sample_sheet_analyze != "") {
            shinyFileChoose(input, "browse_sample_sheet_analyze", roots = group_volumes, session = session, filetypes = c("", "csv"), defaultPath = input$custom_path_sample_sheet_analyze)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_bigwig_files_analyze) && input$custom_path_bigwig_files_analyze != "") {
            shinyFileChoose(input, "browse_bigwig_files_analyze", roots = group_volumes, session = session, filetypes = c("", "bigWig", "bw"), defaultPath = input$custom_path_bigwig_files_analyze)
          }
        })
        
        observe({
          if (!is.null(input$custom_path_contrasts_analyze) && input$custom_path_contrasts_analyze != "") {
            shinyFileChoose(input, "browse_contrasts_analyze", roots = group_volumes, session = session, filetypes = c("", "txt", "csv"), defaultPath = input$custom_path_contrasts_analyze)
          }
        })
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
  setup_file_browser("browse_existing_dds", "existing_dds")
  setup_file_browser("browse_existing_annotation", "existing_annotation") 
  setup_file_browser("browse_sample_sheet_analyze", "sample_sheet_analyze")
  setup_file_browser("browse_bigwig_files_analyze", "bigwig_files_analyze", multiple = TRUE)
  setup_file_browser("browse_contrasts_analyze", "contrasts_analyze")
  
  # File display outputs
  # Sample sheet handling - add this to your server function
  
  # Replace the sample sheet server logic with this:
  
  # Track which option is active
  values$sample_sheet_source <- NULL
  values$sample_sheet_path <- NULL
  
  # Option 1: HiPerGator browse
  setup_file_browser("browse_sample_sheet", "sample_sheet_browse")
  
  # Updated browse observer to clear upload when browse is used
  observeEvent(values$selected_files$sample_sheet_browse, {
    if (!is.null(values$selected_files$sample_sheet_browse)) {
      # Clear any previous upload selection
      # (Note: We can't easily clear the fileInput widget, but we can clear our tracking)
      
      values$sample_sheet_source <- "hipergator"
      values$sample_sheet_path <- values$selected_files$sample_sheet_browse
      values$selected_files$sample_sheet <- values$selected_files$sample_sheet_browse
      showNotification("HiPerGator file selected (upload cleared)", type = "message")
    }
  })
  
  # Server-side observers for info buttons
  
  # Info for "Prepare data only" mode
  observeEvent(input$info_prepare_only, {
    showModal(modalDialog(
      title = tags$div(
        icon("database"), 
        "Prepare Data Only Mode",
        style = "color: #0066cc;"
      ),
      
      tags$div(
        tags$h5("When to use this mode:", style = "color: #333; margin-top: 0;"),
        tags$ul(
          tags$li("You have BED files (peak calls) and BAM files for your samples"),
          tags$li("You have run a peak caller but don't have consensus peaks yet"),
          tags$li("You need consensus peak merging and quantification (featureCounts)"),
          tags$li("You want to combine peaks from multiple datasets or analyses"),
          tags$li("You ran peak calling as stand-alone (not part of an integrated pipeline like nf-core/atacseq)")
        ),
        
        tags$h5("What this mode does:", style = "color: #333; margin-top: 15px;"),
        tags$ul(
          tags$li("Creates consensus peaks across all your samples"),
          tags$li("Counts reads in consensus peaks using featureCounts"), 
          tags$li("Generates peak annotations (or uses existing if provided)"),
          tags$li("Creates a DDS (DESeq2) object ready for differential analysis"),
          tags$li(tags$strong("Saves intermediate files for future use"))
        ),
        
        tags$h5("Output files you'll get:", style = "color: #333; margin-top: 15px;"),
        tags$ul(
          tags$li(tags$code("dds.RData"), " - DESeq2 object with count matrix"),
          tags$li(tags$code("consensus-peaks.txt"), " - Merged peak coordinates"),
          tags$li(tags$code("annotated.consensus-peaks.txt"), " - Peaks with gene annotations")
        ),
        
        tags$div(
          tags$em("Note: Peak annotation will be generated automatically if not provided."),
          style = "color: #666; font-size: 0.9em; margin-top: 10px;"
        )
      ),
      
      footer = modalButton("Close"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  # Info for "Prepare + analyze" mode  
  observeEvent(input$info_prepare_analyze, {
    showModal(modalDialog(
      title = tags$div(
        icon("cogs"), 
        "Full Workflow Mode", 
        style = "color: #28a745;"
      ),
      
      tags$div(
        tags$h5("When to use this mode:", style = "color: #333; margin-top: 0;"),
        tags$ul(
          tags$li("Same as 'Prepare Data Only' but you're ready to run analysis immediately"),
          tags$li("You want both data preparation AND differential analysis in one go"),
          tags$li("You want to generate a complete analysis report")
        ),
        
        tags$h5("What this mode does:", style = "color: #333; margin-top: 15px;"),
        tags$div(
          tags$strong("Everything from 'Prepare Data Only' mode, PLUS:"),
          tags$ul(
            tags$li("Runs differential accessibility analysis"),
            tags$li("Generates comprehensive HTML report with plots and tables"),
            tags$li("Creates visualization files and track hub data"),
            tags$li("Performs pathway enrichment analysis (GO/KEGG)")
          )
        ),
        
        tags$h5("Output files you'll get:", style = "color: #333; margin-top: 15px;"),
        tags$ul(
          tags$li(tags$strong("All intermediate files"), " (same as 'Prepare Data Only')"),
          tags$li(tags$code("analysis_report.html"), " - Complete analysis report"),
          tags$li("Differential analysis results tables"),
          tags$li("Track hub files for genome browser viewing")
        ),
        
        tags$div(
          tags$strong("Advantage: "), 
          "You get intermediate files saved so you can run additional analyses later with different parameters, contrasts, or sample subsets.",
          style = "color: #155724; background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;"
        )
      ),
      
      footer = modalButton("Close"),
      size = "l", 
      easyClose = TRUE
    ))
  })
  
  # Info for "Analyze only" mode
  observeEvent(input$info_analyze_only, {
    showModal(modalDialog(
      title = tags$div(
        icon("chart-line"), 
        "Analysis from Existing Data Mode",
        style = "color: #dc3545;"
      ),
      
      tags$div(
        tags$h5("When to use this mode:", style = "color: #333; margin-top: 0;"),
        tags$ul(
          tags$li("You already ran 'Prepare Data Only' or 'Full Workflow' mode previously"),
          tags$li("You have output from nf-core/atacseq or another integrated pipeline"), 
          tags$li("You have a DDS object and annotated consensus peaks from another source"),
          tags$li("You want to re-run analysis with different parameters or contrasts"),
          tags$li("You want to analyze a subset of samples from a previous analysis")
        ),
        
        tags$h5("What you need to provide:", style = "color: #333; margin-top: 15px;"),
        tags$ul(
          tags$li(tags$code("dds.RData"), " - DESeq2 object with count matrix and sample metadata"),
          tags$li(tags$code("annotated.consensus-peaks.txt"), " - Peak annotations (optional - will generate if missing)")
        ),
        
        tags$h5("What this mode does:", style = "color: #333; margin-top: 15px;"),
        tags$ul(
          tags$li("Loads your existing DDS object and peak annotations"),
          tags$li("Runs differential accessibility analysis"), 
          tags$li("Generates comprehensive HTML report"),
          tags$li("Creates all visualization and output files")
        ),
        
        tags$div(
          tags$strong("Typical workflow: "), 
          "Run 'Prepare Data Only' once, then use 'Analysis from Existing Data' for different parameter combinations or sample subsets.",
          style = "color: #004085; background-color: #cce5ff; padding: 10px; border-radius: 5px; margin-top: 10px;"
        ),
        
        tags$div(
          tags$em("Note: If peak annotation file is missing or not provided, annotation will be generated automatically."),
          style = "color: #666; font-size: 0.9em; margin-top: 10px;"
        )
      ),
      
      footer = modalButton("Close"),
      size = "l",
      easyClose = TRUE
    ))
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
  
  # Add these output handlers for analyze-only mode
  output$selected_existing_dds <- renderUI({
    if (!is.null(values$selected_files$existing_dds)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$existing_dds),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$existing_dds)
      )
    }
  })
  
  output$selected_existing_annotation <- renderUI({
    if (!is.null(values$selected_files$existing_annotation)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$existing_annotation),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$existing_annotation)
      )
    }
  })
  
  output$selected_sample_sheet_browse_analyze <- renderUI({
    if (!is.null(values$selected_files$sample_sheet_analyze)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$sample_sheet_analyze),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$sample_sheet_analyze)
      )
    }
  })
  
  output$selected_bigwig_files_analyze <- renderUI({
    if (!is.null(values$selected_files$bigwig_files_analyze)) {
      files <- values$selected_files$bigwig_files_analyze
      div(class = "selected-file",
          strong(paste("Selected", length(files), "BigWig files:")),
          tags$br(),
          paste(basename(files), collapse = ", ")
      )
    }
  })
  
  output$selected_contrasts_analyze <- renderUI({
    if (!is.null(values$selected_files$contrasts_analyze)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$contrasts_analyze),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$contrasts_analyze)
      )
    }
  })
  output$selected_contrasts_optional <- renderUI({
    if (!is.null(values$selected_files$contrasts_optional)) {
      div(class = "selected-file",
          strong("Selected: "), basename(values$selected_files$contrasts_optional),
          tags$br(),
          tags$span(style = "font-size: 10px;", values$selected_files$contrasts_optional)
      )
    }
  })
  
  # Option 2: Upload
  # Updated upload handler for sample sheet (regular mode)
  observeEvent(input$upload_sample_sheet, {
    if (!is.null(input$upload_sample_sheet)) {
      # Get the user-defined output path
      output_path <- input$output_path
      
      if (is.null(output_path) || output_path == "") {
        showNotification("Please set an output path before uploading files", type = "error")
        return()
      }
      
      # Create output directory if it doesn't exist
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Copy to output path with original filename
      output_file_path <- file.path(output_path, input$upload_sample_sheet$name)
      file.copy(input$upload_sample_sheet$datapath, output_file_path, overwrite = TRUE)
      
      # Clear browse selection and set upload as active
      values$selected_files$sample_sheet_browse <- NULL
      values$sample_sheet_source <- "upload"
      values$sample_sheet_path <- output_file_path
      values$selected_files$sample_sheet <- output_file_path
      
      showNotification(paste("File uploaded to output directory:", output_file_path), type = "message")
    }
  })
  
  # Updated upload handler for sample sheet (analyze mode)
  observeEvent(input$upload_sample_sheet_analyze, {
    if (!is.null(input$upload_sample_sheet_analyze)) {
      # Get the user-defined output path for analyze mode
      output_path <- input$output_path_analyze
      
      if (is.null(output_path) || output_path == "") {
        showNotification("Please set an output path before uploading files", type = "error")
        return()
      }
      
      # Create output directory if it doesn't exist
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Copy to output path with original filename
      output_file_path <- file.path(output_path, input$upload_sample_sheet_analyze$name)
      file.copy(input$upload_sample_sheet_analyze$datapath, output_file_path, overwrite = TRUE)
      
      # Clear browse selection and set upload as active
      values$selected_files$sample_sheet_analyze <- output_file_path
      values$selected_files$sample_sheet <- output_file_path  # Also update main key
      
      showNotification(paste("File uploaded to output directory:", output_file_path), type = "message")
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
  # Updated load existing parameters function with cross-mode compatibility
  observeEvent(input$load_existing_params, {
    req(values$existing_params_file)
    
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
      
      # Parse and store file lists function (moved up so it's available)
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
      
      # ===== BASIC CONFIGURATION - Populate BOTH modes =====
      if ("seqID" %in% names(params)) {
        cat("DEBUG: Updating seqID to:", params$seqID, "\n")
        updateTextInput(session, "seqID", value = params$seqID)
        updateTextInput(session, "seqID_analyze", value = params$seqID)
      }
      
      if ("report_title" %in% names(params)) {
        cat("DEBUG: Updating report_title to:", params$report_title, "\n")
        updateTextInput(session, "report_title", value = params$report_title)
        updateTextInput(session, "report_title_analyze", value = params$report_title)
      }
      
      if ("organism" %in% names(params)) {
        updateSelectInput(session, "organism", selected = params$organism)
        updateSelectInput(session, "organism_analyze", selected = params$organism)
      }
      
      if ("annotation_db" %in% names(params)) {
        updateSelectInput(session, "annotation_db", selected = params$annotation_db)
        updateSelectInput(session, "annotation_db_analyze", selected = params$annotation_db)
      }
      
      hipergator_group_key <- if ("hipergator-group" %in% names(params)) "hipergator-group" else "hipergator_group"
      if (hipergator_group_key %in% names(params)) {
        updateTextInput(session, "hipergator_group", value = params[[hipergator_group_key]])
        updateTextInput(session, "hipergator_group_analyze", value = params[[hipergator_group_key]])
      }
      
      output_path_key <- if ("output-path" %in% names(params)) "output-path" else "output_path"
      if (output_path_key %in% names(params)) {
        updateTextInput(session, "output_path", value = params[[output_path_key]])
        updateTextInput(session, "output_path_analyze", value = params[[output_path_key]])
      }
      
      if ("user_email" %in% names(params)) {
        updateTextInput(session, "user_email", value = params$user_email)
        updateTextInput(session, "user_email_analyze", value = params$user_email)
      }
      
      # ===== NUMERIC INPUTS - Populate BOTH modes =====
      if ("min_count_for_filtering" %in% names(params)) {
        updateNumericInput(session, "min_count_for_filtering", value = as.numeric(params$min_count_for_filtering))
        updateNumericInput(session, "min_count_for_filtering_analyze", value = as.numeric(params$min_count_for_filtering))
      }
      
      if ("min_prop_for_filtering" %in% names(params)) {
        updateNumericInput(session, "min_prop_for_filtering", value = as.numeric(params$min_prop_for_filtering))
        updateNumericInput(session, "min_prop_for_filtering_analyze", value = as.numeric(params$min_prop_for_filtering))
      }
      
      # ===== OPTIONAL URLs - Same for both modes =====
      if ("raw_seq_URL" %in% names(params)) {
        updateTextInput(session, "raw_seq_URL", value = params$raw_seq_URL)
      }
      if ("multiqc_url" %in% names(params)) {
        updateTextInput(session, "multiqc_url", value = params$multiqc_url)
      }
      
      # ===== REPORT METADATA - Same for both modes =====
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
      
      # ===== CONTRASTS - Handle BOTH modes =====
      if ("contrasts" %in% names(params)) {
        if (!file.exists(params$contrasts)) {
          # It's a text value, not a file path
          updateTextInput(session, "contrasts_text", value = params$contrasts)
          updateTextInput(session, "contrasts_text_analyze", value = params$contrasts)
        } else {
          # It's a file path
          values$selected_files$contrasts <- params$contrasts
          values$selected_files$contrasts_analyze <- params$contrasts
        }
      }
      
      # ===== SAMPLE SHEET - Handle BOTH modes =====
      if ("sample_sheet" %in% names(params)) {
        values$selected_files$sample_sheet <- params$sample_sheet
        values$selected_files$sample_sheet_analyze <- params$sample_sheet
        values$sample_sheet_source <- "loaded_from_params"
        values$sample_sheet_path <- params$sample_sheet
      }
      
      # ===== FILE LISTS - Handle BOTH modes =====
      if ("peak_files" %in% names(params)) {
        peak_files_parsed <- parse_file_pairs(params$peak_files)
        values$selected_files$peak_files <- peak_files_parsed
      }
      
      if ("bigwig_files" %in% names(params)) {
        bigwig_files_parsed <- parse_file_pairs(params$bigwig_files)
        values$selected_files$bigwig_files <- bigwig_files_parsed
        values$selected_files$bigwig_files_analyze <- bigwig_files_parsed  # For analyze mode
      }
      
      if ("bam_files" %in% names(params)) {
        values$selected_files$bam_files <- parse_file_pairs(params$bam_files)
      }
      
      # ===== ANALYZE-ONLY SPECIFIC FILES =====
      if ("dds_file" %in% names(params)) {
        values$selected_files$existing_dds <- params$dds_file  # Use analyze-only key
      }
      
      if ("peak_annotation" %in% names(params)) {
        values$selected_files$existing_annotation <- params$peak_annotation  # Use analyze-only key
      }
      
      # ===== OTHER FILES - Original mode only =====
      if ("qc_flagstat_dir" %in% names(params)) {
        values$selected_files$qc_flagstat_dir <- params$qc_flagstat_dir
      }
      if ("qc_frip_file" %in% names(params)) {
        values$selected_files$qc_frip_file <- params$qc_frip_file
      }
      
      # Mark that parameters were loaded
      values$params_loaded <- TRUE
      
      cat("DEBUG: All updates completed successfully\n")
      showNotification("Parameters loaded successfully from file! Fields populated for all compatible modes.", type = "message")
      
    }, error = function(e) {
      cat("DEBUG: Error in load_existing_params:", e$message, "\n")
      showNotification(paste("Error loading parameters:", e$message), type = "error")
    })
  })
  
  # Show active sample sheet status (fixes the "0.csv" issue)
  # Updated active sample sheet status display
  output$active_sample_sheet_status <- renderUI({
    if (!is.null(values$sample_sheet_source) && !is.null(values$sample_sheet_path)) {
      # Get the correct filename based on source
      filename <- if (values$sample_sheet_source == "hipergator") {
        basename(values$sample_sheet_path)
      } else if (values$sample_sheet_source == "upload") {
        # For uploads, show the original filename
        if (!is.null(input$upload_sample_sheet)) {
          input$upload_sample_sheet$name
        } else {
          basename(values$sample_sheet_path)
        }
      } else {
        basename(values$sample_sheet_path)
      }
      
      source_text <- switch(values$sample_sheet_source,
                            "hipergator" = "Using HiPerGator file",
                            "upload" = "Using uploaded file (saved to output directory)",
                            "loaded_from_params" = "Loaded from params.txt"
      )
      
      div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 15px;",
          tags$i(class = "fa fa-check-circle", style = "color: #155724;"),
          strong(" Active: "), source_text,
          br(),
          strong("File: "), filename,
          if (values$sample_sheet_source == "upload") {
            tags$div(
              tags$small(paste("Saved to:", values$sample_sheet_path), style = "color: #6c757d;")
            )
          }
      )
    }
  })
  
  # Updated analyze mode status
  output$active_sample_sheet_status_analyze <- renderUI({
    if (!is.null(values$selected_files$sample_sheet_analyze)) {
      # Determine if this was uploaded or browsed
      source_text <- if (!is.null(input$upload_sample_sheet_analyze)) {
        "Using uploaded file (saved to output directory)"
      } else {
        "Using HiPerGator file"
      }
      
      filename <- basename(values$selected_files$sample_sheet_analyze)
      
      div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 15px;",
          tags$i(class = "fa fa-check-circle", style = "color: #155724;"),
          strong(" Active: "), source_text,
          br(),
          strong("File: "), filename,
          tags$div(
            tags$small(paste("Path:", values$selected_files$sample_sheet_analyze), style = "color: #6c757d;")
          )
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
  validate_parameters <- function() {
    messages <- c()
    # Determine which input fields to check based on analysis mode
    if (input$analysis_mode == "analyze_only") {
      # Use analyze-only field names
      seqID_field <- input$seqID_analyze
      hipergator_group_field <- input$hipergator_group_analyze
      output_path_field <- input$output_path_analyze
      user_email_field <- input$user_email_analyze
      min_count_field <- input$min_count_for_filtering_analyze
      min_prop_field <- input$min_prop_for_filtering_analyze
      contrasts_text_field <- input$contrasts_text_analyze
      # Check for analyze-only specific requirements
      if (is.null(values$selected_files$existing_dds)) {
        messages <- c(messages, "❌ DDS file is required for analyze-only mode")
      }
      # Check file keys for analyze-only mode
      sample_sheet_key <- "sample_sheet_analyze"
      bigwig_files_key <- "bigwig_files_analyze"
      contrasts_key <- "contrasts_analyze"
    } else if (input$analysis_mode == "prepare_only") {
      # Use original field names for prepare_only
      seqID_field <- input$seqID
      hipergator_group_field <- input$hipergator_group
      output_path_field <- input$output_path
      user_email_field <- input$user_email
      min_count_field <- NULL  # Not needed for prepare_only
      min_prop_field <- NULL   # Not needed for prepare_only
      contrasts_text_field <- NULL  # Not needed for prepare_only
      
      # Check for data preparation requirements
      if (is.null(values$selected_files$peak_files)) {
        messages <- c(messages, "❌ Peak files are required")
      }
      if (is.null(values$selected_files$bam_files)) {
        messages <- c(messages, "❌ BAM files are required for data preparation")
      }
      
      # Use original file keys but don't require bigwig or contrasts for prepare_only
      sample_sheet_key <- "sample_sheet"
      bigwig_files_key <- NULL  # Don't check bigwig for prepare_only
      contrasts_key <- NULL     # Don't check contrasts for prepare_only
    } else {
      # Use original field names for prepare_and_analyze
      seqID_field <- input$seqID
      hipergator_group_field <- input$hipergator_group
      output_path_field <- input$output_path
      user_email_field <- input$user_email
      min_count_field <- input$min_count_for_filtering
      min_prop_field <- input$min_prop_for_filtering
      contrasts_text_field <- input$contrasts_text
      
      # Check for data preparation requirements
      if (is.null(values$selected_files$peak_files)) {
        messages <- c(messages, "❌ Peak files are required")
      }
      # Check that either DDS file OR BAM files are provided for non-analyze modes
      has_dds <- !is.null(values$selected_files$dds_file)
      has_bam <- !is.null(values$selected_files$bam_files)
      if (!has_dds && !has_bam) {
        messages <- c(messages, "❌ Either DDS file OR BAM files must be provided")
      }
      # Use original file keys
      sample_sheet_key <- "sample_sheet"
      bigwig_files_key <- "bigwig_files"
      contrasts_key <- "contrasts"
    }
    
    # Check required fields (common logic)
    if (is.null(seqID_field) || seqID_field == "") {
      messages <- c(messages, "❌ Sequence ID is required")
    }
    if (is.null(hipergator_group_field) || hipergator_group_field == "") {
      messages <- c(messages, "❌ HiPerGator Group is required")
    }
    if (is.null(output_path_field) || output_path_field == "") {
      messages <- c(messages, "❌ Output path is required")
    }
    if (is.null(user_email_field) || user_email_field == "") {
      messages <- c(messages, "❌ User email is required")
    }
    
    # Check sample sheet
    if (is.null(values$selected_files[[sample_sheet_key]])) {
      messages <- c(messages, "❌ Sample sheet is required")
    }
    
    # Check BigWig files (only for analysis modes)
    if (!is.null(bigwig_files_key) && is.null(values$selected_files[[bigwig_files_key]])) {
      messages <- c(messages, "❌ BigWig files are required")
    }
    
    # Validate contrasts (skip for prepare_only mode)
    if (!is.null(contrasts_key)) {
      has_contrasts_file <- !is.null(values$selected_files[[contrasts_key]])
      has_contrasts_text <- !is.null(contrasts_text_field) && contrasts_text_field != ""
      if (input$analysis_mode == "prepare_and_analyze") {
        # Contrasts are required for full workflow
        if (!has_contrasts_file && !has_contrasts_text) {
          messages <- c(messages, "❌ Contrasts are required for differential analysis")
        }
      } else if (input$analysis_mode == "analyze_only") {
        # Contrasts are required for analyze_only
        if (!has_contrasts_file && !has_contrasts_text) {
          messages <- c(messages, "❌ Contrasts are required for differential analysis")
        }
      }
    }
    
    # Validate file matching if sample sheet is available
    if (!is.null(values$selected_files[[sample_sheet_key]])) {
      tryCatch({
        sample_df <- read.csv(values$selected_files[[sample_sheet_key]], stringsAsFactors = FALSE)
        if (!("sample" %in% colnames(sample_df))) {
          messages <- c(messages, "❌ Sample sheet must have a 'sample' column")
        } else {
          # Check BigWig files matching
          if (!is.null(bigwig_files_key) && !is.null(values$selected_files[[bigwig_files_key]])) {
            bigwig_basenames <- basename(values$selected_files[[bigwig_files_key]])
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
          # Check peak files matching (only for non-analyze modes)
          if (input$analysis_mode != "analyze_only" && !is.null(values$selected_files$peak_files)) {
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
          # Check BAM files matching (only for prepare modes)
          if (input$analysis_mode != "analyze_only" && !is.null(values$selected_files$bam_files)) {
            bam_basenames <- basename(values$selected_files$bam_files)
            matched_bams <- 0
            for (sample in sample_df$sample) {
              if (any(grepl(sample, bam_basenames, fixed = TRUE))) {
                matched_bams <- matched_bams + 1
              }
            }
            if (matched_bams == 0) {
              messages <- c(messages, "❌ No BAM files could be matched to sample names")
            } else if (matched_bams < nrow(sample_df)) {
              messages <- c(messages, paste("⚠️ Only", matched_bams, "of", nrow(sample_df), "samples matched to BAM files"))
            } else {
              messages <- c(messages, paste("✅", matched_bams, "samples successfully matched to BAM files"))
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
  # Updated generate params function that handles different modes
  generate_params_content <- function() {
    lines <- c()
    # Determine which input fields to use based on analysis mode
    if (input$analysis_mode == "analyze_only") {
      # Use analyze-only field names
      seqID_field <- input$seqID_analyze
      report_title_field <- input$report_title_analyze
      organism_field <- input$organism_analyze
      annotation_db_field <- input$annotation_db_analyze
      hipergator_group_field <- input$hipergator_group_analyze
      output_path_field <- input$output_path_analyze
      user_email_field <- input$user_email_analyze
      min_count_field <- input$min_count_for_filtering_analyze
      min_prop_field <- input$min_prop_for_filtering_analyze
      contrasts_text_field <- input$contrasts_text_analyze
      # Use analyze-only file keys
      sample_sheet_key <- "sample_sheet_analyze"
      bigwig_files_key <- "bigwig_files_analyze"
      contrasts_key <- "contrasts_analyze"
    } else if (input$analysis_mode == "prepare_only") {
      # Use original field names for prepare_only
      seqID_field <- input$seqID
      report_title_field <- input$report_title
      organism_field <- input$organism
      annotation_db_field <- input$annotation_db
      hipergator_group_field <- input$hipergator_group
      output_path_field <- input$output_path
      user_email_field <- input$user_email
      min_count_field <- input$min_count_for_filtering
      min_prop_field <- input$min_prop_for_filtering
      contrasts_text_field <- input$contrasts_text_optional
      # Use original file keys but optional contrasts
      sample_sheet_key <- "sample_sheet"
      bigwig_files_key <- "bigwig_files"
      contrasts_key <- "contrasts_optional"
    } else {
      # Use original field names for prepare_and_analyze
      seqID_field <- input$seqID
      report_title_field <- input$report_title
      organism_field <- input$organism
      annotation_db_field <- input$annotation_db
      hipergator_group_field <- input$hipergator_group
      output_path_field <- input$output_path
      user_email_field <- input$user_email
      min_count_field <- input$min_count_for_filtering
      min_prop_field <- input$min_prop_for_filtering
      contrasts_text_field <- input$contrasts_text
      # Use original file keys
      sample_sheet_key <- "sample_sheet"
      bigwig_files_key <- "bigwig_files"
      contrasts_key <- "contrasts"
    }
    
    # Generate parameters using the appropriate field values
    lines <- c(lines, paste("--seqID", shQuote(seqID_field)))
    lines <- c(lines, paste("--report_title", shQuote(report_title_field)))
    lines <- c(lines, paste("--organism", shQuote(organism_field)))
    lines <- c(lines, paste("--annotation_db", shQuote(annotation_db_field)))
    lines <- c(lines, paste("--hipergator-group", shQuote(hipergator_group_field)))
    lines <- c(lines, paste("--output-path", shQuote(output_path_field)))
    lines <- c(lines, paste("--user_email", shQuote(user_email_field)))
    lines <- c(lines, paste("--min_count_for_filtering", min_count_field))
    lines <- c(lines, paste("--min_prop_for_filtering", min_prop_field))
    
    # Sample sheet
    if (!is.null(values$selected_files[[sample_sheet_key]])) {
      lines <- c(lines, paste("--sample_sheet", shQuote(values$selected_files[[sample_sheet_key]])))
    }
    
    # Report metadata parameters (only add if not empty) - these are the same for all modes
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
    
    # BigWig files with sample matching
    if (!is.null(bigwig_files_key) && !is.null(values$selected_files[[bigwig_files_key]])) {
      bigwig_pairs <- c()
      sample_df <- read.csv(values$selected_files[[sample_sheet_key]], stringsAsFactors = FALSE)
      for (bigwig_file in values$selected_files[[bigwig_files_key]]) {
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
    if (!is.null(values$selected_files[[contrasts_key]])) {
      lines <- c(lines, paste("--contrasts", shQuote(values$selected_files[[contrasts_key]])))
    } else if (!is.null(contrasts_text_field) && contrasts_text_field != "") {
      lines <- c(lines, paste("--contrasts", shQuote(contrasts_text_field)))
    }
    
    # Mode-specific files
    if (input$analysis_mode == "analyze_only") {
      # Add existing DDS file for analyze-only mode
      if (!is.null(values$selected_files$existing_dds)) {
        lines <- c(lines, paste("--dds_file", shQuote(values$selected_files$existing_dds)))
      }
      # Add existing annotation if provided
      if (!is.null(values$selected_files$existing_annotation)) {
        lines <- c(lines, paste("--peak_annotation", shQuote(values$selected_files$existing_annotation)))
      }
    } else {
      # Add peak files and BAM files for other modes (prepare_only and prepare_and_analyze)
      if (!is.null(values$selected_files$peak_files)) {
        peak_pairs <- c()
        sample_df <- read.csv(values$selected_files$sample_sheet, stringsAsFactors = FALSE)
        for (peak_file in values$selected_files$peak_files) {
          peak_basename <- basename(peak_file)
          for (sample in sample_df$sample) {
            if (grepl(sample, peak_basename, fixed = TRUE)) {
              peak_pairs <- c(peak_pairs, paste0(sample, ":", peak_file))
              break
            }
          }
        }
        lines <- c(lines, paste("--peak_files", shQuote(paste(peak_pairs, collapse = ","))))
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
    }
    
    # Optional QC files (same for all modes)
    if (!is.null(values$selected_files$qc_flagstat_dir)) {
      lines <- c(lines, paste("--qc_flagstat_dir", shQuote(values$selected_files$qc_flagstat_dir)))
    }
    if (!is.null(values$selected_files$qc_frip_file)) {
      lines <- c(lines, paste("--qc_frip_file", shQuote(values$selected_files$qc_frip_file)))
    }
    
    # Optional URLs (same for all modes)
    if (!is.null(input$raw_seq_URL) && input$raw_seq_URL != "") {
      lines <- c(lines, paste("--raw_seq_URL", shQuote(input$raw_seq_URL)))
    }
    if (!is.null(input$multiqc_url) && input$multiqc_url != "") {
      lines <- c(lines, paste("--multiqc_url", shQuote(input$multiqc_url)))
    }
    
    return(lines)
  }
  # Generate params file
  observeEvent(input$generate_params, {
    req(values$params_valid)
    tryCatch({
      # Get the correct output path based on analysis mode
      output_path <- if (input$analysis_mode == "analyze_only") {
        input$output_path_analyze
      } else {
        input$output_path
      }
      
      # Get the correct seqID based on analysis mode
      seqID <- if (input$analysis_mode == "analyze_only") {
        input$seqID_analyze
      } else {
        input$seqID
      }
      
      # Ensure output directory exists
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Generate params file directly to HiPerGator storage (NOT tempdir!)
      params_file <- file.path(output_path, paste0(seqID, "_params.txt"))
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
  
  # Download params.txt file - Updated for different modes
  output$download_params <- downloadHandler(
    filename = function() {
      seqID <- if (input$analysis_mode == "analyze_only") {
        input$seqID_analyze
      } else {
        input$seqID
      }
      paste0(seqID, "_params.txt")
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
      shinyjs::enable("download_params")
      # Enable the appropriate button based on mode
      if (input$analysis_mode == 'prepare_only') {
        shinyjs::enable("submit_job_prepare")
        shinyjs::disable("submit_job_analyze")  # Ensure other is disabled
      } else {
        shinyjs::enable("submit_job_analyze")
        shinyjs::disable("submit_job_prepare")  # Ensure other is disabled
      }
    } else {
      shinyjs::disable("download_params")
      shinyjs::disable("submit_job_prepare")
      shinyjs::disable("submit_job_analyze")
    }
  })
  # Submit job - Updated to handle different analysis modes and fix file copy error
  # Lightweight event handlers that just call the main function
  observeEvent(input$submit_job_prepare, {
    submit_analysis_job()
  })
  
  observeEvent(input$submit_job_analyze, {
    submit_analysis_job()
  })
  
  # If needed for prep-and-analyze just add:
  # observeEvent(input$submit_job_third_mode, {
  #   submit_analysis_job()
  # })
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
} # This ends the server{}

shinyApp(ui = ui, server = server)
