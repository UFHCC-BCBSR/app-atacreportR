#!/usr/bin/env Rscript

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript run_prepare_data_only.R <params_file>")
}

params_file <- args[1]

# Set library paths
.libPaths(c('/blue/cancercenter-dept/shared/R/library/4.5/', 
           '/usr/local/lib/R/site-library', 
           '/usr/lib/R/site-library', 
           '/usr/lib/R/library'))

# Source the data preparation functions
source('R/data_preparation.R')

# Define the %||% operator if not already available
`%||%` <- function(x, y) if (is.null(x)) y else x

# Parse params file
lines <- readLines(params_file)
params <- list()

for (line in lines) {
  line <- trimws(line)
  if (line == '' || startsWith(line, '#')) next
  
  if (startsWith(line, '--')) {
    space_pos <- regexpr('\\s', line)
    if (space_pos > 0) {
      param_name <- gsub('^--', '', substr(line, 1, space_pos - 1))
      param_value <- trimws(substr(line, space_pos + 1, nchar(line)))
      
      # Remove surrounding quotes
      if ((startsWith(param_value, '"') && endsWith(param_value, '"')) ||
          (startsWith(param_value, "'") && endsWith(param_value, "'"))) {
        param_value <- substr(param_value, 2, nchar(param_value) - 1)
      }
      params[[param_name]] <- param_value
    }
  }
}

# Handle hyphenated parameter names
if ('output-path' %in% names(params)) {
  params[['output_path']] <- params[['output-path']]
}
if ('hipergator-group' %in% names(params)) {
  params[['hipergator_group']] <- params[['hipergator-group']]
}

# Force prepare-only mode by setting dds_file to NULL
params[['dds_file']] <- NULL

cat('Running data preparation with parameters:\n')
str(params)

# Run the preparation
result <- prepare_analysis_data(params)
cat('Data preparation completed successfully!\n')

# Display output files
seqID <- params[['seqID']] %||% 'project'
output_path <- params[['output_path']] %||% getwd()

cat('Files created:\n')
cat('  - DDS object:', file.path(output_path, paste0(seqID, '.dds.RData')), '\n')
cat('  - Consensus peaks:', file.path(output_path, paste0(seqID, '.consensus-peaks.txt')), '\n')
cat('  - Annotated peaks:', file.path(output_path, paste0(seqID, '.annotated.consensus-peaks.txt')), '\n')
