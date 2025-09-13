load_sample_metadata <- function(sample_sheet_path) {
  cat("Loading sample metadata from:", basename(sample_sheet_path), "\n")
  
  # Check if file exists
  if (!file.exists(sample_sheet_path)) {
    stop("Sample sheet not found: ", sample_sheet_path)
  }
  
  # Read the CSV file
  sample_info <- read.csv(sample_sheet_path, stringsAsFactors = FALSE)
  
  # Basic validation
  if (nrow(sample_info) == 0) {
    stop("Sample sheet is empty")
  }
  
  # Detect format type
  is_nfcore_format <- any(c("fastq_1", "fastq_2", "replicate") %in% colnames(sample_info))
  
  if (is_nfcore_format) {
    cat("Detected nf-core sample sheet format\n")
    sample_info <- process_nfcore_format(sample_info)
  } else {
    cat("Detected simple sample sheet format\n")
    sample_info <- process_simple_format(sample_info)
  }
  
  cat("Loaded metadata for", nrow(sample_info), "samples\n")
  cat("Available metadata columns:", paste(colnames(sample_info), collapse = ", "), "\n")
  
  return(sample_info)
}

process_nfcore_format <- function(sample_info) {
  # Remove technical replicate suffixes (nf-core adds _T1, _T2, etc.)
  sample_info$sample <- gsub("_T\\d+", "", sample_info$sample)
  
  # Remove fastq-related columns
  fastq_cols <- grep("fastq", colnames(sample_info), ignore.case = TRUE)
  if (length(fastq_cols) > 0) {
    sample_info <- sample_info[, -fastq_cols, drop = FALSE]
    cat("Removed", length(fastq_cols), "fastq-related columns\n")
  }
  
  # Remove other nf-core specific columns that aren't useful
  remove_cols <- c("single_end", "control")
  sample_info <- sample_info[, !colnames(sample_info) %in% remove_cols, drop = FALSE]
  
  # Fix any obvious typos in condition names
  if ("Condition" %in% colnames(sample_info)) {
    sample_info$Condition[sample_info$Condition == "Heath"] <- "Heat"  # Fix typo
  }
  
  # Remove duplicate rows BEFORE setting row names
  sample_info <- unique(sample_info)
  
  # Check for remaining duplicates
  if (any(duplicated(sample_info$sample))) {
    cat("Warning: Still have duplicate sample names after processing\n")
    # Keep only first occurrence of each sample
    sample_info <- sample_info[!duplicated(sample_info$sample), ]
  }
  
  # Set row names to sample names (now should be unique)
  rownames(sample_info) <- sample_info$sample
  
  return(sample_info)
}

process_simple_format <- function(sample_info) {
  # Check for required columns - at minimum we need sample identifiers
  if (!"sample" %in% colnames(sample_info)) {
    # Try common alternatives
    if ("sample_id" %in% colnames(sample_info)) {
      sample_info$sample <- sample_info$sample_id
      cat("Using 'sample_id' column as 'sample'\n")
    } else if ("Sample" %in% colnames(sample_info)) {
      sample_info$sample <- sample_info$Sample
      cat("Using 'Sample' column as 'sample'\n")
    } else {
      stop("Sample sheet must have a 'sample' column (or 'sample_id', 'Sample')")
    }
  }
  
  # Set row names to sample names
  rownames(sample_info) <- sample_info$sample
  
  return(sample_info)
}
load_dds_file <- function(dds_file_path) {
  cat("Loading existing DDS file from:", basename(dds_file_path), "\n")
  
  # Check if file exists
  if (!file.exists(dds_file_path)) {
    stop("DDS file not found: ", dds_file_path)
  }
  
  # Load the RData file
  env <- new.env()
  load(dds_file_path, envir = env)
  
  # Look for DDS object in the loaded environment
  # Common variable names: dds, targetGenome_dds, etc.
  possible_names <- c("dds", "targetGenome_dds", "deseq_obj", "dds_obj")
  
  dds_obj <- NULL
  for (var_name in ls(env)) {
    obj <- get(var_name, envir = env)
    if (inherits(obj, "DESeqDataSet")) {
      dds_obj <- obj
      cat("Found DESeqDataSet object:", var_name, "\n")
      break
    }
  }
  
  # If not found by class, try common names
  if (is.null(dds_obj)) {
    for (name in possible_names) {
      if (exists(name, envir = env)) {
        obj <- get(name, envir = env)
        if (inherits(obj, c("DESeqDataSet", "DGEList"))) {
          dds_obj <- obj
          cat("Found object by name:", name, "\n")
          break
        }
      }
    }
  }
  
  if (is.null(dds_obj)) {
    stop("No DESeqDataSet object found in: ", dds_file_path)
  }
  
  cat("Loaded DDS object:", nrow(dds_obj), "peaks x", ncol(dds_obj), "samples\n")
  
  return(dds_obj)
}

load_qc_data <- function(qc_files) {
  cat("Loading QC data...\n")
  
  qc_data <- list()
  
  # Load flagstat files if directory provided
  if (!is.null(qc_files$flagstat_dir) && dir.exists(qc_files$flagstat_dir)) {
    cat("Loading flagstat files from:", qc_files$flagstat_dir, "\n")
    
    flagstat_files <- list.files(qc_files$flagstat_dir, pattern = "\\.flagstat$", full.names = TRUE)
    
    if (length(flagstat_files) > 0) {
      qc_data$flagstat <- load_flagstat_files(flagstat_files)
      cat("Loaded", length(flagstat_files), "flagstat files\n")
    } else {
      cat("No .flagstat files found in directory\n")
    }
  } else {
    cat("No flagstat directory specified or directory not found\n")
  }
  
  # Load FRiP file if provided
  if (!is.null(qc_files$frip_file) && file.exists(qc_files$frip_file)) {
    cat("Loading FRiP scores from:", basename(qc_files$frip_file), "\n")
    
    qc_data$frip <- load_frip_file(qc_files$frip_file)
    cat("Loaded FRiP scores for", nrow(qc_data$frip), "samples\n")
  } else {
    cat("No FRiP file specified or file not found\n")
  }
  
  if (length(qc_data) == 0) {
    cat("No QC data loaded - this is optional\n")
    return(NULL)
  }
  
  return(qc_data)
}

load_flagstat_files <- function(flagstat_files) {
  # Simple implementation - just return file paths for now
  # Could be expanded to actually parse flagstat content
  flagstat_data <- data.frame(
    sample = basename(flagstat_files),
    file_path = flagstat_files,
    stringsAsFactors = FALSE
  )
  
  return(flagstat_data)
}

load_frip_file <- function(frip_file) {
  # Try to read FRiP file - format may vary
  tryCatch({
    frip_data <- read.delim(frip_file, stringsAsFactors = FALSE)
    return(frip_data)
  }, error = function(e) {
    cat("Warning: Could not parse FRiP file:", e$message, "\n")
    return(NULL)
  })
}

validate_data_consistency <- function(dds, sample_info, peak_anno) {
  cat("Validating data consistency...\n")
  
  # Check that DDS and sample_info have matching samples
  dds_samples <- colnames(dds)
  info_samples <- rownames(sample_info)
  
  if (!identical(sort(dds_samples), sort(info_samples))) {
    missing_in_info <- setdiff(dds_samples, info_samples)
    missing_in_dds <- setdiff(info_samples, dds_samples)
    
    if (length(missing_in_info) > 0) {
      cat("Warning: Samples in DDS but not in sample_info:", paste(missing_in_info, collapse = ", "), "\n")
    }
    if (length(missing_in_dds) > 0) {
      cat("Warning: Samples in sample_info but not in DDS:", paste(missing_in_dds, collapse = ", "), "\n")
    }
  } else {
    cat("Sample names match between DDS and sample_info\n")
  }
  
  # Check that DDS and peak annotation have matching peaks
  dds_peaks <- rownames(dds)
  anno_peaks <- peak_anno$interval
  
  if (!identical(sort(dds_peaks), sort(anno_peaks))) {
    missing_in_anno <- setdiff(dds_peaks, anno_peaks)
    missing_in_dds <- setdiff(anno_peaks, dds_peaks)
    
    if (length(missing_in_anno) > 0) {
      cat("Warning: Peaks in DDS but not in annotation (showing first 5):", 
          paste(head(missing_in_anno, 5), collapse = ", "), "\n")
    }
    if (length(missing_in_dds) > 0) {
      cat("Warning: Peaks in annotation but not in DDS (showing first 5):", 
          paste(head(missing_in_dds, 5), collapse = ", "), "\n")
    }
  } else {
    cat("Peak names match between DDS and annotation\n")
  }
  
  # Basic sanity checks
  cat("Data summary:\n")
  cat("  DDS object:", nrow(dds), "peaks x", ncol(dds), "samples\n")
  cat("  Sample info:", nrow(sample_info), "samples with", ncol(sample_info), "metadata columns\n")
  cat("  Peak annotation:", nrow(peak_anno), "peaks with", ncol(peak_anno), "annotation columns\n")
  
  cat("Validation complete\n")
  
  return(invisible(TRUE))
}