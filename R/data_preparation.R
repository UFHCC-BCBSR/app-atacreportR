source("R/peak_processing.R")
source("R/count_processing.R") 
source("R/annotation.R")
source("R/validation.R")

prepare_analysis_data <- function(report_params) {
  cat("🔄 Loading and preparing analysis data...\n")
  
  # 1. Parse file specifications from params
  file_specs <- parse_file_specifications(report_params)
  
  # 2. Load sample metadata
  sample_info <- load_sample_metadata(report_params$sample_sheet)
  
  # 3. Load or create DDS object
  if (!is.null(file_specs$dds_file) && file.exists(file_specs$dds_file)) {
    cat("📂 Loading existing DDS file...\n")
    dds <- load_dds_file(file_specs$dds_file)
  } else {
    cat("🔨 Creating DDS from peak and BAM files...\n")
    dds <- create_dds_from_peaks(
      peak_files = file_specs$peak_files,
      bam_files = file_specs$bam_files,  # CHANGED: bam_files instead of count_files
      sample_info = sample_info,
      organism = report_params$organism
    )
  }
  
  # 4. Load or generate peak annotation
  if (!is.null(file_specs$peak_annotation) && file.exists(file_specs$peak_annotation)) {
    cat("📂 Loading existing peak annotation...\n") 
    peak_anno <- load_peak_annotation(file_specs$peak_annotation)
  } else {
    cat("🔨 Generating peak annotation with ChIPseeker...\n")
    peak_anno <- generate_peak_annotation(dds, report_params$organism)
  }
  
  # 5. Load QC data (optional)
  qc_data <- load_qc_data(file_specs$qc_files)
  
  # 6. Validate everything matches up
  validate_data_consistency(dds, sample_info, peak_anno)
  
  cat("✅ Data preparation complete!\n")
  return(list(
    dds = dds,
    sample_info = sample_info,
    peaks_anno = peak_anno, 
    file_specs = file_specs,  
    qc_data = qc_data,
    bigwig_files = file_specs$bigwig_files
  ))
}

parse_file_specifications <- function(report_params) {
  
  # Parse peak files: "sample1:/path/file1.bed,sample2:/path/file2.broadPeak"
  peak_files <- NULL
  if (!is.null(report_params$peak_files) && report_params$peak_files != "") {
    pairs <- strsplit(report_params$peak_files, ",")[[1]]
    peak_files <- setNames(
      sapply(pairs, function(x) strsplit(x, ":")[[1]][2]), 
      sapply(pairs, function(x) strsplit(x, ":")[[1]][1])
    )
  }
  
  # Parse BAM files: "sample1:/path/file1.bam,sample2:/path/file2.bam"  
  bam_files <- NULL  
  if (!is.null(report_params$bam_files) && report_params$bam_files != "") {
    pairs <- strsplit(report_params$bam_files, ",")[[1]]
    bam_files <- setNames(
      sapply(pairs, function(x) strsplit(x, ":")[[1]][2]),
      sapply(pairs, function(x) strsplit(x, ":")[[1]][1])  
    )
  }
  
  # Parse bigwig files: same format
  bigwig_files <- NULL
  if (!is.null(report_params$bigwig_files) && report_params$bigwig_files != "") {
    pairs <- strsplit(report_params$bigwig_files, ",")[[1]] 
    bigwig_files <- setNames(
      sapply(pairs, function(x) strsplit(x, ":")[[1]][2]),
      sapply(pairs, function(x) strsplit(x, ":")[[1]][1])
    )
  }
  
  return(list(
    dds_file = if(is.null(report_params$dds_file) || report_params$dds_file == "") NULL else report_params$dds_file,
    peak_files = peak_files,
    bam_files = bam_files,  # CHANGED: bam_files instead of count_files
    peak_annotation = if(is.null(report_params$peak_annotation) || report_params$peak_annotation == "") NULL else report_params$peak_annotation,
    bigwig_files = bigwig_files,
    qc_files = list(
      flagstat_dir = if(is.null(report_params$qc_flagstat_dir) || report_params$qc_flagstat_dir == "") NULL else report_params$qc_flagstat_dir,
      frip_file = if(is.null(report_params$qc_frip_file) || report_params$qc_frip_file == "") NULL else report_params$qc_frip_file
    )
  ))
}