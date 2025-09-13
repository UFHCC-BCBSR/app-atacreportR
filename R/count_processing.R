library(Rsubread)

count_bam_reads_in_peaks <- function(bam_files, consensus_peaks) {
  cat("Counting reads in", length(consensus_peaks), "consensus peaks across", length(bam_files), "samples...\n")
  
  # Create temporary SAF file for featureCounts
  saf_file <- create_saf_file(consensus_peaks)
  
  # Prepare BAM file vector (featureCounts wants a vector, not named list)
  bam_vector <- as.character(bam_files)
  names(bam_vector) <- names(bam_files)
  
  # Check that BAM files exist
  missing_bams <- bam_vector[!file.exists(bam_vector)]
  if (length(missing_bams) > 0) {
    stop("BAM files not found: ", paste(names(missing_bams), collapse = ", "))
  }
  
  cat("Running featureCounts...\n")


fc_result <- featureCounts(
  files = bam_vector,
  annot.ext = saf_file,
  isGTFAnnotationFile = FALSE,
  isPairedEnd = TRUE,
  nthreads = 14,
  verbose = TRUE
)
  
  # Clean up temporary file
  unlink(saf_file)
  
  # Extract count matrix
  count_matrix <- fc_result$counts
  
  # Set proper column names (sample names)
  colnames(count_matrix) <- names(bam_files)
  
  # Set row names to match consensus peak names
  rownames(count_matrix) <- names(consensus_peaks)
  
  cat("Counting complete:", nrow(count_matrix), "peaks x", ncol(count_matrix), "samples\n")
  
  return(count_matrix)
}

create_saf_file <- function(consensus_peaks) {
  # Create SAF format data frame
  saf_df <- data.frame(
    GeneID = names(consensus_peaks),
    Chr = as.character(seqnames(consensus_peaks)),
    Start = start(consensus_peaks),
    End = end(consensus_peaks),
    Strand = "+",  # Use + for all peaks (standard for ATAC-seq)
    stringsAsFactors = FALSE
  )
  
  # Write to temporary file
  temp_saf <- tempfile(fileext = ".saf")
  write.table(saf_df, file = temp_saf, sep = "\t", quote = FALSE, 
              row.names = FALSE, col.names = TRUE)
  
  cat("Created SAF file with", nrow(saf_df), "features\n")
  return(temp_saf)
}