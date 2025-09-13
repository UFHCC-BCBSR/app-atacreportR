library(GenomicRanges)

make_consensus_peaks <- function(peak_files) {
  cat("Creating consensus peaks from", length(peak_files), "samples...\n")
  
  all_peaks <- list()
  
  # Read each peak file and convert to GRanges
  for (sample_name in names(peak_files)) {
    file_path <- peak_files[[sample_name]]
    cat("Reading peaks for", sample_name, "from", basename(file_path), "\n")
    
    peaks_gr <- read_peak_file(file_path, sample_name)
    all_peaks[[sample_name]] <- peaks_gr
    
    cat("   ->", length(peaks_gr), "peaks found\n")
  }
  
  # Combine all peaks - FIX: unname the list first
  combined_peaks <- do.call(c, unname(all_peaks))  # ADD unname() here
  cat("Total peaks before filtering:", length(combined_peaks), "\n")
  
  # PRE-FILTER: Keep only peaks that appear in 2+ samples (reproducible)
  reduced_all <- GenomicRanges::reduce(combined_peaks)  # Also add explicit namespace
  overlaps <- findOverlaps(combined_peaks, reduced_all)
  peak_counts <- table(subjectHits(overlaps))
  
  # Keep regions found in at least 2 samples
  min_samples <- max(2, ceiling(length(peak_files) * 0.3))
  reproducible_indices <- as.numeric(names(peak_counts)[peak_counts >= min_samples])
  consensus_peaks <- reduced_all[reproducible_indices]
  
  cat("After reproducibility filter (≥", min_samples, "samples):", length(consensus_peaks), "peaks\n")
  cat("Filtered out", length(reduced_all) - length(consensus_peaks), "singleton peaks\n")
  
  # Add peak names
  names(consensus_peaks) <- paste0("Peak_", seq_along(consensus_peaks))
  
  return(consensus_peaks)
}

read_peak_file <- function(file_path, sample_name) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("Peak file not found: ", file_path)
  }
  
  # Read the file (assuming tab-separated, no header)
  df <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE)
  
  # Check if we have at least 3 columns (chr, start, end)
  if (ncol(df) < 3) {
    stop("Peak file must have at least 3 columns (chr, start, end): ", file_path)
  }
  
  # Create GRanges from first 3 columns
  peaks_gr <- GRanges(
    seqnames = df[,1],
    ranges = IRanges(start = df[,2] + 1, end = df[,3])  # +1 for BED to GRanges conversion
  )
  
  # Add sample info as metadata
  mcols(peaks_gr)$sample <- sample_name
  
  return(peaks_gr)
}

library(DESeq2)

create_dds_from_peaks <- function(peak_files, bam_files, sample_info, organism) {
  cat("Creating DDS object from peak and BAM files...\n")
  
  # 1. Create consensus peaks
  consensus_peaks <- make_consensus_peaks(peak_files)
  
  # 2. Count reads in consensus peaks
  count_matrix <- count_bam_reads_in_peaks(bam_files, consensus_peaks)
  
  # 3. Align sample_info with count matrix columns
  sample_info_matched <- match_sample_info_to_counts(sample_info, count_matrix)
  
  # 4. Create DESeq2 object
  cat("Creating DESeq2 object...\n")
  dds <- DESeqDataSetFromMatrix(
    countData = count_matrix,
    colData = sample_info_matched,
    design = ~ 1  # Simple design, will be updated later based on contrasts
  )
  
  # 5. Add consensus peaks as rowRanges
  rowRanges(dds) <- consensus_peaks
  
  cat("DDS object created:", nrow(dds), "peaks x", ncol(dds), "samples\n")
  return(dds)
}

match_sample_info_to_counts <- function(sample_info, count_matrix) {
  # Get sample names from count matrix columns
  count_sample_names <- colnames(count_matrix)
  
  # Try to match with sample_info
  # This assumes sample_info has a column that matches the count matrix column names
  
  # For now, create a simple sample info if none provided
  if (is.null(sample_info) || nrow(sample_info) == 0) {
    cat("No sample info provided, creating basic sample info...\n")
    sample_info_matched <- data.frame(
      sample = count_sample_names,
      row.names = count_sample_names,
      stringsAsFactors = FALSE
    )
  } else {
    # Match existing sample_info to count matrix
    # This will need to be customized based on your sample_info structure
    sample_info_matched <- sample_info[match(count_sample_names, sample_info$sample), ]
    rownames(sample_info_matched) <- count_sample_names
  }
  
  return(sample_info_matched)
}