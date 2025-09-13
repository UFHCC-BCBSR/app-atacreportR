library(ChIPseeker)
library(GenomicFeatures)

generate_peak_annotation <- function(dds, organism) {
  cat("Generating peak annotation with ChIPseeker for organism:", organism, "\n")
  
  # Get the consensus peaks from the DDS object
  consensus_peaks <- rowRanges(dds)
  
  # Load appropriate TxDb and OrgDb based on organism
  if (organism == "mmu") {
    library(TxDb.Mmusculus.UCSC.mm10.knownGene)
    library(org.Mm.eg.db)
    txdb <- TxDb.Mmusculus.UCSC.mm10.knownGene
    orgdb <- org.Mm.eg.db
    cat("Using mouse genome annotation (mm10)\n")
  } else if (organism == "hsa") {
    library(TxDb.Hsapiens.UCSC.hg38.knownGene)
    library(org.Hs.eg.db)
    txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
    orgdb <- org.Hs.eg.db
    cat("Using human genome annotation (hg38)\n")
  } else {
    stop("Unsupported organism: ", organism, ". Use 'mmu' or 'hsa'")
  }
  
  # Annotate peaks
  cat("Annotating", length(consensus_peaks), "consensus peaks...\n")
  peak_anno <- annotatePeak(
    peak = consensus_peaks,
    tssRegion = c(-1000, 1000),
    TxDb = txdb,
    verbose = TRUE
  )
  
  # Convert to data frame
  anno_df <- as.data.frame(peak_anno)
  
  # ADD GENE SYMBOL MAPPING
  cat("Adding gene symbol mapping...\n")
  anno_df$Gene.Name <- mapIds(
    orgdb,
    keys = anno_df$geneId,
    column = "SYMBOL",
    keytype = "ENTREZID",
    multiVals = "first"
  )
  
  # Also add Entrez ID for compatibility (rename geneId)
  anno_df$Entrez.ID <- anno_df$geneId
  
  # Add peak IDs to match DDS rownames
  anno_df$interval <- names(consensus_peaks)
  
  # Reorder columns to put interval first
  anno_df <- anno_df[, c("interval", setdiff(names(anno_df), "interval"))]
  
  # Add simplified annotation column
  anno_df$Annotation_short <- gsub(" .*", "", anno_df$annotation)
  
  cat("Annotation complete:", nrow(anno_df), "peaks annotated\n")
  cat("Gene symbols added for", sum(!is.na(anno_df$Gene.Name)), "peaks\n")
  
  return(anno_df)
}

load_peak_annotation <- function(annotation_file_path) {
  cat("Loading existing peak annotation from:", basename(annotation_file_path), "\n")
  
  # Check if file exists
  if (!file.exists(annotation_file_path)) {
    stop("Peak annotation file not found: ", annotation_file_path)
  }
  
  # Read the annotation file (HOMER format is tricky)
  # HOMER files often have multiple header lines and complex formatting
  lines <- readLines(annotation_file_path)
  
  # Find the actual header line (contains "Chr", "Start", "End", etc.)
  header_line <- 1
  for (i in 1:min(5, length(lines))) {
    if (grepl("Chr.*Start.*End", lines[i])) {
      header_line <- i
      break
    }
  }
  
  cat("Found header at line", header_line, "\n")
  
  # Read the file starting from the header line
  peaks_anno <- read.delim(
    annotation_file_path,
    header = TRUE,
    skip = header_line - 1,
    stringsAsFactors = FALSE,
    sep = "\t"
  )
  
  # Clean up column names (remove extra characters)
  colnames(peaks_anno) <- gsub("\\.\\..*$", "", colnames(peaks_anno))  # Remove trailing dots and text
  colnames(peaks_anno) <- gsub("\\.$", "", colnames(peaks_anno))       # Remove single trailing dots
  
  # Standardize first column name
  colnames(peaks_anno)[1] <- "interval"
  
  # Add simplified annotation column if it doesn't exist
  if (!"Annotation_short" %in% colnames(peaks_anno) && "Annotation" %in% colnames(peaks_anno)) {
    peaks_anno$Annotation_short <- gsub(" .*", "", peaks_anno$Annotation)
  }
  
  cat("Loaded annotation for", nrow(peaks_anno), "peaks\n")
  cat("First few column names:", paste(head(colnames(peaks_anno), 10), collapse = ", "), "\n")
  
  return(peaks_anno)
}