source("R/data_preparation.R")

# Create test params (simulating what your params parser creates)
test_params <- list(
  dds_file = "",  # Empty means create from scratch
  peak_files = "sample1:/blue/licht/runs/Evans-MDS/NS3971/RUN2/GENRICH/PEAKS/GENRICH/04D_REP1_genrich.narrowPeak,sample2:/blue/licht/runs/Evans-MDS/NS3971/RUN2/GENRICH/PEAKS/GENRICH/04D_REP2_genrich.narrowPeak",
  bam_files = "sample1:/blue/licht/runs/Evans-MDS/NS3971/RUN2/GENRICH/PEAKS/04D_REP1_nsorted.bam,sample2:/blue/licht/runs/Evans-MDS/NS3971/RUN2/GENRICH/PEAKS/04D_REP2_nsorted.bam",
  bigwig_files = "sample1:/blue/licht/runs/Evans-MDS/NS3971/RUN2/OUTPUT/bowtie2/merged_library/bigwig/04D_REP1.mLb.clN.bigWig,sample2:/blue/licht/runs/Evans-MDS/NS3971/RUN2/OUTPUT/bowtie2/merged_library/bigwig/04D_REP2.mLb.clN.bigWig",
  peak_annotation = "",
  qc_flagstat_dir = "",
  qc_frip_file = ""
)

# Test the parsing
result <- parse_file_specifications(test_params)
print("Parsed results:")
print(result)

# Should show:
# $peak_files: sample1 -> /path/to/peaks1.narrowPeak, etc.
# $bam_files: sample1 -> /path/to/bam1.bam, etc.

# Test make_consensus_peaks()
source("R/peak_processing.R")

# Use the parsed peak files from your previous test
test_peak_files <- result$peak_files

# This will read your actual narrowPeak files!
consensus <- make_consensus_peaks(test_peak_files)
print(paste("Created", length(consensus), "consensus peaks"))
print(head(consensus))

# Test 3
source("R/count_processing.R")

# Use your parsed BAM files and consensus peaks from previous steps
test_bam_files <- result$bam_files
test_consensus <- consensus

# This will run featureCounts (might take a few minutes)
count_matrix <- count_bam_reads_in_peaks(test_bam_files, test_consensus)

# Check the results
dim(count_matrix)
head(count_matrix)

# Test 4

# Skip re-running
test_sample_info <- data.frame(
  sample = c("sample1", "sample2"),
  condition = c("treatment", "control"),
  stringsAsFactors = FALSE
)

# Create DDS directly from existing objects
test_dds <- DESeqDataSetFromMatrix(
  countData = count_matrix,  # From previous test
  colData = test_sample_info,
  design = ~ 1
)
rowRanges(test_dds) <- consensus  # From previous test

print(test_dds)


# Test 4

source("R/annotation.R")

# Generate annotation for your test DDS
peak_annotation <- generate_peak_annotation(test_dds, organism = "mmu")

# Check the results
head(peak_annotation)
colnames(peak_annotation)
table(peak_annotation$Annotation_short)

# Test 5

# Create a test sample sheet
test_sample_sheet <- data.frame(
  sample = c("sample1", "sample2"),
  condition = c("treatment", "control"),
  batch = c("A", "A")
)
write.csv(test_sample_sheet, "test_samples.csv", row.names = FALSE)

# Test the function
source("R/validation.R")
loaded_samples <- load_sample_metadata("test_samples.csv")
print(loaded_samples)

# Test 6

# Save your test DDS to test loading
save(test_dds, file = "test_dds.RData")

# Test loading it back
source("R/validation.R")
loaded_dds <- load_dds_file("test_dds.RData")
print(loaded_dds)

# Should be identical
identical(test_dds, loaded_dds)


# Test 6
# Test with no QC files (should return NULL gracefully)
source("R/validation.R")

test_qc_files <- list(
  flagstat_dir = NULL,
  frip_file = NULL
)

qc_result <- load_qc_data(test_qc_files)
print("QC result:")
print(qc_result)

# Test 7
source("R/validation.R")

# Test with your existing objects
validate_data_consistency(test_dds, loaded_samples, peak_annotation)

# Final full test
# Source all the functions
source("R/data_preparation.R")

# Create test params (simulating what your real params will look like)
test_report_params <- list(
  peak_files = "sample1:/blue/licht/runs/Evans-MDS/NS3971/RUN2/GENRICH/PEAKS/GENRICH/04D_REP1_genrich.narrowPeak,sample2:/blue/licht/runs/Evans-MDS/NS3971/RUN2/GENRICH/PEAKS/GENRICH/04D_REP2_genrich.narrowPeak",
  bam_files = "sample1:/blue/licht/runs/Evans-MDS/NS3971/RUN2/GENRICH/PEAKS/04D_REP1_nsorted.bam,sample2:/blue/licht/runs/Evans-MDS/NS3971/RUN2/GENRICH/PEAKS/04D_REP2_nsorted.bam",
  bigwig_files = "sample1:/blue/licht/runs/Evans-MDS/NS3971/RUN2/OUTPUT/bowtie2/merged_library/bigwig/04D_REP1.mLb.clN.bigWig,sample2:/blue/licht/runs/Evans-MDS/NS3971/RUN2/OUTPUT/bowtie2/merged_library/bigwig/04D_REP2.mLb.clN.bigWig",
  sample_sheet = "test_samples.csv",  # The file we created earlier
  dds_file = "",
  peak_annotation = "",
  qc_flagstat_dir = "",
  qc_frip_file = "",
  organism = "mmu"
)

# Test the complete pipeline
cat("=== Testing Complete Pipeline ===\n")
analysis_data <- prepare_analysis_data(test_report_params)

# Test 8
source("R/validation.R")

# Test with the complex nf-core format
nfcore_samples <- load_sample_metadata("/blue/cancercenter-dept/privapps/data/atac/clanton/NS4167_Male_GoodSamples/samplesheet.valid.csv")

# Test with your simple format  
simple_samples <- load_sample_metadata("/blue/cancercenter-dept/hkates/Apps/new-atacreportR/test_samples.csv")

# Test 9

source("R/annotation.R")

# Reload the annotation
test_anno <- load_peak_annotation("/blue/cancercenter-dept/privapps/data/atac/clanton/NS4167_Male_GoodSamples/consensus_peaks.mLb.clN.annotatePeaks.txt")

head(test_anno)
colnames(test_anno)
