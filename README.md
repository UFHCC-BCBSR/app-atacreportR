# Peak Reporter

An interactive Shiny application for configuring, running, and reporting differential analysis of peak data (CUT&RUN, ChIP-seq, ATAC-seq, and more). Designed for use with the University of Florida's HiPerGator computing system.

## Overview

Peak Reporter provides a user-friendly interface for managing complex peak analysis workflows, from initial data preparation through comprehensive differential analysis reporting. The application supports three distinct analysis modes to accommodate different stages of the analysis pipeline.

## Features

### Analysis Modes

1. **Prepare Data Only**
   - Creates consensus peaks across samples
   - Performs read quantification using featureCounts
   - Generates peak annotations
   - Creates DESeq2 DDS objects for downstream analysis

2. **Full Workflow** (Prepare + Analyze)
   - All data preparation steps
   - Differential accessibility analysis
   - Comprehensive HTML report generation
   - Track hub creation for genome browser visualization

3. **Analysis from Existing Data**
   - Uses pre-existing DDS objects and annotations
   - Re-run analysis with different parameters or contrasts
   - Analyze sample subsets from previous analyses

### Supported Peak Callers
- MACS2 (default)
- Genrich

### Normalization Methods
- TMM normalization (default)
- Spike-in normalization (when spike-in DDS provided)

## Quick Start

1. **Clone the repository:**
   ```bash
   git clone <repository-url>
   cd new-atacreportR
   ```

2. **Run the shiny app in an rserver session:**
   ```bash
   module load R/4.5
   rserver
   ```

3. **Use the web interface to:**
   - Configure analysis parameters
   - Select input files (sample sheets, peaks, BigWig files, etc.)
   - Generate `params.txt` file
   - Submit SLURM job directly from the app

## How It Works

This project provides an interactive way to configure and run peak-based differential analysis on HiPerGator. The workflow consists of three main components:

### 1. Parameter Generation (`app.R`)
Interactive Shiny application that allows users to:
- Load existing parameter files or start fresh
- Select analysis mode (prepare-only, full workflow, or analyze-only)
- Browse HiPerGator files through integrated file browser
- Configure analysis settings (organism, peak caller, filtering thresholds, etc.)
- Add report metadata (PI, project info, etc.)
- Generate properly formatted parameter files
- Submit SLURM jobs with a single click

### 2. Analysis Report (`differential_peak_report.Rmd`)
R Markdown document that reads the parameter file and generates a comprehensive HTML report including:
- Sample quality control metrics
- Peak annotation and genomic distribution
- Differential accessibility analysis
- Pathway enrichment analysis
- Interactive visualizations and downloadable data tables

### 3. SLURM Job Submission
- `render-report-with-params.sbatch`: Full analysis workflow
- `prepare-data-only.sbatch`: Data preparation only

## Analysis Modules (`R/` directory)

The analysis is organized into modular R scripts sourced by the main report:

- **`data_preparation.R`**: Flexible data loading system that handles multiple input scenarios (nf-core output, existing DDS objects, individual files). Manages sample sheet integration and spike-in data loading.

- **`peak_processing.R`**: Merges peak files across samples, creates consensus peak sets, and handles peak caller-specific formatting differences.

- **`count_processing.R`**: Generates count matrices from consensus peaks using featureCounts, applies filtering thresholds, and prepares data for statistical analysis.

- **`analysis.R`**: Performs differential accessibility analysis using DESeq2/edgeR pipeline with support for both TMM and spike-in normalization methods.

- **`annotation.R`**: Annotates peaks with nearby genes and genomic features using organism-specific databases (supports mouse and human).

- **`validation.R`**: Comprehensive input validation, quality control checks, file matching verification, and error handling with informative messages.

- **`report_funcs.R`**: Utility functions for plotting, data formatting, table generation, and report styling. Includes functions for PCA plots, heatmaps, and interactive data tables.

## File Structure

```
├── app.R                              # Main Shiny application
├── differential_peak_report.Rmd       # Analysis report template
├── render-report-with-params.sbatch   # Full analysis SLURM script
├── prepare-data-only.sbatch           # Data preparation SLURM script
├── text-config.R                      # Report text configuration
├── R/                                 # Analysis modules
│   ├── data_preparation.R            # Flexible data loading system
│   ├── peak_processing.R             # Consensus peak creation
│   ├── count_processing.R            # Count matrix generation
│   ├── analysis.R                    # Differential analysis
│   ├── annotation.R                  # Peak annotation
│   ├── validation.R                  # Input validation & QC
│   └── report_funcs.R                # Report utility functions
├── scripts/
│   └── run_prepare_data_only.R       # Standalone data prep script
├── legacy/
│   └── render-report.sbatch          # Legacy SLURM script
├── mm10.chrom.sizes                   # Mouse genome chromosome sizes
├── hg38.chrom.sizes                   # Human genome chromosome sizes
```

## Input Requirements

### Required Files (varies by mode)

**For Data Preparation Modes:**
- **Sample sheet**: CSV with columns `sample`, `group`, and any covariates
- **Peak files**: `.narrowPeak` or `.broadPeak` format (one per sample)
- **BAM files**: Aligned reads (one per sample)

**For Analysis from Existing Data:**
- **DDS object**: `.RData` file from previous analysis
- **Sample sheet**: CSV matching samples in DDS object
- **BigWig files**: Coverage tracks (one per sample)

**For All Analysis Modes:**
- **BigWig files**: Coverage tracks for visualization
- **Contrasts**: Either text input or file specifying comparisons

### Optional Files
- **Spike-in DDS**: For spike-in normalization (any mode)
- **Peak annotation**: Pre-computed peak annotations
- **QC files**: Flagstat summaries and FRIP scores
- **URLs**: Links to raw data and MultiQC reports

## Usage Examples

### Using the Shiny App
1. Login with your HiPerGator group credentials
2. Select your analysis mode
3. Browse and select required files
4. Configure analysis parameters
5. Validate and generate parameters
6. Submit job directly from the app

### Manual SLURM Submission
```bash
# Full analysis workflow
sbatch render-report-with-params.sbatch \
  --params-file output/my-analysis_params.txt \
  --title "My Peak Analysis"

# Data preparation only
sbatch prepare-data-only.sbatch \
  --params-file output/my-analysis_params.txt
```

### Loading Existing Parameters
The app can load existing parameter files, allowing you to:
- Modify previous analyses
- Use templates for similar projects
- Switch between analysis modes
- Reproduce analyses with updated data

## Output

### Data Preparation Mode
- **DDS object**: `seqID.dds.RData`
- **Consensus peaks**: `seqID.consensus-peaks.txt`
- **Annotated peaks**: `seqID.annotated.consensus-peaks.txt`

### Full Analysis Mode
- **HTML Report**: Comprehensive analysis with interactive plots and tables
- **Parameter File**: Record of all analysis settings for reproducibility
- **Data tables**: Downloadable Excel files with results
- **Log Files**: SLURM job execution logs

## Supported Organisms
- Mouse (mmu) with org.Mm.eg.db
- Human (hsa) with org.Hs.eg.db

## Requirements
- **R 4.5+** with required packages
- **HiPerGator access** for SLURM job submission
- **Group credentials** for file browsing
- **Input data** in the required formats

## Troubleshooting
- **Authentication issues**: Verify group credentials in the app
- **File not found**: Ensure all input files are accessible from compute nodes
- **Memory issues**: Increase `--mem` in the SLURM script for large datasets
- **Job logs**: Check SLURM output for detailed error messages
- **Validation errors**: Use the app's validation feature to check file compatibility

## Advanced Features
- **Cross-mode compatibility**: Parameters generated in one mode can be loaded and modified for other modes
- **File matching validation**: Automatic verification that sample names match across file types
- **Flexible normalization**: Automatic detection and application of appropriate normalization methods
- **Interactive file browser**: Integrated HiPerGator file system navigation
- **Real-time validation**: Immediate feedback on parameter configuration
