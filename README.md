# ATAC-seq Peak Analysis Reporter

An interactive Shiny application for configuring, running, and reporting differential analysis of peak data (ATAC-seq, CUT&RUN, ChIP-seq, and more). This application requires the [`atacreportR`](https://github.com/UFHCC-BCBSR/pkg-atacreportR) R package, which provides all data processing, statistical analysis, and visualization functions. Designed for use with the University of Florida's HiPerGator computing system.

## Overview

Peak Reporter provides a user-friendly interface for managing complex peak analysis workflows, from initial data preparation through comprehensive differential analysis reporting. The application supports three distinct analysis modes to accommodate different stages of the analysis pipeline.

Currently this application can **only** be run on UF HiPerGator due to its interaction with the /blue and /orange filesystems

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

## Installation

### Prerequisites

1. **Install the atacreportR package (REQUIRED):**

```r
# From GitHub
remotes::install_github("UFHCC-BCBSR/pkg-atacreportR")
```

2. **Clone this application:**

```bash
git clone https://github.com/UFHCC-BCBSR/app-atacreportR.git
cd app-atacreportR
```

## Quick Start

1. **Start an RStudio Server session on HiPerGator:**

```bash
module load R/4.5
rserver
```

2. **Launch the Shiny app:**

```r
library(shiny)
runApp("app.R")
```

3. **Use the web interface to:**
   - Configure analysis parameters
   - Select input files (sample sheets, peaks, BigWig files, etc.)
   - Generate `params.txt` file
   - Submit SLURM job directly from the app

## How It Works

This application provides an interactive way to configure and run peak-based differential analysis on HiPerGator. The workflow consists of three main components:

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

R Markdown document that reads the parameter file and calls `atacreportR` functions to generate a comprehensive HTML report including:
- Sample quality control metrics (FRiP, alignment rates, peak counts)
- Peak annotation and genomic distribution
- Differential accessibility analysis using edgeR
- GO term and KEGG pathway enrichment analysis
- Interactive visualizations (PCA, MA plots, volcano plots)
- Downloadable data tables and results

### 3. SLURM Job Submission

- `render-report-with-params.sbatch`: Full analysis workflow
- `prepare-data-only.sbatch`: Data preparation only

## atacreportR Package Functions

The analysis relies entirely on the `atacreportR` R package, which provides all core functionality:

- **Data preparation**: Flexible data loading from nf-core output, existing DDS objects, or individual files
- **Peak processing**: Consensus peak creation across samples with reproducibility filtering
- **Count processing**: Read quantification using featureCounts with customizable filtering
- **Differential analysis**: edgeR-based statistical testing with TMM or spike-in normalization
- **Annotation**: Peak annotation with nearby genes using ChIPseeker
- **Validation**: Comprehensive input validation and QC checks
- **Visualization**: Interactive plots including PCA, correlation heatmaps, MA plots, volcano plots
- **Enrichment analysis**: GO term and KEGG pathway analysis with gene symbol mapping

## File Structure

```
app-atacreportR/
├── app.R                              # Main Shiny application
├── differential_peak_report.Rmd       # Analysis report template
├── render-report-with-params.sbatch   # Full analysis SLURM script
├── prepare-data-only.sbatch           # Data preparation SLURM script
├── text-config.R                      # Report text configuration
├── scripts/
│   └── run_prepare_data_only.R       # Standalone data prep script
├── test-input/                        # Example input files
└── logs/                              # SLURM job logs
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
- **BigWig files**: Coverage tracks for WashU Browser visualization
- **Contrasts**: Either text input or file specifying comparisons (format: `Group1_vs_Group2`)

### Optional Files

- **Spike-in DDS**: For spike-in normalization (any mode)
- **Peak annotation**: Pre-computed peak annotations (HOMER or ChIPseeker format)
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
sbatch render-report-with-params.sbatch 
  --params-file output/my-analysis_params.txt 
  --title "My ATAC-seq Analysis"

# Data preparation only
sbatch prepare-data-only.sbatch 
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
- **Excel files**: Downloadable differential accessibility results
  - Promoter peaks (within 1kb of TSS)
  - All annotated regions
  - Full results (all tested peaks)
- **WashU Browser Tracks**: BigBed files for genome browser visualization
- **Enrichment Results**: GO term and KEGG pathway analysis
- **Log Files**: SLURM job execution logs

## Supported Organisms

- Mouse (mmu) - mm10 genome with org.Mm.eg.db
- Human (hsa) - hg38 genome with org.Hs.eg.db

## Requirements

- **R 4.5+** with **atacreportR package installed**
- **HiPerGator access** for SLURM job submission
- **Group credentials** for file browsing
- **Input data** in the required formats

## Troubleshooting

- **atacreportR not found**: Ensure package is installed: `remotes::install_github("UFHCC-BCBSR/pkg-atacreportR")`
- **Authentication issues**: Verify group credentials in the app
- **File not found**: Ensure all input files are accessible from compute nodes
- **Memory issues**: Increase `--mem` in the SLURM script for large datasets
- **Job logs**: Check SLURM output files in the `logs/` directory for detailed error messages
- **Validation errors**: Use the app's validation feature to check file compatibility

## Advanced Features

- **Cross-mode compatibility**: Parameters generated in one mode can be loaded and modified for other modes
- **File matching validation**: Automatic verification that sample names match across file types
- **Flexible normalization**: Automatic detection and application of appropriate normalization methods
- **Interactive file browser**: Integrated HiPerGator file system navigation
- **Real-time validation**: Immediate feedback on parameter configuration
- **Parallel processing**: Uses BiocParallel for fast enrichment analysis

## Citation

If you use this application in your research, please cite:

```
University of Florida Health Cancer Center Bioinformatics & Computational Research (BCB-SR)
ATAC-seq Peak Analysis Reporter
https://github.com/UFHCC-BCBSR/app-atacreportR
```

## Support

For questions or issues:
- Open an issue on GitHub
- Contact UF Health Cancer Center BCB-SR

## Related Projects

- [atacreportR R Package](https://github.com/UFHCC-BCBSR/pkg-atacreportR) - Required package providing all analysis functions
- [nf-core/atacseq](https://github.com/nf-core/atacseq) - Upstream ATAC-seq pipeline
