```
# ATAC-seq Analysis Parameter Generator & Report

An interactive Shiny application for generating ATAC-seq differential accessibility analysis parameters and submitting HiPerGator SLURM jobs.

## Quick Start

1. **Clone the repository:**
   ```bash
   git clone <repository-url>
   cd new-atacreportR
   ```

2. **Run the shiny app in an rserver session:**
   ```bash
   module load R/4.5
   rserver"
   ```

3. **Use the web interface to:**
   - Configure analysis parameters
   - Select input files (sample sheets, peaks, BigWig files, etc.)
   - Generate `params.txt` file
   - Submit SLURM job directly from the app

## How It Works

This project provides an interactive way to configure and run ATAC-seq differential accessibility analysis on HiPerGator. The workflow consists of three main components:

### 1. Parameter Generation (`app.R`)
Interactive Shiny application that allows users to:
- Load existing parameter files or start fresh
- Select sample sheets and data files through a file browser
- Configure analysis settings (organism, filtering thresholds, etc.)
- Add report metadata (PI, project info, etc.)
- Generate properly formatted parameter files
- Submit SLURM jobs with a single click

### 2. Analysis Report (`differential_peak_report.Rmd`)
R Markdown document that reads the parameter file and generates a comprehensive HTML report including:
- Sample quality control metrics
- Peak annotation and genomic distribution
- Differential accessibility analysis
- Pathway enrichment analysis
- Interactive visualizations and tables

### 3. SLURM Job Submission (`render-report-with-params.sbatch`)
Batch script that:
- Accepts parameter file and report title as arguments
- Sets up the R environment with required libraries
- Renders the R Markdown report
- Handles output file naming and organization

## Analysis Modules (`R/` directory)

The analysis is organized into modular R scripts sourced by the main report:

- **`data_preparation.R`**: Loads and validates input data (sample sheets, peak files, BAM files)
- **`peak_processing.R`**: Merges peak files across samples, creates consensus peak sets
- **`count_processing.R`**: Generates count matrices from peaks and applies filtering thresholds
- **`analysis.R`**: Performs differential accessibility analysis using edgeR/limma
- **`annotation.R`**: Annotates peaks with nearby genes and genomic features
- **`validation.R`**: Input validation, quality control checks, and error handling
- **`ATACseq_report_funcs.R`**: Utility functions for plotting, formatting, and report generation

## File Structure

```
├── app.R                              # Main Shiny application
├── differential_peak_report.Rmd             # Analysis report template
├── render-report-with-params.sbatch  # SLURM submission script
├── R/                                # Analysis modules
│   ├── data_preparation.R
│   ├── peak_processing.R
│   ├── count_processing.R
│   ├── analysis.R
│   ├── annotation.R
│   ├── validation.R
│   └── ATACseq_report_funcs.R
├── mm10.chrom.sizes                  # Mouse genome chromosome sizes
├── test-input/                       # Example input files
├── output/                           # Generated parameters and reports
└── logs/                            # SLURM job logs
```

## Input Requirements

### Required Files
- **Sample sheet**: CSV with columns `sample`, `group`, and any covariates
- **Peak files**: `.narrowPeak` or `.broadPeak` format (one per sample)
- **BigWig files**: Coverage tracks (one per sample)
- **BAM files**: Aligned reads (one per sample) *OR* existing DDS object

### Optional Files
- **Contrasts file**: Text file specifying comparisons (or enter directly in app)
- **Peak annotation**: Pre-computed peak annotations
- **QC files**: Flagstat summaries and FRIP scores

## Usage Examples

### Manual SLURM Submission
```bash
# After generating params.txt through the app
sbatch render-report-with-params.sbatch \
  --params-file output/my-analysis_params.txt \
  --title "My ATAC-seq Analysis"
```

### Loading Existing Parameters
The app can load existing parameter files, allowing you to:
- Modify previous analyses
- Use templates for similar projects
- Reproduce analyses with updated data

## Output

The analysis generates:
- **HTML Report**: Comprehensive analysis with interactive plots and tables
- **Parameter File**: Record of all analysis settings for reproducibility
- **Log Files**: SLURM job execution logs for troubleshooting

## Requirements

- **R 4.5+** with required packages (see shared library: `/blue/cancercenter-dept/shared/R/library/4.5/`)
- **HiPerGator access** for SLURM job submission
- **Input data** in the required formats

## Troubleshooting

- **Module errors**: The SLURM script initializes the module system automatically
- **File not found**: Ensure all input files are accessible from compute nodes
- **Memory issues**: Increase `--mem` in the SLURM script for large datasets
- **Job logs**: Check `logs/` directory for detailed error messages

## Example Data

The `test-input/` directory contains example files to help understand the required input formats.
```
