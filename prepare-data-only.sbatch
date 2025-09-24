#!/bin/bash
#SBATCH --job-name=atac-prep-data
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=200gb
#SBATCH --time=24:00:00
#SBATCH --output=logs/prep-data_%j.log
#SBATCH --error=logs/prep-data_%j.error
#SBATCH --account=cancercenter-dept
#SBATCH --qos=cancercenter-dept-b

# Usage: sbatch prepare-data-only.sbatch --params-file my-params.txt

# Parse command line arguments
PARAMS_FILE=""

while [[ $# -gt 0 ]]; do
case $1 in
  --params-file)
    PARAMS_FILE="$2"
    shift 2
    ;;
  *)
    echo "Unknown parameter: $1"
    exit 1
    ;;
esac
done

# Validate required arguments
if [[ -z "$PARAMS_FILE" ]]; then
  echo "Error: --params-file is required"
  exit 1
fi

if [[ ! -f "$PARAMS_FILE" ]]; then
  echo "Error: Params file not found: $PARAMS_FILE"
  exit 1
fi

echo "=========================================="
echo "ATAC-seq Data Preparation Only"
echo "=========================================="
echo "Job ID: $SLURM_JOB_ID"
echo "Params file: $PARAMS_FILE"
echo "Start time: $(date)"
echo "=========================================="

# Initialize module system
source /etc/profile.d/modules.sh

# Load modules
module purge
module load R/4.5

# Debug: check if R works
which Rscript

# Get SEQID for output naming
SEQID=$(grep "^--seqID" "$PARAMS_FILE" | cut -d"'" -f2 | head -1 | xargs)
if [[ -z "$SEQID" ]]; then
  SEQID="prep-data"
fi

echo "Project ID: $SEQID"

# Run data preparation using the R script
Rscript R/run_prepare_data_only.R "$PARAMS_FILE"

if [[ $? -eq 0 ]]; then
  OUTPUT_PATH=$(grep "^--output-path\|^--output_path" "$PARAMS_FILE" | cut -d"'" -f2 | head -1 | xargs)
  if [[ -z "$OUTPUT_PATH" ]]; then
    OUTPUT_PATH="."
  fi
  
  echo "=========================================="
  echo "Data preparation completed successfully!"
  echo "Files created:"
  echo "  - ${OUTPUT_PATH}/${SEQID}.dds.RData"
  echo "  - ${OUTPUT_PATH}/${SEQID}.consensus-peaks.txt"
  echo "  - ${OUTPUT_PATH}/${SEQID}.annotated.consensus-peaks.txt"
  echo "End time: $(date)"
  echo "=========================================="
else
  echo "=========================================="
  echo "Data preparation failed!"
  echo "Check error logs for details"
  echo "End time: $(date)"
  echo "=========================================="
  exit 1
fi
