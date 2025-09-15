#!/bin/bash
#SBATCH --job-name=atac_report
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=200gb
#SBATCH --time=24:00:00
#SBATCH --output=logs/rserver_%j.log
#SBATCH --error=logs/rserver_%j.error
#SBATCH --account=cancercenter-dept
#SBATCH --qos=cancercenter-dept

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --params-file)
      PARAMS_FILE="$2"
      shift 2
      ;;
    --hipergator-group)
      HIPERGATOR_GROUP="$2"
      shift 2
      ;;
    --output-path)
      OUTPUT_PATH="$2"
      shift 2
      ;;
    --user-email)
      USER_EMAIL="$2"
      shift 2
      ;;
    *)
      echo "Unknown parameter: $1"
      exit 1
      ;;
  esac
done

# Set defaults
HIPERGATOR_GROUP=${HIPERGATOR_GROUP:-"cancercenter-dept"}
OUTPUT_PATH=${OUTPUT_PATH:-"/blue/cancercenter-dept/hkates/Apps/new-atacreportR"}

# Load modules (like your working script)
module purge
module load R/4.5
module load pandoc/2.2.2.1

# Debug: check if pandoc is available
which pandoc
pandoc --version

# Change to app directory
cd /blue/cancercenter-dept/hkates/Apps/new-atacreportR

# Set RSTUDIO_PANDOC to help R find pandoc
export RSTUDIO_PANDOC=$(which pandoc | xargs dirname)

# Create output directory
mkdir -p "$OUTPUT_PATH"

# Render with params file if provided, otherwise use defaults
if [[ -n "$PARAMS_FILE" && -f "$PARAMS_FILE" ]]; then
  echo "Rendering with params file: $PARAMS_FILE"
  echo "Output directory: $OUTPUT_PATH"
  Rscript -e ".libPaths(c('/home/hkates/R/x86_64-pc-linux-gnu-library/4.5', '/usr/local/lib/R/site-library', '/usr/lib/R/site-library', '/usr/lib/R/library')); Sys.setenv(RSTUDIO_PANDOC='$RSTUDIO_PANDOC'); rmarkdown::render('atac_edgeR_report.Rmd', params = yaml::read_yaml('$PARAMS_FILE'), output_dir = '$OUTPUT_PATH')"
else
  echo "Rendering with default params"
  Rscript -e ".libPaths(c('/home/hkates/R/x86_64-pc-linux-gnu-library/4.5', '/usr/local/lib/R/site-library', '/usr/lib/R/site-library', '/usr/lib/R/library')); Sys.setenv(RSTUDIO_PANDOC='$RSTUDIO_PANDOC'); rmarkdown::render('atac_edgeR_report.Rmd')"
fi

echo "Job completed!"
