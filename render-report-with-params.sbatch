#!/bin/bash
#SBATCH --job-name=atac-report
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=200gb
#SBATCH --time=24:00:00
#SBATCH --output=logs/peak-report_%j.log
#SBATCH --error=logs/peak-report_%j.error
#SBATCH --account=cancercenter-dept
#SBATCH --qos=cancercenter-dept-b

# Usage: sbatch render-report-with-params.sbatch --params-file my-test-app-params.txt --title "My Custom Report Title" --email user@ufl.edu
# NOTE: Run this script from the directory containing atac_edgeR_report.Rmd

# Parse command line arguments
PARAMS_FILE=""
REPORT_TITLE="ATAC-seq Differential Accessibility Analysis"
OUTPUT_DIR=""
USER_EMAIL=""

while [[ $# -gt 0 ]]; do
case $1 in
  --params-file)
    PARAMS_FILE="$2"
    shift 2
    ;;
  --title)
    REPORT_TITLE="$2"
    shift 2
    ;;
  --output-dir)
    OUTPUT_DIR="$2"
    shift 2
    ;;
  --email)
    USER_EMAIL="$2"
    shift 2
    ;;
  *)
    echo "Unknown parameter: $1"
    exit 1
    ;;
esac
done

# Set email notification if provided
if [[ -n "$USER_EMAIL" ]]; then
  echo "Setting up email notifications to: $USER_EMAIL"
  # Use scontrol to add email notification to the running job
  scontrol update JobId=$SLURM_JOB_ID MailUser=$USER_EMAIL MailType=END,FAIL
fi

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
echo "ATAC-seq Report Generation"
echo "=========================================="
echo "Job ID: $SLURM_JOB_ID"
echo "Params file: $PARAMS_FILE"
echo "Report title: $REPORT_TITLE"
if [[ -n "$USER_EMAIL" ]]; then
  echo "Email notifications: $USER_EMAIL"
fi
echo "Start time: $(date)"
echo "=========================================="

# Initialize module system
source /etc/profile.d/modules.sh

# Load modules
module purge
module load R/4.5
module load pandoc/2.2.2.1

# Debug: check if they work
which Rscript
which pandoc
pandoc --version

# Set RSTUDIO_PANDOC
export RSTUDIO_PANDOC=$(which pandoc | xargs dirname)

# Set output file name
SEQID=$(grep "^--seqID" "$PARAMS_FILE" | cut -d"'" -f2 | xargs)
if [[ -z "$SEQID" ]]; then
  SEQID="ATAC_Report"
fi
OUTPUT_FILE="${SEQID}_Report.html"
if [[ -n "$OUTPUT_DIR" ]]; then
  OUTPUT_FILE="$OUTPUT_DIR/$OUTPUT_FILE"
fi
echo "Output file: $OUTPUT_FILE"

# Render the report with parameters
Rscript -e "
.libPaths(c('/blue/cancercenter-dept/shared/R/library/4.5/', '/usr/local/lib/R/site-library', '/usr/lib/R/site-library', '/usr/lib/R/library'))
Sys.setenv(RSTUDIO_PANDOC='$RSTUDIO_PANDOC')
rmarkdown::render(
  'atac_edgeR_report.Rmd',
  params = list(
    params_file = '$PARAMS_FILE',
    report_title = '$REPORT_TITLE'
  ),
  output_file = '$OUTPUT_FILE'
)
"

if [[ $? -eq 0 ]]; then
  echo "=========================================="
  echo "Report completed successfully!"
  echo "Output: $OUTPUT_FILE"
  echo "End time: $(date)"
  echo "=========================================="
else
  echo "=========================================="
  echo "Report generation failed!"
  echo "Check error logs for details"
  echo "End time: $(date)"
  echo "=========================================="
  exit 1
fi
