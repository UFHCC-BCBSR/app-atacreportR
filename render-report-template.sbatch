#!/bin/bash
#SBATCH --job-name=atac-report
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=200gb
#SBATCH --time=24:00:00
#SBATCH --output=logs/rserver_%j.log
#SBATCH --error=logs/rserver_%j.error
#SBATCH --account=HIPERGATOR_GROUP
#SBATCH --qos=HIPERGATOR_GROUP

module purge
module load R/4.5
module load pandoc/2.2.2.1

# Debug: check if pandoc is available
which pandoc
pandoc --version

cd WORK_DIR

# Create logs directory if it doesn't exist
mkdir -p logs

# Set RSTUDIO_PANDOC to help R find pandoc
export RSTUDIO_PANDOC=$(which pandoc | xargs dirname)

Rscript -e ".libPaths(c('/blue/HIPERGATOR_GROUP/shared/R/library/4.5', '/usr/local/lib/R/site-library', '/usr/lib/R/site-library', '/usr/lib/R/library')); Sys.setenv(RSTUDIO_PANDOC='$RSTUDIO_PANDOC'); rmarkdown::render('atac_edgeR_report.Rmd')"
