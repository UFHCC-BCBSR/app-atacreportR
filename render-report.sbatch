#!/bin/bash
#SBATCH --job-name=rserver2
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=200gb
#SBATCH --time=24:00:00
#SBATCH --output=logs/rserver_%j.log
#SBATCH --error=logs/rserver_%j.error
#SBATCH --account=cancercenter-dept
#SBATCH --qos=cancercenter-dept

module purge
module load R/4.5
module load pandoc/2.2.2.1

# Debug: check if pandoc is available
which pandoc
pandoc --version

cd /blue/cancercenter-dept/hkates/Apps/new-atacreportR

# Set RSTUDIO_PANDOC to help R find pandoc
export RSTUDIO_PANDOC=$(which pandoc | xargs dirname)

Rscript -e ".libPaths(c('/home/hkates/R/x86_64-pc-linux-gnu-library/4.5', '/usr/local/lib/R/site-library', '/usr/lib/R/site-library', '/usr/lib/R/library')); Sys.setenv(RSTUDIO_PANDOC='$RSTUDIO_PANDOC'); rmarkdown::render('atac_edgeR_report.Rmd')"
