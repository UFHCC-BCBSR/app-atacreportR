# Rmd-specific helper functions for atacreportR package
# These adapt report_params format to package function signatures

#' Parse file specifications from report parameters
#'
#' Extracts and parses file paths for peaks, BAMs, BigWigs, and QC data
#' from comma-separated parameter strings in report_params format.
parse_file_specifications <- function(report_params) {

  # Helper to parse "sample:path,sample:path" format
  parse_colon_pairs <- function(param_string) {
    if (is.null(param_string) || param_string == "") return(NULL)
    pairs <- strsplit(param_string, ",")[[1]]
    setNames(
      sapply(pairs, function(x) strsplit(x, ":")[[1]][2]),
      sapply(pairs, function(x) strsplit(x, ":")[[1]][1])
    )
  }

  list(
    dds_file = if (is.null(report_params$dds_file) || report_params$dds_file == "") NULL else report_params$dds_file,
    peak_files = parse_colon_pairs(report_params$peak_files),
    bam_files = parse_colon_pairs(report_params$bam_files),
    peak_annotation = if (is.null(report_params$peak_annotation) || report_params$peak_annotation == "") NULL else report_params$peak_annotation,
    bigwig_files = parse_colon_pairs(report_params$bigwig_files),
    qc_files = list(
      flagstat_dir = report_params$qc_flagstat_dir %||% NULL,
      frip_file = report_params$qc_frip_file %||% NULL
    )
  )
}

#' Prepare analysis data from report_params
#'
#' Rmd wrapper that converts report_params format to atacreportR package format
prepare_analysis_data_from_params <- function(report_params) {
  file_specs <- parse_file_specifications(report_params)

  atacreportR::prepare_analysis_data(
    sample_sheet = report_params$sample_sheet,
    organism = report_params$organism,
    output_dir = report_params$output_path %||% report_params[["output-path"]],
    project_id = report_params$seqID %||% "project",
    dds_file = file_specs$dds_file,
    peak_annotation = file_specs$peak_annotation,
    peak_files = file_specs$peak_files,
    bam_files = file_specs$bam_files,
    bigwig_files = file_specs$bigwig_files,
    qc_flagstat_dir = file_specs$qc_files$flagstat_dir,
    qc_frip_file = file_specs$qc_files$frip_file
  )
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || (is.character(x) && x == "")) y else x

#' Parse contrast specifications
#'
#' Parses contrast strings from either a file or comma-separated string.
#'
#' @param x Character string: either a file path or comma-separated contrasts
#'
#' @return List where each element is a character vector of length 2 (group1, group2)
#'
#' @importFrom stringr str_trim str_split
#' @export
parse_contrasts <- function(x) {
  if (file.exists(x)) {
    contrast_lines <- readLines(x, warn = FALSE)
  } else {
    contrast_lines <- unlist(strsplit(x, ","))
  }

  contrast_lines <- trimws(contrast_lines)
  contrast_lines <- contrast_lines[contrast_lines != ""]

  contrast_list <- lapply(contrast_lines, function(line) {
    parts <- strsplit(line, "_vs_")[[1]]
    if (length(parts) != 2) stop(paste("Invalid contrast:", line))
    parts
  })

  return(contrast_list)
}
