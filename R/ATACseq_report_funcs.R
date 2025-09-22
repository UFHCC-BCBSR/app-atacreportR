library(readr)
library(tibble)
library(tidyr)
library(stringr)
library(DT)
library(clusterProfiler)
library(dplyr)
library(ggplot2)
library(org.Hs.eg.db)
library(plotly)
library(heatmaply)
library(org.Mm.eg.db)

# Create a function to assign gene symbols to a list of Entrez IDs
assign_gene_symbols <- function(peak_list, contrast_full) {
  # Extract base contrast and direction
  contrast_base <- sub("\\.(up|down)$", "", contrast_full)
  direction <- sub("^.*\\.", "", contrast_full)
  
  # Split the peak list into Entrez IDs
  entrez_ids <- unlist(strsplit(peak_list, "/"))
  
  # Filter de_results_df based on contrast and direction
  de_sub <- de_results_df_filtered %>%
    filter(grepl(contrast_base, Contrast),
           ENTREZID %in% entrez_ids,
           case_when(
             direction == "up" ~ logFC > 0,
             direction == "down" ~ logFC < 0
           ))
  
  # Sort by FDR and get the top 20 peaks
  top_peaks <- de_sub %>%
    arrange(FDR) %>%
    slice_head(n = 20) %>%
    pull(ENTREZID)
  
  top_peaks <- unlist(top_peaks)
  
  # Map Entrez IDs to gene symbols
  gene_symbols <- gene_symbols_vector[top_peaks]
  
  # Return gene symbols as a string (with line breaks)
  if (length(gene_symbols) > 0) {
    paste(unique(gene_symbols), collapse = "<br>")
  } else {
    NA_character_
  }
}

download_button_png <- function(plot_object, output_name = "plot", width = 12, height = 6, dpi = 300) {
  # Step 1: Save the ggplot object as a temporary PNG file
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot = plot_object, width = width, height = height, dpi = dpi)
  
  # Step 2: Convert the image to Base64 encoding
  encoded_img <- base64enc::dataURI(file = temp_file, mime = "image/png")
  
  # Step 3: Create a styled HTML download button
  download_button <- HTML(paste0(
    '<a href="', encoded_img, '" download="', output_name, '.png" ',
    'class="btn btn-primary" style="padding:10px; font-size:16px; text-decoration:none; ',
    'color:white; background-color:#007BFF; border-radius:5px;">',
    '📥 Download ', output_name, '</a>'
  ))
  
  # Return the HTML download button
  return(download_button)
}
summarize_atac_sample_qc <- function(analysis_data, render_table = TRUE) {
  dds <- analysis_data$dds
  samples <- colnames(dds)
  # Get file specifications
  file_specs <- analysis_data$file_specs
  peak_files <- file_specs$peak_files
  qc_data <- analysis_data$qc_data
  
  # Identify extra metadata columns to include
  exclude_cols <- c("sample", "sizeFactor", "fastq_1", "fastq_2", "replicate")
  extra_cols <- setdiff(colnames(dds@colData), exclude_cols)
  
  # === 1. OPEN BASES & PEAK COUNT ===
  if (!is.null(peak_files)) {
    # Use provided peak files
    open_bases_df <- purrr::map_dfr(samples, function(sample) {
      if (sample %in% names(peak_files)) {
        peak_file <- peak_files[[sample]]
        if (file.exists(peak_file)) {
          peaks <- readr::read_tsv(peak_file, col_names = FALSE, col_types = readr::cols_only(
            X1 = readr::col_character(), X2 = readr::col_double(), X3 = readr::col_double()
          ))
          total_bases <- sum(peaks$X3 - peaks$X2)
          peak_count <- nrow(peaks)
          tibble::tibble(Sample = sample, Open_bases = total_bases, Peaks = peak_count)
        } else {
          tibble::tibble(Sample = sample, Open_bases = NA_real_, Peaks = nrow(dds))
        }
      } else {
        tibble::tibble(Sample = sample, Open_bases = NA_real_, Peaks = nrow(dds))
      }
    })
  } else {
    # Fall back to old approach - look for individual peak files in resolved_seq_dir if available
    if (exists("resolved_seq_dir")) {
      open_bases_df <- purrr::map_dfr(samples, function(sample) {
        peak_file <- file.path(resolved_seq_dir, paste0(sample, ".mLb.clN_peaks.broadPeak"))
        if (file.exists(peak_file)) {
          peaks <- readr::read_tsv(peak_file, col_names = FALSE, col_types = readr::cols_only(
            X1 = readr::col_character(), X2 = readr::col_double(), X3 = readr::col_double()
          ))
          total_bases <- sum(peaks$X3 - peaks$X2)
          peak_count <- length(readLines(peak_file))
          tibble::tibble(Sample = sample, Open_bases = total_bases, Peaks = peak_count)
        } else {
          tibble::tibble(Sample = sample, Open_bases = NA_real_, Peaks = nrow(dds))
        }
      })
    } else {
      # No individual peak files available
      open_bases_df <- tibble::tibble(
        Sample = samples,
        Open_bases = NA_real_,
        Peaks = nrow(dds)
      )
    }
  }
  
  # === 2. SIZE FACTORS ===
  factor_df <- tibble::tibble(
    Sample = samples,
    Factor = round(colData(dds)[samples, "sizeFactor", drop = TRUE], 3)
  )  
  
  # === 3. ALIGNED READS ===
  aligned_reads_df <- tibble::tibble(Sample = samples, Aligned_reads = NA_integer_)
  if (!is.null(qc_data$flagstat)) {
    aligned_reads_df <- purrr::map_dfr(samples, function(sample) {
      flagstat_row <- qc_data$flagstat[grepl(sample, qc_data$flagstat$sample), ]
      if (nrow(flagstat_row) > 0) {
        flagstat_path <- flagstat_row$file_path[1]
        if (file.exists(flagstat_path)) {
          flagstat_lines <- readLines(flagstat_path)
          aligned_reads <- flagstat_lines %>%
            stringr::str_subset("mapped \\(") %>%
            stringr::str_extract("^\\d+") %>%
            as.numeric()
          return(tibble::tibble(Sample = sample, Aligned_reads = aligned_reads))
        }
      }
      return(tibble::tibble(Sample = sample, Aligned_reads = NA_integer_))
    })
  }
  
  # === 4. FRIP SCORES ===
  # FIX: Always create frip_df, even if no FRiP data exists
  frip_df <- tibble::tibble(Sample = samples, FRIP = NA_real_)
  
  if (!is.null(qc_data$frip)) {
    # Extract FRIP values - handle column name mismatches
    for (sample in samples) {
      # Try multiple column name formats
      possible_cols <- c(
        sample,                    # Direct match: "04D_REP1"
        paste0("X", sample),       # X prefix: "X04D_REP1"
        gsub("_", ".", sample)     # Dots instead: "04D.REP1"
      )
      # Find which column exists
      matching_col <- intersect(possible_cols, colnames(qc_data$frip))
      if (length(matching_col) > 0) {
        # Find the row where this sample's FRiP value is stored
        sample_row <- which(qc_data$frip$Sample == sample)
        if (length(sample_row) > 0) {
          frip_value <- qc_data$frip[[matching_col[1]]][sample_row[1]]
          if (!is.na(frip_value)) {
            frip_df$FRIP[frip_df$Sample == sample] <- round(frip_value * 100, 1)
          }
        }
      }
    }
  }
  
  # === 5. JOIN ALL ===
  full_qc_df <- factor_df %>%
    dplyr::left_join(aligned_reads_df, by = "Sample") %>%
    dplyr::left_join(open_bases_df, by = "Sample") %>%
    dplyr::left_join(frip_df, by = "Sample") %>%
    dplyr::select(Sample, Aligned_reads, Open_bases, Factor, Peaks, FRIP) %>%
    dplyr::left_join(as_tibble(colData(dds), rownames = "Sample") %>%
                       dplyr::select(Sample, tidyselect::all_of(extra_cols)),
                     by = "Sample")
  
  # Remove any duplicate rows
  full_qc_df <- full_qc_df %>% dplyr::distinct()
  
  # === 6. Optional Render ===
  if (render_table) {
    qc_caption <- "Sample quality control metrics. Some metrics may be unavailable depending on input data."
    return(DT::datatable(
      full_qc_df,
      caption = qc_caption,
      options = list(pageLength = 20, dom = 't'),
      rownames = FALSE
    ))
  }
  
  return(full_qc_df)
}

generate_consensus_peak_summary <- function(consensus_bed_path, contrast_list) {
  library(readr)
  library(dplyr)
  library(DT)
  
  # Read consensus BED file
  consensus_peaks <- read_tsv(
    consensus_bed_path,
    col_names = c("chr", "start", "end", "name", "score", "strand"),
    col_types = cols(.default = col_character())
  ) %>%
    mutate(start = as.numeric(start), end = as.numeric(end)) %>%
    mutate(width = end - start)
  
  # Number of consensus peaks and their average size
  n_peaks <- nrow(consensus_peaks)
  avg_size <- mean(consensus_peaks$width)
  
  # Create the summary for each contrast (same values repeated)
  summary_df <- purrr::map_dfr(contrast_list, function(pair) {
    tibble(
      Test = pair[[1]],
      Ctrl = pair[[2]],
      `Number of peaks` = format(n_peaks, big.mark = ","),
      `Avg size in Test` = paste0(round(avg_size, 1), " bp"),
      `Avg size in Ctrl` = paste0(round(avg_size, 1), " bp")
    )
  })
  
  # Render datatable
  datatable(summary_df,
            caption = "",  # You can add your own caption here
            rownames = FALSE,
            options = list(pageLength = 10, dom = 't'))
}

parse_contrasts <- function(x) {
  # Determine whether `x` is a file path or a string of contrasts
  if (file.exists(x)) {
    contrast_lines <- readLines(x, warn = FALSE)
  } else {
    # Assume comma-separated list
    contrast_lines <- unlist(strsplit(x, ","))
  }
  
  # Clean and split each contrast
  contrast_lines <- trimws(contrast_lines)
  contrast_lines <- contrast_lines[contrast_lines != ""]
  
  contrast_list <- lapply(contrast_lines, function(line) {
    parts <- strsplit(line, "_vs_")[[1]]
    if (length(parts) != 2) stop(paste("Invalid contrast:", line))
    parts
  })
  
  return(contrast_list)
}


# Usage
#contrast_file <- report_params$contrasts
#contrast_list <- parse_contrasts(contrast_file)

convert_all_chr_to_ucsc <- function(results_list) {
  convert_to_ucsc_chr <- function(chr_vec) {
    chr_vec <- gsub("^chr", "", chr_vec)  # strip existing 'chr' if present
    chr_ucsc <- ifelse(chr_vec %in% c("MT", "M"), "chrM", paste0("chr", chr_vec))
    return(chr_ucsc)
  }
  
  for (contrast in names(results_list)) {
    if (!is.null(results_list[[contrast]]$table$Chr)) {
      results_list[[contrast]]$table$Chr <- convert_to_ucsc_chr(results_list[[contrast]]$table$Chr)
    }
  }
  
  return(results_list)
}

# Path to the bash script
build_hub_script <- "build_ucsc_trackhub.sh"

generate_enrichment_plot_atac <- function(gene_lists, de_results_df, universe_entrez, ont_category, significance_threshold = 0.05, top_n = 10, annotation_db) {
  # Load the annotation object
  annotation_obj <- get(annotation_db, envir = asNamespace(annotation_db))
  names(gene_lists) <- gsub("efit_|_results_df","",names(gene_lists))
  
  # Ensure gene lists are named and define contrast order
  if (is.null(names(gene_lists))) stop("Each gene list must be named!")
  contrast_order <- names(gene_lists)
  
  # FIX: Handle both Ensembl and Entrez ID formats
  sample_id <- de_results_df$Entrez.ID[!is.na(de_results_df$Entrez.ID)][1]
  
  if (grepl("^ENSMUSG|^ENSG", sample_id)) {
    # These are Ensembl IDs - need to map to Entrez
    cat("Mapping Ensembl IDs to Entrez IDs...\n")
    de_results_df <- de_results_df %>%
      mutate(ENTREZID = mapIds(
        annotation_obj,
        keys = Entrez.ID,
        column = "ENTREZID",
        keytype = "ENSEMBL",
        multiVals = "first"
      ))
  } else {
    # These are already Entrez IDs - use directly
    cat("Using existing Entrez IDs...\n")
    de_results_df$ENTREZID <- as.character(de_results_df$Entrez.ID)
  }
  
  # Prepare data for compareCluster
  data <- bind_rows(lapply(contrast_order, function(contrast) {
    genes <- gene_lists[[contrast]]
    if (length(genes) == 0 || all(is.na(genes))) {
      return(NULL)
    }
    data.frame(
      Entrez = genes,
      Contrast = contrast,
      stringsAsFactors = FALSE
    )
  }))
  
  if (is.null(data) || nrow(data) == 0) {
    message_plot <- ggplot() +
      annotate("text", x = 1, y = 1, label = paste0("No significant genes found for enrichment\n(", ont_category, ")"), size = 6, hjust = 0.5) +
      theme_void() +
      ggtitle(paste("GO Term Enrichment (", ont_category, ")", sep = ""))
    
    interactive_plot <- ggplotly(message_plot)
    static_plot <- message_plot
    
    return(list(
      interactive_plot = interactive_plot,
      static_plot = static_plot,
      go_results = NULL
    ))
  }
  
  # Ensure no duplicates and valid formatting
  data <- data %>%
    distinct(Entrez, Contrast, .keep_all = TRUE)
  data$Entrez <- as.character(data$Entrez)
  
  # PARALLELIZE: Run GO enrichment with parallel processing
  cat("Running GO enrichment analysis (parallelized)...\n")
  
  # Set up parallel backend
  library(BiocParallel)
  # Hardcode to use exactly 14 workers
  register(MulticoreParam(workers = 14))
  
  formula_res <- compareCluster(
    Entrez ~ Contrast,
    data = data,
    fun = "enrichGO",
    universe = na.omit(universe_entrez),
    OrgDb = annotation_obj,
    keyType = "ENTREZID",
    ont = ont_category,
    pvalueCutoff = significance_threshold  )
  
  # Handle no results case
  if (is.null(formula_res) || nrow(formula_res@compareClusterResult) == 0) {
    formula_res <- new("compareClusterResult",
                       compareClusterResult = data.frame(
                         Cluster = factor(),
                         ID = character(),
                         Description = character(),
                         GeneRatio = character(),
                         BgRatio = character(),
                         pvalue = numeric(),
                         p.adjust = numeric(),
                         qvalue = numeric(),
                         geneID = character(),
                         Count = integer(),
                         stringsAsFactors = FALSE
                       ))
  }
  
  # Ensure clusters are ordered correctly
  formula_res@compareClusterResult$Cluster <- factor(
    formula_res@compareClusterResult$Cluster,
    levels = contrast_order
  )
  
  # Filter by significance threshold
  filtered_results <- subset(
    formula_res@compareClusterResult,
    p.adjust <= significance_threshold
  )
  
  # Handle the special case: no enrichment found
  if (nrow(filtered_results) == 0) {
    message_plot <- ggplot() +
      annotate("text", x = 1, y = 1, label = paste0("No significant GO enrichment found\n(", ont_category, ")"), size = 6, hjust = 0.5) +
      theme_void() +
      ggtitle(paste("GO Term Enrichment (", ont_category, ")", sep = ""))
    interactive_plot <- ggplotly(message_plot)
    static_plot <- message_plot
    return(list(
      interactive_plot = interactive_plot,
      static_plot = static_plot,
      go_results = NULL
    ))
  }
  
  # Step 1: Gather all Entrez IDs from filtered_results
  all_entrez_ids <- unique(unlist(strsplit(filtered_results$geneID, "/")))
  
  # Step 2: Map all Entrez IDs to gene symbols in one go (already efficient)
  gene_symbols_batch <- mapIds(annotation_obj,
                               keys = all_entrez_ids,
                               column = "SYMBOL",
                               keytype = "ENTREZID",
                               multiVals = "first") %>%
    na.omit()
  
  gene_symbols_vector <- gene_symbols_batch[all_entrez_ids]
  
  # PARALLELIZE: Gene symbol assignment
  library(parallel)
  num_cores <- min(6, detectCores() - 2)
  # Create a function to assign gene symbols to a list of Entrez IDs
  assign_gene_symbols <- function(peak_list, contrast_full) {
    # Extract base contrast and direction
    contrast_base <- sub("\\.(up|down)$", "", contrast_full)
    direction <- sub("^.*\\.", "", contrast_full)
    
    # Split the peak list into Entrez IDs
    entrez_ids <- unlist(strsplit(peak_list, "/"))
    
    # Filter de_results_df based on contrast and direction
    de_sub <- de_results_df_filtered %>%
      filter(grepl(contrast_base, Contrast),
             ENTREZID %in% entrez_ids,
             case_when(
               direction == "up" ~ logFC > 0,
               direction == "down" ~ logFC < 0
             ))
    
    # Sort by FDR and get the top 20 peaks
    top_peaks <- de_sub %>%
      arrange(FDR) %>%
      slice_head(n = 20) %>%
      pull(ENTREZID)
    
    top_peaks <- unlist(top_peaks)
    
    # Map Entrez IDs to gene symbols
    gene_symbols <- gene_symbols_vector[top_peaks]
    
    # Return gene symbols as a string (with line breaks)
    if (length(gene_symbols) > 0) {
      paste(unique(gene_symbols), collapse = "<br>")
    } else {
      NA_character_
    }
  }
  filtered_results$GeneSymbols <- mclapply(seq_len(nrow(filtered_results)), function(i) {
    peak_list <- filtered_results$geneID[i]
    contrast_full <- as.character(filtered_results$Cluster[i])
    assign_gene_symbols(peak_list, contrast_full)
  }, mc.cores = num_cores)
  
  # Convert list to character vector
  filtered_results$GeneSymbols <- unlist(filtered_results$GeneSymbols)
  
  # Save filtered GO results for download
  download_go_results <- filtered_results %>%
    select(Cluster, Description, p.adjust, GeneSymbols, everything())
  # Identify top `n` GO terms for plotting
  top_GO_terms <- filtered_results %>%
    group_by(Cluster) %>%
    arrange(p.adjust, .by_group = TRUE) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    pull(Description) %>%
    unique()
  
  # Filter for plotting (only top GO terms)
  formula_res@compareClusterResult <- filtered_results %>%
    filter(Description %in% top_GO_terms)
  
  # Convert GeneRatio to numeric
  formula_res@compareClusterResult <- formula_res@compareClusterResult %>%
    mutate(GeneRatio = sapply(strsplit(as.character(GeneRatio), "/"), function(x) as.numeric(x[1]) / as.numeric(x[2])))
  
  # Reorder GO terms using hierarchical clustering (same as before)
  reorder_GO_terms <- function(df) {
    term_matrix <- table(df$Description, df$Cluster)  
    
    if (nrow(term_matrix) > 1 && length(unique(df$Description)) > 1) {
      term_dist <- dist(term_matrix, method = "binary")  
      term_hclust <- hclust(term_dist, method = "ward.D2")  
      term_order <- rownames(term_matrix)[term_hclust$order]
    } else {
      term_order <- unique(df$Description)  # Ensure a valid factor level
    }
    
    df$Description <- factor(df$Description, levels = term_order)  
    return(df)
  }
  
  formula_res@compareClusterResult <- reorder_GO_terms(formula_res@compareClusterResult)
  
  # Define GeneRatio bins and size mapping
  bin_breaks <- c(0, 0.01, 0.05, 0.10, max(formula_res@compareClusterResult$GeneRatio, na.rm = TRUE) + 0.01)  
  bin_labels <- c("≤0.01", "0.01 - 0.05", "0.05 - 0.10", "≥0.10")
  
  formula_res@compareClusterResult <- formula_res@compareClusterResult %>%
    mutate(GeneRatioCategory = cut(GeneRatio, breaks = bin_breaks, labels = bin_labels, include.lowest = TRUE, right = FALSE))
  
  size_mapping <- c("≤0.01" = 2, "0.01 - 0.05" = 4, "0.05 - 0.10" = 6, "≥0.10" = 8)
  
  # Correct the mutate statement
  formula_res@compareClusterResult <- formula_res@compareClusterResult %>%
    mutate(
      p.adjust = as.numeric(as.character(p.adjust)),
      GeneRatio = as.numeric(as.character(GeneRatio)),
      plot_label = ifelse(
        sapply(strsplit(as.character(Description), " "), length) > 6,
        sapply(strsplit(as.character(Description), " "), function(words) {
          paste(c(words[1:3], "...", tail(words, 3)), collapse = " ")
        }),
        as.character(Description)
      )
    ) %>%
    mutate(
      tooltip_text = paste(
        "Cluster: ", as.character(Cluster), "<br>",  
        "GO Term: ", as.character(Description), "<br>",  
        "p.adjust: ", signif(p.adjust, 3), "<br>",
        "GeneRatio: ", signif(GeneRatio, 3), "<br>",
        "Top Genes:<br>", as.character(GeneSymbols)  
      )
    )
  
  # Create interactive plot
  p <- ggplot(formula_res@compareClusterResult, aes(
    x = Cluster, 
    y = plot_label, 
    size = GeneRatioCategory,  
    color = p.adjust,
    text = tooltip_text  
  )) +
    geom_point(alpha = 0.8) +  
    scale_size_manual(name = "Gene Ratio", values = size_mapping) +  
    scale_color_gradient(low = "red", high = "blue", 
                         limits = c(min(formula_res@compareClusterResult$p.adjust, na.rm = TRUE), 
                                    max(formula_res@compareClusterResult$p.adjust, na.rm = TRUE)),
                         name = "p.adjust") +  
    guides(color = guide_colorbar(title = "p.adjust")) +  
    ggtitle(paste("GO Term Enrichment (", ont_category, ")", sep = "")) +
    xlab("DE Gene list") +
    ylab("GO Term") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.title.x = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  # **Convert to interactive plot (ensure tooltips work)**
  interactive_plot <- ggplotly(p, tooltip = "text") %>%  
    layout(legend = list(title = list(text = "Gene Ratio")))
  
  # **Static High-Resolution Plot (for manuscript)**
  static_plot <- dotplot(formula_res, showCategory = top_n) +
    ggtitle(paste("GO Term Enrichment (", ont_category, ")", sep = "")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.title.x = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  # Return interactive plot, static plot, and GO results
  return(list(
    interactive_plot = interactive_plot, 
    static_plot = static_plot, 
    go_results = download_go_results
  ))
}

generate_kegg_enrichment_plot_atac <- function(gene_lists, de_results_df, universe_entrez, significance_threshold = 0.05, top_n = 10, annotation_db) {
  # Load the annotation object
  annotation_obj <- get(annotation_db, envir = asNamespace(annotation_db))
  names(gene_lists) <- gsub("efit_|_results_df","",names(gene_lists))
  
  # Ensure gene lists are named and define contrast order
  if (is.null(names(gene_lists))) stop("Each gene list must be named!")
  contrast_order <- names(gene_lists)
  
  # FIX: Handle both Ensembl and Entrez ID formats
  sample_id <- de_results_df$Entrez.ID[!is.na(de_results_df$Entrez.ID)][1]
  if (grepl("^ENSMUSG|^ENSG", sample_id)) {
    # These are Ensembl IDs - need to map to Entrez
    cat("Mapping Ensembl IDs to Entrez IDs...\n")
    de_results_df <- de_results_df %>%
      mutate(ENTREZID = mapIds(
        annotation_obj,
        keys = Entrez.ID,
        column = "ENTREZID",
        keytype = "ENSEMBL",
        multiVals = "first"
      ))
  } else {
    # These are already Entrez IDs - use directly
    cat("Using existing Entrez IDs...\n")
    de_results_df$ENTREZID <- as.character(de_results_df$Entrez.ID)
  }
  
  # Prepare data for compareCluster
  data <- bind_rows(lapply(contrast_order, function(contrast) {
    genes <- gene_lists[[contrast]]
    if (length(genes) == 0 || all(is.na(genes))) {
      return(NULL)
    }
    data.frame(
      Entrez = genes,
      Contrast = contrast,
      stringsAsFactors = FALSE
    )
  }))
  
  # Ensure no duplicates and valid formatting
  data <- data %>%
    distinct(Entrez, Contrast, .keep_all = TRUE)
  data$Entrez <- as.character(data$Entrez)
  
  # PARALLELIZE: Run KEGG enrichment with parallel processing
  cat("Running KEGG enrichment analysis (parallelized)...\n")
  # Set up parallel backend
  library(BiocParallel)
  # Hardcode to use exactly 14 workers
  register(MulticoreParam(workers = 14))
  
  kegg_res <- compareCluster(
    Entrez ~ Contrast,
    data = data,
    fun = "enrichKEGG",
    universe = na.omit(universe_entrez),
    organism = "mmu",  # Assuming human (Homo sapiens) for KEGG enrichment
    keyType = "ncbi-geneid",
    pvalueCutoff = significance_threshold
  )
  
  # Handle no results case
  if (is.null(kegg_res) || nrow(kegg_res@compareClusterResult) == 0) {
    kegg_res <- new("compareClusterResult",
                    compareClusterResult = data.frame(
                      Cluster = factor(),
                      ID = character(),
                      Description = character(),
                      GeneRatio = character(),
                      BgRatio = character(),
                      pvalue = numeric(),
                      p.adjust = numeric(),
                      qvalue = numeric(),
                      geneID = character(),
                      Count = integer(),
                      stringsAsFactors = FALSE
                    ))
  }
  
  # Ensure clusters are ordered correctly
  kegg_res@compareClusterResult$Cluster <- factor(
    kegg_res@compareClusterResult$Cluster,
    levels = contrast_order
  )
  
  # Filter by significance threshold
  filtered_results <- subset(
    kegg_res@compareClusterResult,
    p.adjust <= significance_threshold
  )
  
  # Handle the special case: no enrichment found
  if (nrow(filtered_results) == 0) {
    message_plot <- ggplot() +
      annotate("text", x = 1, y = 1, label = "No significant KEGG enrichment found", size = 6, hjust = 0.5) +
      theme_void() +
      ggtitle("KEGG Pathway Enrichment")
    
    interactive_plot <- ggplotly(message_plot)
    static_plot <- message_plot
    
    return(list(
      interactive_plot = interactive_plot,
      static_plot = static_plot,
      kegg_results = NULL
    ))
  }
  
  # Step 1: Gather all Entrez IDs from filtered_results
  all_entrez_ids <- unique(unlist(strsplit(filtered_results$geneID, "/")))
  
  # Step 2: Map all Entrez IDs to gene symbols in one go (already efficient)
  gene_symbols_batch <- mapIds(annotation_obj,
                               keys = all_entrez_ids,
                               column = "SYMBOL",
                               keytype = "ENTREZID",
                               multiVals = "first") %>%
    na.omit()
  
  gene_symbols_vector <- gene_symbols_batch[all_entrez_ids]
  
  # Pre-filter de_results_df once for relevant contrasts
  de_results_df_filtered <- de_results_df %>%
    filter(!is.na(ENTREZID) & ENTREZID != "")
  
  # Create a function to assign gene symbols to a list of Entrez IDs
  assign_gene_symbols <- function(peak_list, contrast_full) {
    # Extract base contrast and direction
    contrast_base <- sub("\\.(up|down)$", "", contrast_full)
    direction <- sub("^.*\\.", "", contrast_full)
    
    # Split the peak list into Entrez IDs
    entrez_ids <- unlist(strsplit(peak_list, "/"))
    
    # Filter de_results_df based on contrast and direction
    de_sub <- de_results_df_filtered %>%
      filter(grepl(contrast_base, Contrast),
             ENTREZID %in% entrez_ids,
             case_when(
               direction == "up" ~ logFC > 0,
               direction == "down" ~ logFC < 0
             ))
    
    # Sort by FDR and get the top 20 peaks
    top_peaks <- de_sub %>%
      arrange(FDR) %>%
      slice_head(n = 20) %>%
      pull(ENTREZID)
    
    top_peaks <- unlist(top_peaks)
    
    # Map Entrez IDs to gene symbols
    gene_symbols <- gene_symbols_vector[top_peaks]
    
    # Return gene symbols as a string (with line breaks)
    if (length(gene_symbols) > 0) {
      paste(unique(gene_symbols), collapse = "<br>")
    } else {
      NA_character_
    }
  }
  
  # PARALLELIZE: Gene symbol assignment
  library(parallel)
  num_cores <- min(6, detectCores() - 2)
  filtered_results$GeneSymbols <- mclapply(seq_len(nrow(filtered_results)), function(i) {
    peak_list <- filtered_results$geneID[i]
    contrast_full <- as.character(filtered_results$Cluster[i])
    assign_gene_symbols(peak_list, contrast_full)
  }, mc.cores = num_cores)
  
  # Convert list to character vector
  filtered_results$GeneSymbols <- unlist(filtered_results$GeneSymbols)
  
  # Save filtered KEGG results for download
  download_kegg_results <- filtered_results %>%
    select(Cluster, Description, p.adjust, GeneSymbols, everything())
  
  # Identify top `n` KEGG pathways for plotting
  top_KEGG_terms <- filtered_results %>%
    group_by(Cluster) %>%
    arrange(p.adjust, .by_group = TRUE) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    pull(Description) %>%
    unique()
  
  # Filter for plotting (only top KEGG pathways)
  kegg_res@compareClusterResult <- filtered_results %>%
    filter(Description %in% top_KEGG_terms)
  
  # Convert GeneRatio to numeric
  kegg_res@compareClusterResult <- kegg_res@compareClusterResult %>%
    mutate(GeneRatio = sapply(strsplit(as.character(GeneRatio), "/"), function(x) as.numeric(x[1]) / as.numeric(x[2])))
  
  # Reorder KEGG terms using hierarchical clustering (same as before)
  reorder_KEGG_terms <- function(df) {
    term_matrix <- table(df$Description, df$Cluster)  
    if (nrow(term_matrix) > 1 && length(unique(df$Description)) > 1) {
      term_dist <- dist(term_matrix, method = "binary")  
      term_hclust <- hclust(term_dist, method = "ward.D2")  
      term_order <- rownames(term_matrix)[term_hclust$order]
    } else {
      term_order <- unique(df$Description)  # Ensure a valid factor level
    }
    df$Description <- factor(df$Description, levels = term_order)  
    return(df)
  }
  
  kegg_res@compareClusterResult <- reorder_KEGG_terms(kegg_res@compareClusterResult)
  
  # Define GeneRatio bins and size mapping
  bin_breaks <- c(0, 0.01, 0.05, 0.10, max(kegg_res@compareClusterResult$GeneRatio, na.rm = TRUE) + 0.01)  
  bin_labels <- c("≤0.01", "0.01 - 0.05", "0.05 - 0.10", "≥0.10")
  
  kegg_res@compareClusterResult <- kegg_res@compareClusterResult %>%
    mutate(GeneRatioCategory = cut(GeneRatio, breaks = bin_breaks, labels = bin_labels, include.lowest = TRUE, right = FALSE))
  
  size_mapping <- c("≤0.01" = 2, "0.01 - 0.05" = 4, "0.05 - 0.10" = 6, "≥0.10" = 8)
  
  # Correct the mutate statement
  kegg_res@compareClusterResult <- kegg_res@compareClusterResult %>%
    mutate(
      p.adjust = as.numeric(as.character(p.adjust)),
      GeneRatio = as.numeric(as.character(GeneRatio)),
      plot_label = ifelse(
        sapply(strsplit(as.character(Description), " "), length) > 6,
        sapply(strsplit(as.character(Description), " "), function(words) {
          paste(c(words[1:3], "...", tail(words, 3)), collapse = " ")
        }),
        as.character(Description)
      )
    ) %>%
    mutate(
      tooltip_text = paste(
        "Cluster: ", as.character(Cluster), "<br>",  
        "KEGG Pathway: ", as.character(Description), "<br>",  
        "p.adjust: ", signif(p.adjust, 3), "<br>",
        "GeneRatio: ", signif(GeneRatio, 3), "<br>",
        "Top Genes:<br>", as.character(GeneSymbols)  
      )
    )
  
  # Create interactive plot
  p <- ggplot(kegg_res@compareClusterResult, aes(
    x = Cluster,
    y = plot_label,
    size = GeneRatioCategory,  
    color = p.adjust,
    text = tooltip_text  
  )) +
    geom_point(alpha = 0.8) +  
    scale_size_manual(name = "Gene Ratio", values = size_mapping) +  
    scale_color_gradient(low = "red", high = "blue",
                         limits = c(min(kegg_res@compareClusterResult$p.adjust, na.rm = TRUE),
                                    max(kegg_res@compareClusterResult$p.adjust, na.rm = TRUE)),
                         name = "p.adjust") +  
    guides(color = guide_colorbar(title = "p.adjust")) +  
    ggtitle("KEGG Pathway Enrichment") +
    xlab("DE Gene list") +
    ylab("KEGG Pathway") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.title.x = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  # **Convert to interactive plot (ensure tooltips work)**
  interactive_plot <- ggplotly(p, tooltip = "text") %>%  
    layout(legend = list(title = list(text = "Gene Ratio")))
  
  # **Static High-Resolution Plot (for manuscript)**
  static_plot <- dotplot(kegg_res, showCategory = top_n) +
    ggtitle("KEGG Pathway Enrichment") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.title.x = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  # Return interactive plot, static plot, and KEGG results
  return(list(
    interactive_plot = interactive_plot,
    static_plot = static_plot,
    kegg_results = download_kegg_results
  ))
}

write_bigbed_de_peaks <- function(results_list, report_params, chrom_sizes_file, bedToBigBed_path) {
  public_web_dir <- "/orange/cancercenter-dept/web/public/BCB-SR/trackhubs"
  
  # Function to find column names flexibly
  find_column <- function(df, possible_names) {
    for (name in possible_names) {
      if (name %in% colnames(df)) return(name)
    }
    return(NULL)
  }
  
  for (contrast in names(results_list)) {
    contrast_clean <- gsub("\\.", "-", contrast)
    contrast_clean <- gsub("_", "-", contrast_clean)
    contrast_clean <- gsub("-vs-", "_vs_", contrast_clean)
    
    # Extract group names from contrast
    group_names <- strsplit(contrast, "_vs_")[[1]]
    group1 <- group_names[1]
    group2 <- if(length(group_names) > 1) group_names[2] else "baseline"
    
    trackhub_dir <- file.path(public_web_dir, report_params$seqID, contrast_clean)
    if (!dir.exists(trackhub_dir)) dir.create(trackhub_dir, recursive = TRUE)
    
    result_table <- results_list[[contrast]]$table
    de_table <- subset(result_table, FDR < 0.05 & abs(logFC) > 1 & logCPM > 2)
    
    # Flexible column name detection
    chr_col <- find_column(de_table, c("seqnames", "Chr", "chromosome", "chrom"))
    start_col <- find_column(de_table, c("start", "Start", "chromStart"))
    end_col <- find_column(de_table, c("end", "End", "chromEnd"))
    strand_col <- find_column(de_table, c("strand", "Strand"))
    gene_col <- find_column(de_table, c("Gene.Name", "gene_name", "GeneName", "symbol"))
    interval_col <- find_column(de_table, c("interval", "peak_id", "PeakID", "name"))
    
    # Debug: Report found columns
    cat("Column mapping for", contrast, ":\n")
    cat("  Chromosome:", chr_col, "\n")
    cat("  Start:", start_col, "\n") 
    cat("  End:", end_col, "\n")
    cat("  Strand:", strand_col, "\n")
    cat("  Gene name:", gene_col, "\n")
    cat("  Interval/Peak ID:", interval_col, "\n")
    
    # Check required columns exist
    if (is.null(chr_col) || is.null(start_col) || is.null(end_col)) {
      cat("ERROR: Missing required coordinate columns for", contrast, "\n")
      next
    }
    
    # Read valid UCSC chromosome names  
    valid_chroms <- read.table(chrom_sizes_file, header = FALSE, stringsAsFactors = FALSE)[[1]]
    de_table <- de_table[de_table[[chr_col]] %in% valid_chroms, ]
    
    if (nrow(de_table) == 0) {
      cat("No valid peaks after chromosome filtering for", contrast, "\n")
      next
    }
    
    cat("Number of DE peaks:", nrow(de_table), "\n")
    
    # Create base names with fallback hierarchy
    if (!is.null(gene_col)) {
      base_names <- ifelse(
        !is.na(de_table[[gene_col]]) & de_table[[gene_col]] != "",
        de_table[[gene_col]],
        if (!is.null(interval_col)) {
          de_table[[interval_col]]
        } else {
          paste0(de_table[[chr_col]], ":", de_table[[start_col]], "-", de_table[[end_col]])
        }
      )
    } else if (!is.null(interval_col)) {
      base_names <- de_table[[interval_col]]
    } else {
      base_names <- paste0(de_table[[chr_col]], ":", de_table[[start_col]], "-", de_table[[end_col]])
    }
    
    # Add direction annotation
    direction_suffix <- ifelse(
      de_table$logFC > 0,
      paste0("_UP_IN_", toupper(group1)),
      paste0("_UP_IN_", toupper(group2))
    )
    
    annotated_names <- paste0(base_names, direction_suffix)
    
    # Debug output
    cat("Sample names:\n")
    sample_idx <- 1:min(3, length(base_names))
    for (i in sample_idx) {
      cat(sprintf("  %s (logFC=%.2f) -> %s\n", 
                  base_names[i], de_table$logFC[i], annotated_names[i]))
    }
    cat("\n")
    
    # Construct BED dataframe using flexible column names
    bed_df <- data.frame(
      chrom      = de_table[[chr_col]],
      chromStart = de_table[[start_col]] - 1,  # BED is 0-based
      chromEnd   = de_table[[end_col]],
      name       = annotated_names,
      score      = 1000,  # Simplified for now
      strand     = if (!is.null(strand_col)) de_table[[strand_col]] else ".",
      stringsAsFactors = FALSE
    )
    
    bed_file <- file.path(trackhub_dir, paste0(contrast_clean, "_DA_peaks_directional.bed"))
    write.table(bed_df, bed_file, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    # Sort BED
    sort_cmd <- sprintf("LC_COLLATE=C sort -k1,1 -k2,2n %s -o %s", bed_file, bed_file)
    system(sort_cmd)
    
    # Convert to BigBed
    bb_file <- file.path(trackhub_dir, paste0(contrast_clean, "_DA_peaks_directional.bb"))
    cmd <- sprintf("%s %s %s %s", bedToBigBed_path, bed_file, chrom_sizes_file, bb_file)
    system(cmd, intern = TRUE)
    
    file.remove(bed_file)
  }
}

find_variable_for_contrast <- function(group1, group2, metadata) {
  for (var in colnames(metadata)) {
    vals <- make.names(unique(as.character(metadata[[var]])))
    if (all(make.names(c(group1, group2)) %in% vals)) return(var)
  }
  stop("No matching variable found for contrast.")
}

# Read the report parameters from the text file
parse_params <- function(filepath) {
  lines <- readLines(filepath)
  param_list <- list()
  for (line in lines) {
    if (grepl("^--", line)) {
      parts <- strsplit(line, " ", fixed = TRUE)[[1]]
      key <- gsub("^--", "", parts[1])  # Remove leading --
      value <- paste(parts[-1], collapse = " ")
      value <- gsub("^['\"]|['\"]$", "", value)  # Remove surrounding quotes (both single and double)
      # Convert to numeric if it looks like a number
      if (!is.na(suppressWarnings(as.numeric(value)))) {
        value <- as.numeric(value)
      }
      param_list[[key]] <- value
    }
  }
  return(param_list)
}

get_log_matrix <- function(dge) {
  if (!is.null(dge$E_corrected)) {
    return(dge$E_corrected)
  } else if (!is.null(dge$counts)) {
    return(cpm(dge, log = TRUE))
  } else {
    stop("Input DGE object must contain either 'counts' or 'E_corrected'.")
  }
}

library(plotly)
library(RColorBrewer)
library(car)
plot_pca <- function(dge, title, show_legend = TRUE) {
  # Extract log2 CPM values
  logcpm <- cpm(dge, log = TRUE)
  
  # Check for and remove genes with non-finite values
  finite_genes <- apply(logcpm, 1, function(x) all(is.finite(x)))
  logcpm_clean <- logcpm[finite_genes, ]
  
  # Perform PCA
  pca_res <- prcomp(t(logcpm_clean), center = TRUE, scale. = FALSE)
  
  # Get PC scores and variance explained
  scores <- as.data.frame(pca_res$x[, 1:2])
  percentVar <- round(100 * pca_res$sdev^2 / sum(pca_res$sdev^2), 1)
  
  # Get sample info
  samples_df <- as.data.frame(dge$samples)
  scores$Sample <- rownames(scores)
  scores$Group <- samples_df$Condition
  
  # Create base plot
  fig <- plot_ly()
  
  # Add ellipses for each group (modified for small sample sizes)
  for (group in unique(scores$Group)) {
    group_data <- scores[scores$Group == group, ]
    if (nrow(group_data) >= 2) {  # Changed from 3 to 2
      # Check if coordinates are finite before creating ellipse
      if (all(is.finite(group_data$PC1)) && all(is.finite(group_data$PC2))) {
        tryCatch({
          if (nrow(group_data) >= 3) {
            # Use dataEllipse for 3+ points
            ellipse_coords <- car::dataEllipse(group_data$PC1, group_data$PC2,
                                               levels = 0.68, plot.points = FALSE, draw = FALSE)
          } else {
            # For 2 points, create a simple hull around the points
            hull_indices <- chull(group_data$PC1, group_data$PC2)
            ellipse_coords <- as.matrix(group_data[hull_indices, c("PC1", "PC2")])
            # Expand slightly to make it visible
            center_x <- mean(ellipse_coords[,1])
            center_y <- mean(ellipse_coords[,2])
            ellipse_coords[,1] <- center_x + (ellipse_coords[,1] - center_x) * 1.5
            ellipse_coords[,2] <- center_y + (ellipse_coords[,2] - center_y) * 1.5
          }
          
          fig <- fig %>%
            add_polygons(x = ellipse_coords[,1], y = ellipse_coords[,2],
                         name = paste(group, "CI"),
                         opacity = 0.2,
                         showlegend = FALSE,
                         hoverinfo = "skip")
        }, error = function(e) {
          message("Skipping ellipse for group ", group, ": ", e$message)
        })
      }
    }
  }
  
  # Add points
  fig <- fig %>%
    add_trace(
      data = scores,
      x = ~PC1, y = ~PC2,
      color = ~Group,
      text = ~Sample,
      type = 'scatter', mode = 'markers',
      marker = list(size = 10),
      hovertemplate = "%{text}<br>PC1: %{x:.2f}<br>PC2: %{y:.2f}<extra></extra>",
      showlegend = show_legend
    ) %>%
    layout(
      xaxis = list(title = paste0("PC1 (", percentVar[1], "%)")),
      yaxis = list(title = paste0("PC2 (", percentVar[2], "%)")),
      width = 500,
      height = 400
    )
  
  return(list(plot = fig, title = title))  # Return both plot and title
}
