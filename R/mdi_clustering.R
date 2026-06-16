#' Clustering 
#'
#' @description
#' This tool clusters observations using one of the following methods:
#' - k-means;
#' - hierarchical clustering with Ward linkage;
#' - hierarchical clustering with complete linkage;
#' - hierarchical clustering with average linkage;
#' - hierarchical clustering with single linkage;
#' - PAM;
#' - Gaussian mixture;
#' - DBSCAN.
#' The tool can optionally compute:
#' - total within-cluster sum of squares (WSS);
#' - average silhouette width;
#' - bootstrap-style ARI stability;
#' - the WSS or silhouette selection plot used to choose the number of clusters.
#'
#' @param DT A data.table or data.frame with observations to be clustered. Data frames are silently converted to data.table.
#' @param id_vars Character vector with one or more id variables.
#' @param cluster_vars Character vector with the numeric variables used for
#'   clustering.
#' @param method Clustering method. One of `"kmeans"`, `"hc_ward"`,
#'   `"hc_complete"`, `"hc_average"`, `"hc_single"`, `"pam"`, `"mclust"`,
#'   `"dbscan"`.
#' @param k_selection Selection method for k: `"fixed"` or `"automatic"`.
#' @param k_fixed Fixed number of clusters. Used only when
#'   `k_selection = "fixed"`.
#' @param automatic_by_wss Logical. If `TRUE`, automatic k selection is based
#'   on WSS elbow. Default `FALSE`.
#' @param automatic_by_silhouette Logical. If `TRUE`, automatic k selection is
#'   based on average silhouette. Default `FALSE`.
#' @param compute_wss Logical. If `TRUE`, computes final WSS. Default `TRUE`.
#' @param compute_silhouette Logical. If `TRUE`, computes final average
#'   silhouette. Default `TRUE`.
#' @param compute_stability Logical. If `TRUE`, performs bootstrap-style ARI
#'   stability analysis. Default `FALSE`.
#' @param plot_selection Logical. If `TRUE`, plots the WSS or silhouette curve
#'   used to select k. Default `FALSE`.
#' @param k_grid Candidate values of k for automatic selection. Default
#'   `2:25`.
#' @param exclude_noise Logical. Mainly relevant for DBSCAN; if `TRUE`,
#'   observations labelled as noise (cluster 0) are excluded from WSS and
#'   silhouette calculations. Default `TRUE`.
#' @param B_boot Number of bootstrap repetitions. Default `200`.
#' @param nstart Number of random starts for k-means. Default `100`.
#' @param seed Integer seed for reproducibility. Default `123`.
#' @param minPts DBSCAN `minPts` parameter. Default `4`.
#' @param q Quantile used to choose DBSCAN `eps` automatically. Default
#'   `0.95`.
#' @param eps Numeric or `NULL`. DBSCAN radius parameter. If a numeric value
#'   is provided, DBSCAN uses it directly. If `NULL`, `eps` is chosen
#'   automatically from the `q` quantile of k-nearest-neighbour distances
#'   with `k = minPts`.
#' @param G Candidate number of mixture components for mclust. Default `1:10`.
#' @param standardize Logical. If `TRUE`, clustering variables are standardised
#'   via `scale()` before clustering. Recommended for distance-based methods
#'   when variables are on different scales. Default `TRUE`.
#' @param na_action Character. How to handle missing values in `cluster_vars`.
#'   `"stop"` raises an error; `"omit"` removes incomplete rows before
#'   clustering.
#' @param cluster_col Character. Name of the output column for the cluster
#'   assignment. Default `"cluster"`.
#' @param overwrite_cluster_col Logical. If `FALSE`, stops when `cluster_col`
#'   already exists in `DT`. If `TRUE`, the existing column is replaced.
#'   Default `FALSE`.
#' @param bootstrap_reselect_parameters Logical. Only used when
#'   `compute_stability = TRUE`. If `FALSE`, each bootstrap sample uses the
#'   same parameters as the final model. If `TRUE`, automatic parameters are
#'   re-selected within each bootstrap sample. Default `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default
#'   `TRUE`.
#'   
#' @return a list with:
#'   - data: the original input data.table with an additional clustering
#'     column, by default called "cluster".
#'   - chosen_k: final selected number of clusters, when relevant.
#'   - wss: final WSS result, if requested.
#'   - silhouette: final silhouette result, if requested.
#'   - stability: bootstrap-style ARI stability result, if requested.
#'   - selection_plot: recorded plot object, if plot_selection = TRUE.
#' 
#' @examples
#' library(data.table)
#' set.seed(1)
#' DT <- data.table(
#'   firmid = 1:30,
#'   x1     = c(rnorm(15, 0, 0.5), rnorm(15, 5, 0.5)),
#'   x2     = c(rnorm(15, 0, 0.5), rnorm(15, 5, 0.5))
#' )
#' \donttest{
#' result <- mdi_clustering(DT, id_vars = "firmid",
#'                      cluster_vars = c("x1", "x2"),
#'                      method = "kmeans", k_selection = "fixed", k_fixed = 2,
#'                      compute_wss = TRUE, compute_silhouette = TRUE,
#'                      compute_stability = FALSE, verbose = FALSE)
#' }
#' 
#' @export

mdi_clustering <- function(
    DT,
    id_vars,
    cluster_vars,
    method = c(
      "hc_ward",
      "hc_complete",
      "hc_average",
      "hc_single",
      "kmeans",
      "pam",
      "mclust",
      "dbscan"
    ),
    k_selection = c("fixed", "automatic"),
    k_fixed = NULL,
    automatic_by_wss = FALSE,
    automatic_by_silhouette = FALSE,
    compute_wss = TRUE,
    compute_silhouette = TRUE,
    compute_stability = FALSE,
    plot_selection = FALSE,
    k_grid = 2:25,
    exclude_noise = TRUE,
    B_boot = 200,
    nstart = 100,
    seed = 123,
    minPts = 4,
    q = 0.95,
    eps = NULL,
    G = 1:10,
    standardize = TRUE,
    na_action = c("stop", "omit"),
    cluster_col = "cluster",
    overwrite_cluster_col = FALSE,
    bootstrap_reselect_parameters = FALSE,
    verbose = TRUE
) {
  
  ###########################################################################
  # 1. Methodology setting
  ###########################################################################
  
  method <- match.arg(method)
  k_selection <- match.arg(k_selection)
  na_action <- match.arg(na_action)
  
  fixed_k_methods <- c(
    "hc_ward",
    "hc_complete",
    "hc_average",
    "hc_single",
    "kmeans",
    "pam"
  )
  
  automatic_only_methods <- c("mclust", "dbscan")
  
  if (!data.table::is.data.table(DT)) {
    DT <- data.table::as.data.table(DT)
  }

  input_dt <- data.table::copy(DT)
  
  check_char_vec(id_vars,      "id_vars")
  check_char_vec(cluster_vars, "cluster_vars")
  
  missing_id_vars <- setdiff(id_vars, names(input_dt))
  missing_cluster_vars <- setdiff(cluster_vars, names(input_dt))
  
  if (length(missing_id_vars) > 0) {
    stop(
      "The following id_vars are not in DT: ",
      paste(missing_id_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(missing_cluster_vars) > 0) {
    stop(
      "The following cluster_vars are not in DT: ",
      paste(missing_cluster_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (cluster_col %in% names(input_dt) && !isTRUE(overwrite_cluster_col)) {
    stop(
      "The output cluster column '", cluster_col, "' already exists in DT. ",
      "Either rename the existing column, choose another cluster_col, ",
      "or set overwrite_cluster_col = TRUE.",
      call. = FALSE
    )
  }
  
  auto_flags <- c(
    automatic_by_wss = isTRUE(automatic_by_wss),
    automatic_by_silhouette = isTRUE(automatic_by_silhouette)
  )
  
  n_auto_flags <- sum(auto_flags)
  
  if (method %in% automatic_only_methods) {
    if (k_selection != "automatic") {
      stop(
        "Method '", method, "' is treated as automatic only. ",
        "Use k_selection = 'automatic'.",
        call. = FALSE
      )
    }
    
    if (n_auto_flags > 0) {
      stop(
        "For method '", method, "', do not set automatic_by_wss or ",
        "automatic_by_silhouette. These are only used for fixed-k methods.",
        call. = FALSE
      )
    }
    
    selection_criterion <- "none"
  }
  
  if (method %in% fixed_k_methods) {
    if (k_selection == "fixed") {
      if (n_auto_flags > 0) {
        stop(
          "When k_selection = 'fixed', both automatic_by_wss and ",
          "automatic_by_silhouette must be FALSE.",
          call. = FALSE
        )
      }
      
      if (is.null(k_fixed)) {
        stop("When k_selection = 'fixed', k_fixed must be provided.", call. = FALSE)
      }
      
      selection_criterion <- "none"
    }
    
    if (k_selection == "automatic") {
      if (n_auto_flags != 1) {
        stop(
          "When k_selection = 'automatic' for method '", method, "', exactly one of ",
          "automatic_by_wss or automatic_by_silhouette must be TRUE.",
          call. = FALSE
        )
      }
      
      selection_criterion <- if (isTRUE(automatic_by_wss)) {
        "wss"
      } else {
        "silhouette"
      }
    }
  }
  
  if (!is.numeric(k_grid)) {
    stop("k_grid must be numeric.", call. = FALSE)
  }
  
  k_grid <- sort(unique(as.integer(k_grid)))
  
  if (length(k_grid) < 1) {
    stop("k_grid must contain at least one candidate value.", call. = FALSE)
  }
  
  if (!is.numeric(nstart) || length(nstart) != 1 || nstart < 1) {
    stop("nstart must be a positive scalar.", call. = FALSE)
  }
  
  if (!is.numeric(B_boot) || length(B_boot) != 1 || B_boot < 1) {
    stop("B_boot must be a positive scalar.", call. = FALSE)
  }
  
  if (!is.numeric(minPts) || length(minPts) != 1 || minPts < 1) {
    stop("minPts must be a positive scalar.", call. = FALSE)
  }
  
  if (!is.numeric(q) || length(q) != 1 || q <= 0 || q >= 1) {
    stop("q must be a scalar strictly between 0 and 1.", call. = FALSE)
  }
  
  ###########################################################################
  # 2. Auxiliary functions
  ###########################################################################
  
  method_to_linkage <- function(method) {
    switch(
      method,
      hc_ward = "ward.D2",
      hc_complete = "complete",
      hc_average = "average",
      hc_single = "single",
      NULL
    )
  }
  
  count_clusters <- function(labels, exclude_noise = FALSE) {
    labs <- labels
    
    if (isTRUE(exclude_noise)) {
      labs <- labs[labs != 0]
    }
    
    length(unique(labs))
  }

  calc_avg_silhouette <- function(labels, Xmat, exclude_noise = FALSE) {
    if (length(labels) != nrow(Xmat)) {
      stop("length(labels) must equal nrow(Xmat)", call. = FALSE)
    }
    
    if (isTRUE(exclude_noise)) {
      keep <- labels != 0
      labels <- labels[keep]
      Xmat <- Xmat[keep, , drop = FALSE]
    }
    
    if (nrow(Xmat) < 2) {
      return(NA_real_)
    }
    
    labs <- as.integer(factor(labels))
    
    if (length(unique(labs)) < 2) {
      return(NA_real_)
    }
    
    out <- tryCatch(
      {
        d <- stats::dist(Xmat)
        mean(cluster::silhouette(labs, d)[, 3])
      },
      error = function(e) NA_real_
    )
    
    as.numeric(out)
  }
  
  calc_total_wss <- function(labels, Xmat, exclude_noise = FALSE) {
    if (length(labels) != nrow(Xmat)) {
      stop("length(labels) must equal nrow(Xmat)", call. = FALSE)
    }
    
    if (isTRUE(exclude_noise)) {
      keep <- labels != 0
      labels <- labels[keep]
      Xmat <- Xmat[keep, , drop = FALSE]
    }
    
    if (nrow(Xmat) < 1) {
      return(NA_real_)
    }
    
    labs <- as.integer(factor(labels))
    
    if (length(unique(labs)) < 1) {
      return(NA_real_)
    }
    
    total_wss <- 0
    
    for (g in unique(labs)) {
      Xg <- Xmat[labs == g, , drop = FALSE]
      
      if (nrow(Xg) <= 1) {
        next
      }
      
      center <- colMeans(Xg)
      
      center_mat <- matrix(
        center,
        nrow = nrow(Xg),
        ncol = ncol(Xg),
        byrow = TRUE
      )
      
      total_wss <- total_wss + sum(rowSums((Xg - center_mat)^2))
    }
    
    as.numeric(total_wss)
  }
  
  choose_k_by_elbow <- function(wss, k_grid) {
    if (length(wss) != length(k_grid)) {
      stop("wss and k_grid must have the same length.", call. = FALSE)
    }
    
    ok <- is.finite(wss)
    
    if (sum(ok) < 2) {
      stop("At least two finite WSS values are required for elbow selection.", call. = FALSE)
    }
    
    wss_use <- wss[ok]
    k_use <- k_grid[ok]
    
    x1 <- k_use[1]
    y1 <- wss_use[1]
    x2 <- k_use[length(k_use)]
    y2 <- wss_use[length(wss_use)]
    
    distances <- vapply(seq_along(k_use), function(i) {
      x0 <- k_use[i]
      y0 <- wss_use[i]

      num <- abs((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1)
      den <- sqrt((y2 - y1)^2 + (x2 - x1)^2)

      num / den
    }, FUN.VALUE = numeric(1))
    
    distances_full <- rep(NA_real_, length(k_grid))
    distances_full[ok] <- distances
    
    list(
      best_k = k_use[which.max(distances)],
      distances = distances_full
    )
  }
  
  valid_k_grid_for <- function(Xmat) {
    n <- nrow(Xmat)
    
    kg <- sort(unique(as.integer(k_grid)))
    kg <- kg[kg >= 2 & kg <= (n - 1)]
    
    if (length(kg) < 2) {
      stop(
        "Automatic k selection requires at least two candidate k values ",
        "between 2 and nrow(Xmat) - 1.",
        call. = FALSE
      )
    }
    
    kg
  }
  
  choose_eps_dbscan <- function(Xmat, minPts = 4, q = 0.95) {
    kd <- dbscan::kNNdist(Xmat, k = minPts)
    as.numeric(stats::quantile(kd, probs = q, na.rm = TRUE))
  }
  
  choose_k_for_method <- function(Xmat, method, selection_criterion) {
    kg <- valid_k_grid_for(Xmat)
    linkage <- method_to_linkage(method)
    
    if (selection_criterion == "silhouette") {
      if (method %in% c("hc_ward", "hc_complete", "hc_average", "hc_single")) {
        hc <- stats::hclust(stats::dist(Xmat), method = linkage)
        
        sil_values <- vapply(kg, function(k) {
          labs <- stats::cutree(hc, k = k)
          calc_avg_silhouette(labs, Xmat)
        }, FUN.VALUE = numeric(1))

        best_k <- kg[which.max(sil_values)]

        selection_table <- data.table::data.table(
          k = kg,
          criterion = "silhouette",
          value = sil_values,
          selected = kg == best_k
        )

        return(list(
          best_k = best_k,
          selection_table = selection_table,
          hc = hc
        ))
      }

      if (method == "kmeans") {
        set.seed(seed)

        sil_values <- vapply(kg, function(k) {
          km <- stats::kmeans(Xmat, centers = k, nstart = nstart)
          calc_avg_silhouette(km$cluster, Xmat)
        }, FUN.VALUE = numeric(1))

        best_k <- kg[which.max(sil_values)]

        selection_table <- data.table::data.table(
          k = kg,
          criterion = "silhouette",
          value = sil_values,
          selected = kg == best_k
        )

        return(list(
          best_k = best_k,
          selection_table = selection_table,
          hc = NULL
        ))
      }

      if (method == "pam") {
        sil_values <- vapply(kg, function(k) {
          pam_fit <- cluster::pam(Xmat, k = k)
          calc_avg_silhouette(pam_fit$mdi_clustering, Xmat)
        }, FUN.VALUE = numeric(1))
        
        best_k <- kg[which.max(sil_values)]
        
        selection_table <- data.table::data.table(
          k = kg,
          criterion = "silhouette",
          value = sil_values,
          selected = kg == best_k
        )
        
        return(list(
          best_k = best_k,
          selection_table = selection_table,
          hc = NULL
        ))
      }
    }
    
    if (selection_criterion == "wss") {
      if (method %in% c("hc_ward", "hc_complete", "hc_average", "hc_single")) {
        hc <- stats::hclust(stats::dist(Xmat), method = linkage)
        
        wss_values <- vapply(kg, function(k) {
          labs <- stats::cutree(hc, k = k)
          calc_total_wss(labs, Xmat)
        }, FUN.VALUE = numeric(1))

        elbow <- choose_k_by_elbow(wss_values, kg)
        best_k <- elbow$best_k

        selection_table <- data.table::data.table(
          k = kg,
          criterion = "wss",
          value = wss_values,
          elbow_distance = elbow$distances,
          selected = kg == best_k
        )

        return(list(
          best_k = best_k,
          selection_table = selection_table,
          hc = hc
        ))
      }

      if (method == "kmeans") {
        set.seed(seed)

        wss_values <- vapply(kg, function(k) {
          stats::kmeans(Xmat, centers = k, nstart = nstart)$tot.withinss
        }, FUN.VALUE = numeric(1))

        elbow <- choose_k_by_elbow(wss_values, kg)
        best_k <- elbow$best_k

        selection_table <- data.table::data.table(
          k = kg,
          criterion = "wss",
          value = wss_values,
          elbow_distance = elbow$distances,
          selected = kg == best_k
        )

        return(list(
          best_k = best_k,
          selection_table = selection_table,
          hc = NULL
        ))
      }

      if (method == "pam") {
        wss_values <- vapply(kg, function(k) {
          pam_fit <- cluster::pam(Xmat, k = k)
          calc_total_wss(pam_fit$mdi_clustering, Xmat)
        }, FUN.VALUE = numeric(1))
        
        elbow <- choose_k_by_elbow(wss_values, kg)
        best_k <- elbow$best_k
        
        selection_table <- data.table::data.table(
          k = kg,
          criterion = "wss",
          value = wss_values,
          elbow_distance = elbow$distances,
          selected = kg == best_k
        )
        
        return(list(
          best_k = best_k,
          selection_table = selection_table,
          hc = NULL
        ))
      }
    }
    
    stop("Unsupported method or selection criterion.", call. = FALSE)
  }
  
  fit_clustering <- function(Xmat, method, k = NULL, eps_value = NULL) {
    linkage <- method_to_linkage(method)
    
    if (method %in% c("hc_ward", "hc_complete", "hc_average", "hc_single")) {
      hc <- stats::hclust(stats::dist(Xmat), method = linkage)
      labs <- stats::cutree(hc, k = k)
      
      return(list(
        labels = as.integer(labs),
        fit = hc,
        chosen_G = NA_integer_,
        chosen_eps = NA_real_
      ))
    }
    
    if (method == "kmeans") {
      set.seed(seed)
      
      km <- stats::kmeans(
        Xmat,
        centers = k,
        nstart = nstart
      )
      
      return(list(
        labels = as.integer(km$cluster),
        fit = km,
        chosen_G = NA_integer_,
        chosen_eps = NA_real_
      ))
    }
    
    if (method == "pam") {
      pam_fit <- cluster::pam(Xmat, k = k)
      
      return(list(
        labels = as.integer(pam_fit$mdi_clustering),
        fit = pam_fit,
        chosen_G = NA_integer_,
        chosen_eps = NA_real_
      ))
    }
    
    if (method == "mclust") {
      G_valid <- sort(unique(as.integer(G)))
      G_valid <- G_valid[G_valid >= 1 & G_valid <= nrow(Xmat)]
      
      if (length(G_valid) < 1) {
        stop("No valid values of G remain after filtering by sample size.", call. = FALSE)
      }
      
      mc <- mclust::Mclust(Xmat, G = G_valid)
      
      if (is.null(mc) || is.null(mc$classification)) {
        stop("mclust failed to produce a classification.", call. = FALSE)
      }
      
      return(list(
        labels = as.integer(mc$classification),
        fit = mc,
        chosen_G = as.integer(mc$G),
        chosen_eps = NA_real_
      ))
    }
    
    if (method == "dbscan") {
      if (is.null(eps_value)) {
        eps_value <- choose_eps_dbscan(Xmat, minPts = minPts, q = q)
      }
      
      db <- dbscan::dbscan(
        Xmat,
        eps = eps_value,
        minPts = minPts
      )
      
      return(list(
        labels = as.integer(db$cluster),
        fit = db,
        chosen_G = NA_integer_,
        chosen_eps = as.numeric(eps_value)
      ))
    }
    
    stop("Unsupported method.", call. = FALSE)
  }
  
  mode_int <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  bootstrap_ari_stability <- function(Xmat, original_labels, clusterer_fun, B = 200, seed = 123) {
    set.seed(seed)
    
    n <- nrow(Xmat)
    ari_vals <- rep(NA_real_, B)
    
    for (b in seq_len(B)) {
      idx <- sample.int(n, size = n, replace = TRUE)
      Xb <- Xmat[idx, , drop = FALSE]
      
      boot_labels <- tryCatch(
        clusterer_fun(Xb),
        error = function(e) rep(NA_integer_, nrow(Xb))
      )
      
      if (all(is.na(boot_labels))) {
        ari_vals[b] <- NA_real_
        next
      }
      
      tmp <- data.table::data.table(
        orig_id = idx,
        boot_label = boot_labels
      )
      
      tmp2 <- tmp[, list(boot_label = mode_int(.SD[["boot_label"]])), by = "orig_id"]
      
      if (nrow(tmp2) < 2) {
        ari_vals[b] <- NA_real_
        next
      }
      
      ari_vals[b] <- tryCatch(
        {
          mclust::adjustedRandIndex(
            original_labels[tmp2$orig_id],
            tmp2$boot_label
          )
        },
        error = function(e) NA_real_
      )
    }
    
    data.table::data.table(
      mean_ARI = mean(ari_vals, na.rm = TRUE),
      sd_ARI = stats::sd(ari_vals, na.rm = TRUE),
      q05 = as.numeric(stats::quantile(ari_vals, 0.05, na.rm = TRUE)),
      q50 = as.numeric(stats::quantile(ari_vals, 0.50, na.rm = TRUE)),
      q95 = as.numeric(stats::quantile(ari_vals, 0.95, na.rm = TRUE)),
      B = B,
      n_successful_bootstraps = sum(!is.na(ari_vals))
    )
  }
  
  make_bootstrap_clusterer <- function(
    method,
    k_selection,
    selection_criterion,
    chosen_k,
    chosen_eps
  ) {
    function(Xmat) {
      if (method %in% fixed_k_methods) {
        if (
          k_selection == "automatic" &&
          isTRUE(bootstrap_reselect_parameters)
        ) {
          selected <- choose_k_for_method(
            Xmat = Xmat,
            method = method,
            selection_criterion = selection_criterion
          )
          
          k_use <- selected$best_k
        } else {
          k_use <- chosen_k
        }
        
        fit_clustering(
          Xmat = Xmat,
          method = method,
          k = k_use
        )$labels
      } else if (method == "mclust") {
        fit_clustering(
          Xmat = Xmat,
          method = method
        )$labels
      } else if (method == "dbscan") {
        eps_use <- if (isTRUE(bootstrap_reselect_parameters)) {
          NULL
        } else {
          chosen_eps
        }
        
        fit_clustering(
          Xmat = Xmat,
          method = method,
          eps_value = eps_use
        )$labels
      } else {
        stop("Unsupported method in bootstrap clusterer.", call. = FALSE)
      }
    }
  }
  
  ###########################################################################
  # 3. Prepare data for mdi_clustering
  ###########################################################################
  
  keep_cols <- c(id_vars, cluster_vars)
  
  X_dt <- unique(input_dt[, keep_cols, with = FALSE])
  
  missing_cluster_data <- !stats::complete.cases(X_dt[, cluster_vars, with = FALSE])
  
  if (any(missing_cluster_data)) {
    if (na_action == "stop") {
      stop(
        "Missing values found in cluster_vars. ",
        "Use na_action = 'omit' to cluster complete cases only.",
        call. = FALSE
      )
    }
    
    if (na_action == "omit") {
      if (isTRUE(verbose)) {
        message(
          "Omitting ",
          sum(missing_cluster_data),
          " unique ID row(s) with missing mdi_clustering variables."
        )
      }
      
      X_dt <- X_dt[!missing_cluster_data]
    }
  }
  
  duplicate_ids <- X_dt[, .N, by = id_vars]
  duplicate_ids <- duplicate_ids[duplicate_ids[["N"]] > 1L]
  
  if (nrow(duplicate_ids) > 0) {
    stop(
      "There are multiple rows with the same id_vars but different mdi_clustering variables. ",
      "Aggregate the data to one row per ID combination before mdi_clustering.",
      call. = FALSE
    )
  }
  
  not_numeric <- cluster_vars[
    !vapply(X_dt[, cluster_vars, with = FALSE], is.numeric, FUN.VALUE = logical(1))
  ]
  
  if (length(not_numeric) > 0) {
    stop(
      "All cluster_vars must be numeric. Non-numeric variable(s): ",
      paste(not_numeric, collapse = ", "),
      call. = FALSE
    )
  }
  
  X <- as.matrix(X_dt[, cluster_vars, with = FALSE])
  storage.mode(X) <- "double"
  
  if (nrow(X) < 2) {
    stop("At least two complete observations are required for mdi_clustering.", call. = FALSE)
  }
  
  if (isTRUE(standardize)) {
    sds <- apply(X, 2, stats::sd)
    
    zero_sd_vars <- names(sds)[is.na(sds) | sds == 0]
    
    if (length(zero_sd_vars) > 0) {
      stop(
        "The following cluster_vars have zero or undefined variance: ",
        paste(zero_sd_vars, collapse = ", "),
        ". Remove them before mdi_clustering.",
        call. = FALSE
      )
    }
    
    X_scaled <- scale(X)
  } else {
    X_scaled <- X
  }
  
  if (any(!is.finite(X_scaled))) {
    stop("X_scaled contains non-finite values after preprocessing.", call. = FALSE)
  }
  
  rownames(X_scaled) <- do.call(
    paste,
    c(as.list(X_dt[, id_vars, with = FALSE]), sep = "_")
  )
  
  n_obs <- nrow(X_scaled)
  
  if (method %in% fixed_k_methods && k_selection == "fixed") {
    k_fixed <- as.integer(k_fixed)
    
    if (length(k_fixed) != 1 || is.na(k_fixed)) {
      stop("k_fixed must be a single integer.", call. = FALSE)
    }
    
    if (k_fixed < 2 || k_fixed > n_obs) {
      stop(
        "k_fixed must be between 2 and the number of clustered observations.",
        call. = FALSE
      )
    }
  }
  
  ###########################################################################
  # 4. Select k, if required
  ###########################################################################
  
  selection_info <- NULL
  selection_table <- NULL
  chosen_k <- NA_integer_
  
  if (method %in% fixed_k_methods) {
    if (k_selection == "fixed") {
      chosen_k <- k_fixed
    }
    
    if (k_selection == "automatic") {
      selection_info <- choose_k_for_method(
        Xmat = X_scaled,
        method = method,
        selection_criterion = selection_criterion
      )
      
      chosen_k <- selection_info$best_k
      selection_table <- selection_info$selection_table
      
      if (isTRUE(verbose)) {
        message(
          "Selected k = ",
          chosen_k,
          " using ",
          selection_criterion,
          " for method '",
          method,
          "'."
        )
      }
    }
  }
  
  ###########################################################################
  # 5. Plot selection curve, if requested
  ###########################################################################
  
  selection_plot <- NULL
  
  if (
    isTRUE(plot_selection) &&
    k_selection == "automatic" &&
    method %in% fixed_k_methods &&
    !is.null(selection_table)
  ) {
    ylab <- if (selection_criterion == "silhouette") {
      "Average silhouette width"
    } else {
      "Total within-cluster sum of squares (WSS)"
    }
    
    main_title <- if (selection_criterion == "silhouette") {
      paste0("Silhouette-based k selection: ", method)
    } else {
      paste0("WSS elbow-based k selection: ", method)
    }
    
    graphics::plot(
      selection_table$k,
      selection_table$value,
      type = "b",
      pch = 19,
      xlab = "k",
      ylab = ylab,
      main = main_title
    )
    
    graphics::abline(v = chosen_k, lty = 2)
    
    selection_plot <- grDevices::recordPlot()
  }
  
  if (
    isTRUE(plot_selection) &&
    k_selection == "fixed" &&
    isTRUE(verbose)
  ) {
    message("No selection plot was produced because k_selection = 'fixed'.")
  }
  
  if (
    isTRUE(plot_selection) &&
    method %in% automatic_only_methods &&
    isTRUE(verbose)
  ) {
    message(
      "No WSS/silhouette-by-k selection plot was produced for method '",
      method
    )
  }
  
  ###########################################################################
  # 6. Final mdi_clustering
  ###########################################################################
  
  fit_out <- fit_clustering(
    Xmat = X_scaled,
    method = method,
    k = if (method %in% fixed_k_methods) chosen_k else NULL,
    eps_value = eps
  )
  
  final_labels <- fit_out$labels
  chosen_G <- fit_out$chosen_G
  chosen_eps <- fit_out$chosen_eps
  
  if (method == "mclust" && isTRUE(verbose)) {
    message("mclust selected G = ", chosen_G, ".")
  }
  
  if (method == "dbscan" && isTRUE(verbose)) {
    message("DBSCAN used eps = ", round(chosen_eps, 6), " and minPts = ", minPts, ".")
  }
  
  ###########################################################################
  # 7. Merge final mdi_clustering back to original data
  ###########################################################################
  
  assignments <- data.table::copy(X_dt[, id_vars, with = FALSE])
  assignments[, (cluster_col) := final_labels]
  
  out_dt <- data.table::copy(input_dt)
  out_dt[, ("__original_row_order__") := .I]
  
  if (cluster_col %in% names(out_dt)) {
    out_dt[, (cluster_col) := NULL]
  }
  
  out_dt <- merge(
    out_dt,
    assignments,
    by = id_vars,
    all.x = TRUE,
    sort = FALSE
  )
  
  data.table::setorderv(out_dt, "__original_row_order__")
  out_dt[, ("__original_row_order__") := NULL]
  
  ###########################################################################
  # 8. Final WSS and silhouette, if requested
  ###########################################################################
  
  wss_result <- NULL
  
  if (isTRUE(compute_wss)) {
    wss_result <- data.table::data.table(
      method = method,
      k_selection = k_selection,
      selection_criterion = selection_criterion,
      chosen_k = ifelse(is.na(chosen_k), NA_integer_, chosen_k),
      total_wss = calc_total_wss(
        final_labels,
        X_scaled,
        exclude_noise = exclude_noise
      ),
      n_clusters = count_clusters(
        final_labels,
        exclude_noise = exclude_noise
      )
    )
  }
  
  silhouette_result <- NULL
  
  if (isTRUE(compute_silhouette)) {
    silhouette_result <- data.table::data.table(
      method = method,
      k_selection = k_selection,
      selection_criterion = selection_criterion,
      chosen_k = ifelse(is.na(chosen_k), NA_integer_, chosen_k),
      avg_silhouette = calc_avg_silhouette(
        final_labels,
        X_scaled,
        exclude_noise = exclude_noise
      ),
      n_clusters = count_clusters(
        final_labels,
        exclude_noise = exclude_noise
      )
    )
  }
  
  ###########################################################################
  # 9. Bootstrap-style stability, if requested
  ###########################################################################
  
  stability_result <- NULL
  
  if (isTRUE(compute_stability)) {
    clusterer_fun <- make_bootstrap_clusterer(
      method = method,
      k_selection = k_selection,
      selection_criterion = selection_criterion,
      chosen_k = chosen_k,
      chosen_eps = chosen_eps
    )
    
    stability_result <- bootstrap_ari_stability(
      Xmat = X_scaled,
      original_labels = final_labels,
      clusterer_fun = clusterer_fun,
      B = B_boot,
      seed = seed
    )
    
    .stab_new_cols <- c("method", "k_selection", "selection_criterion",
                        "chosen_k", "bootstrap_reselect_parameters")
    stability_result[, (.stab_new_cols) := list(
      method,
      k_selection,
      selection_criterion,
      ifelse(is.na(chosen_k), NA_integer_, chosen_k),
      bootstrap_reselect_parameters
    )]
    
    data.table::setcolorder(
      stability_result,
      c(
        "method",
        "k_selection",
        "selection_criterion",
        "chosen_k",
        "bootstrap_reselect_parameters",
        "mean_ARI",
        "sd_ARI",
        "q05",
        "q50",
        "q95",
        "B",
        "n_successful_bootstraps"
      )
    )
  }
  
  ###########################################################################
  # 10. Return output
  ###########################################################################
  
  output <- list(
    data = out_dt,
    chosen_k = chosen_k,
    wss = wss_result,
    silhouette = silhouette_result,
    stability = stability_result,
    selection_plot = selection_plot
  )
  
  output
}

