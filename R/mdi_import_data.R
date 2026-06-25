#' Import Data into R data.table
#'
#' @description
#' Reads data from various file formats into an R `data.table`. This function is a wrapper around multiple file-reading packages
#' such as `fread` (from `data.table`), `haven` (for Stata, SAS, SPSS), and `readxl` (for Excel). It supports CSV, Stata (.dta), Excel (.xlsx),
#' SAS (.sas7bdat), SPSS (.sav), and tab-delimited text files (.txt). You can also specify a list of columns to import and specify which columns should be imported as characters.
#' However, note that for some file formats (e.g., Stata, SAS, SPSS), the function cannot directly import columns as characters during the import process.
#' In such cases, the specified columns are converted to character types **after** the data has been loaded.
#'
#' @param dir The directory path where the input file is located.
#'
#' @param file The name of the file to be imported.
#'
#' @param format The type of the file to be imported. Supported types include:
#' - `'csv'` for comma-delimited files (direct import as character supported),
#' - `'txt'` for tab-delimited text files (direct import as character supported),
#' - `'gz'` for gzip-compressed delimited files,
#' - `'dta'` for Stata files (post-import conversion to character),
#' - `'xlsx'` for Excel files (post-import conversion to character),
#' - `'sas7bdat'` for SAS files (post-import conversion to character),
#' - `'sav'` for SPSS files (post-import conversion to character),
#' - `'parquet'` for Apache Parquet files (requires the `arrow` package),
#' - `'rdata'` for R workspace files (first object loaded),
#' - `'rds'` for R serialized single-object files.
#'
#' @param col_list A character vector of column names to import. If `NULL`
#'   (default), all columns are imported.
#' @param char_columns A character vector of column names to treat as
#'   character type. For `csv` and `txt`, conversion happens during import;
#'   for all other formats it happens after loading. Default `NULL`.
#' @param encoding Character string passed to the underlying reader (e.g.
#'   `"UTF-8"`, `"Latin-1"`). If `NULL` or empty, a format-specific default
#'   is used. Default `NULL`.
#'
#' @return A `data.table` containing the imported data.
#'
#' @examples
#' tmp_dir <- paste0(tempdir(), "/")
#' write.csv(data.frame(id = 1:3, emp = c(10, 20, 30)),
#'           paste0(tmp_dir, "data.csv"), row.names = FALSE)
#' mdi_import_data(tmp_dir, "data.csv", "csv", char_columns = "id")
#'
#' @export


mdi_import_data <- function(dir, file, format, col_list = NULL, char_columns = NULL,
                        encoding = NULL) {
  check_string(dir,  "dir")
  check_string(file, "file")
  if (!is.null(col_list))     check_char_vec(col_list,     "col_list")
  if (!is.null(char_columns)) check_char_vec(char_columns, "char_columns")

  valid_formats <- c("csv", "txt", "gz", "dta", "xlsx", "sas7bdat", "sav",
                     "parquet", "rdata", "rds")
  format <- tolower(trimws(format))
  check_choice(format, "format", valid_formats)

  if (is.null(encoding) || is.na(encoding) || trimws(encoding) == "") {
    uses_encoding <- FALSE
  } else {
    uses_encoding <- TRUE
  }
  db <- paste0(dir, file)

  fread_with_char_columns <- function(db, select, char_columns, encoding = "UTF-8") {
    data.table::fread(db, select = select,
                      colClasses = list(character = char_columns),
                      encoding = encoding)
  }

  if (format == "csv" || format == "txt" || format == "gz") {
    if (!uses_encoding) encoding <- "unknown"
    # For CSV and TXT, use fread with optional column selection
    if (is.null(col_list)) {
      data <- fread_with_char_columns(db, select = NULL,
                                      char_columns = char_columns,
                                      encoding = encoding)
    } else {
      data <- fread_with_char_columns(db, select = col_list,
                                      char_columns = char_columns,
                                      encoding = encoding)
    }
  } else if (format == "dta") {
    # For Stata files, read full data and then convert char_columns to character if specified
    if (is.null(col_list)) {
      data <- data.table::as.data.table(haven::read_dta(db))[, lapply(.SD, haven::zap_labels)]
    } else {
      data <- data.table::as.data.table(haven::read_dta(db, col_select = col_list))[, lapply(.SD, haven::zap_labels)]
    }
    # If char_columns are provided, convert those columns to character type
    if (!is.null(char_columns)) {
      for (col in char_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.character(data[[col]])
        } else {
          warning(paste("Column", col, "not found in the data"))
        }
      }
    }
  } else if (format == "xlsx") {
    # Read all column names first to match col_list with column numbers
    header    <- readxl::read_excel(db, sheet = 1, n_max = 0)
    col_names <- names(header)

    col_type <- vapply(col_names, function(col) {
      if (!is.null(char_columns) && col %in% char_columns) "text" else "guess"
    }, FUN.VALUE = character(1))

    data <- readxl::read_excel(db, col_types = col_type)
    if (!is.null(col_list)) {
      data <- data.table::as.data.table(data)[, col_list, with = FALSE]
    }
  } else if (format == "sas7bdat") {
    # For SAS files, use col_select if col_list is provided
    if (is.null(col_list)) {
      data <- data.table::as.data.table(haven::read_sas(db))
    } else {
      data <- data.table::as.data.table(haven::read_sas(db, col_select = col_list))
    }
    # If char_columns are provided, convert those columns to character type
    if (!is.null(char_columns)) {
      for (col in char_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.character(data[[col]])
        } else {
          warning(paste("Column", col, "not found in the data"))
        }
      }
    }
  } else if (format == "sav") {
    if (is.null(encoding)) {
      data <- data.table::as.data.table(haven::read_spss(db))
    } else {
      if (!uses_encoding) encoding <- NULL
      data <- data.table::as.data.table(haven::read_sav(db, user_na = TRUE,
                                                         encoding = encoding))
    }
    if (!is.null(col_list)) {
      data <- data[, col_list, with = FALSE]
    }
    # If char_columns are provided, convert those columns to character type
    if (!is.null(char_columns)) {
      for (col in char_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.character(data[[col]])
        } else {
          warning(paste("Column", col, "not found in the data"))
        }
      }
    }
  } else if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE))
      stop("Package 'arrow' is required to read parquet files. Install it with install.packages('arrow').")
    # Read Parquet file with optional column selection
    if (is.null(col_list)) {
      data <- data.table::as.data.table(arrow::read_parquet(db))
    } else {
      data <- data.table::as.data.table(arrow::read_parquet(db, col_select = col_list))
    }
    # Convert specified columns to character if provided
    if (!is.null(char_columns)) {
      for (col in char_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.character(data[[col]])
        } else {
          warning(paste("Column", col, "not found in the data"))
        }
      }
    }
  } else if (format == "rdata") {
    # Create a temporary environment to load the RData file
    tmp_env <- new.env()
    load(db, envir = tmp_env)
    data_name <- ls(tmp_env)[1]
    data <- get(data_name, envir = tmp_env)
    # Convert to data.table if not already
    if (!data.table::is.data.table(data)) {
      data <- data.table::as.data.table(data)
    }
    # Apply column selection if provided
    if (!is.null(col_list)) {
      missing_cols <- setdiff(col_list, names(data))
      if (length(missing_cols) > 0) {
        warning(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")))
      }
      data <- data[, col_list, with = FALSE]
    }
    # Convert specified columns to character type
    if (!is.null(char_columns)) {
      for (col in char_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.character(data[[col]])
        } else {
          warning(paste("Column", col, "not found in the data"))
        }
      }
    }
  } else if (format == "rds") {
    # Read the RDS file
    data <- readRDS(db)
    # Convert to data.table if not already
    if (!data.table::is.data.table(data)) {
      data <- data.table::as.data.table(data)
    }
    # Apply column selection if provided
    if (!is.null(col_list)) {
      missing_cols <- setdiff(col_list, names(data))
      if (length(missing_cols) > 0) {
        warning(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")))
      }
      data <- data[, col_list, with = FALSE]
    }
    # Convert specified columns to character type
    if (!is.null(char_columns)) {
      for (col in char_columns) {
        if (col %in% names(data)) {
          data[[col]] <- as.character(data[[col]])
        } else {
          warning(paste("Column", col, "not found in the data"))
        }
      }
    }
  } else {
    stop(paste0("unsupported format: ", format))
  }
  # Convert any integer64 columns to numeric for compatibility with base R functions
  # (sd, var, quantile, ggplot2 do not support integer64). Values up to 2^53 are
  # represented exactly as double, covering all realistic monetary/trade magnitudes.
  int64_cols <- names(data)[vapply(data, inherits, logical(1L), what = "integer64")]
  if (length(int64_cols) > 0L) {
    data[, (int64_cols) := lapply(.SD, as.numeric), .SDcols = int64_cols]
  }

  return(data)
}