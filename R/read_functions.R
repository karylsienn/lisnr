#' Read scans
#'
#' \code{read_receiver_scan_txt} reads the scans saved as text files.
#'
#' @param full_path character vector, a path to the receiver scan file
#' @param join logical (default FALSE), should the header and data be joined?
#' @param names_repair a function to rename the column names (applied to all columns)
#' @param filename_fun a function that returns a tibble with metadata consisted in the filename
#' @param header_columns a character vector, which columns from header to select
#'
#'
#' @return tibble or a list of tibbles
#' @export
#'
#' @details Header and data table are combined by the common `Scan Number`. If \code{join} is
#' \code{TRUE}, the header tibble and data tibble are joined to form a one large tibble with all the metadata.
#' Otherwise (default) the function returns a list with two fields: `header` and `data`. The common columns is
#' the `Scan Number`.
#'
#' @importFrom stringr str_which str_split str_trim str_extract
#' @importFrom tibble tibble as_tibble_row
#' @importFrom dplyr mutate mutate_all bind_rows
#' @importFrom readr parse_number read_tsv
#' @importFrom tidyr separate
#'
#'
read_receiver_scan_txt <- function(full_path, join = FALSE, names_repair, filename_fun = NULL, header_columns) {

  # TODO: implement reading and combining multiple files and filename metadata
  lines <- readLines(full_path)
  header_positions <- str_which(lines, "% \\[Header\\]")
  data_positions <- str_which(lines, "% \\[Data\\]")

  if(sum(diff(data_positions - header_positions)) != 0)
    stop("Header has different number of elements for each data element.")

  # Else continue
  data_starts <- data_positions + 1
  data_ends <- str_which(lines, "[%]{2,}") - 1

  data_tbl <- tibble()
  for(i in seq_along(header_positions)) {
    hpos <- header_positions[i]
    dpos <- data_positions[i]
    header_lines <- lines[(hpos+1):(dpos-2)]
    header_lines <- str_split(header_lines, pattern = "=")

    nms <- str_trim(map_chr(header_lines, ~str_extract(.x[1], "(?<=%\\s)(.*)")))
    vals <- str_trim(map_chr(header_lines, ~.x[2]))
    names(vals) <- nms
    tbl_row <- as_tibble_row(vals)
    if(i == 1) {
      header_tbl <- tbl_row
    } else {
      header_tbl <- bind_rows(header_tbl, tbl_row)
    }

    data_tbl_tmp <- lines[data_starts[i]:data_ends[i]]
    data_tbl_tmp <- read_tsv(I(data_tbl_tmp), col_names = "X")
    data_tbl_tmp <- separate(data_tbl_tmp, X, c("Frequency", "Magnitude"), sep = "\\s+")
    data_tbl_tmp <- mutate_all(data_tbl_tmp, parse_number)
    data_tbl_tmp <- mutate(data_tbl_tmp, `Scan Number` = vals[["Scan Number"]])
    data_tbl <- bind_rows(
      data_tbl,
      data_tbl_tmp
    )
  }

  # Repair the names
  if (!missing(names_repair)) {
    data_tbl <- data_tbl %>% rename_all(list(names_repair))
    header_tbl <- header_tbl %>% rename_all(list(names_repair))
  }


  # If header columns specified, select only those
  if (!missing(header_columns)) {
    header_tbl <- select(header_tbl, header_columns)
  }

  # Join if specified
  if (join) {
    return(left_join(data_tbl, header_tbl)) # Joining by common names --- assumed to be the scan number.
  } else {
    return(list(data = data_tbl, header = header_tbl))
  }

}


#' Read ESL-3
#'
#' \code{read_esl3} reads the scans from ESL-3 EMI Receiver.
#'
#' @export
#'
#' @importFrom readr read_lines read_delim
#' @importFrom purrr map_chr map
#' @importFrom stringr str_c str_extract
#' @importFrom dplyr select rename
#'
#'
read_esl3 <- function(full_path, join = FALSE) {

  lines <- read_lines(full_path)
  poz <- which(str_detect(lines, 'Values;'))

  # Header
  header <- str_split(lines[1:poz],";")
  header_nms <- map_chr(header, ~.[1])
  header_col <- map(header, function(x) {
    y <- x[-1]
    y[nzchar(y)]
  })
  names(header_col) <- header_nms

  # Assumes there is only one table in a file, but if there are multiples
  nrows <- as.integer(str_extract(lines[poz], '[0-9]+'))
  lines_collapsed <- str_c(lines[seq_len(nrows)+poz], collapse = "\n")
  TD <- read_delim(lines_collapsed, delim =  ";", col_names = FALSE)
  TD <- select(TD, -X3)
  TD <- rename(TD, Frequency = X1, Magnitude = X2)

  if(join) {
    warning("Not implemented yet")
    return(TD)
  } else {
    return(list(data = TD, header = header_col))
  }

}

