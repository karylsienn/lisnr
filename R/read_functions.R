#' Read scans
#'
#' @param full_path a path to the receiver scan file
#'
#' @return list of tibbles, one for metadata and one for the data
#' @export
#'
#' @importFrom stringr str_which str_split str_trim str_extract
#' @importFrom tibble tibble bind_rows as_tibble_row
#' @importFrom dplyr mutate mutate_all
#' @importFrom readr parse_number read_tsv
#' @importFrom tidyr separate
#'
#' @examples
read_receiver_scan_txt <- function(full_path) {

  lines <- readLines(full_path)
  header_positions <- str_which(lines,"% \\[Header\\]")
  data_positions <- str_which(lines, "% \\[Data\\]")


  if(sum(diff(data_positions - header_positions)) == 0) {
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
      data_tbl_tmp <- read_tsv(data_tbl_tmp, col_names = "X")
      data_tbl_tmp <- separate(data_tbl_tmp, X, c("Frequency", "Magnitude"), sep = "\\s+")
      data_tbl_tmp <- mutate_all(data_tbl_tmp, parse_number)
      data_tbl_tmp <- mutate(data_tbl_tmp, `Scan Number` = vals[["Scan Number"]])
      data_tbl <- bind_rows(
        data_tbl,
        data_tbl_tmp
      )
    }

  } else {
    stop("Header has different number of elements for each data element.")
  }

  return(list(data_tbl, header_tbl))

}
