#'  Create an expanded grid of two data.frames
#' @param data_1 A data.frame to be expanded.
#' @param data_2 A data.frame to be expanded.
#' @return A data.frame that is the Cartesian product of `data_1` and `data_2`.
#' @details This function creates an expanded grid (Cartesian product) of two data.frames
#' while preserving the class of the first data.frame (e.g., tibble, rowwise_df).
#' It uses `data.table` for efficient computation.
#' @examples
#' df1 <- data.frame(a = 1:2, b = letters[1:2])
#' df2 <- data.frame(x = 3:4, y = LETTERS[1:2])
#' expanded_df <- expand_grid_df(df1, df2)
#' print(expanded_df)
#' @import data.table
expand_grid_df <- function(data_1, data_2) {
  data_1_ex <-
    data_1[rep(1:nrow(data_1), each = nrow(data_2)), ] %>%
    data.table() %>%
    .[, rowid := 1:nrow(.)]

  data_2_ex <-
    data_2[rep(1:nrow(data_2), nrow(data_1)), ] %>%
    data.table() %>%
    .[, rowid := 1:nrow(.)]

  expanded_data <-
    data_1_ex[data_2_ex, on = "rowid"] %>%
    .[, rowid := NULL]

  if ("tbl" %in% class(data_1)) {
    expanded_data <- as_tibble(expanded_data)
  }

  if ("rowwise_df" %in% class(data_1)) {
    expanded_data <- rowwise(expanded_data)
  }

  return(expanded_data)
}
