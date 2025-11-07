#' list_to_tibble
#'
#' Combines a list of tibbles to a tibble. If one or several columns are incompatible, the columns with the most abundant column type in the list are retained and the incompatible columns are set to NA.
#'
#' @param data_list a list of tibbles
#'
#' @return a combined tibble
#' @export
list_to_tibble <- function(data_list) {
  col_classes <- data_list |> purrr::map_dfr(function(x) {
    x |> dplyr::summarise_all(class)
  })

  # make all not matching columns NA
  for (col in 1:ncol(col_classes)) {
    classes <- col_classes |>
      dplyr::select(all_of(col)) |>
      dplyr::count(dplyr::across(dplyr::everything()), sort = TRUE)
    if (nrow(classes) == 1) {next}

    # the largest group is assumed to be correct
    class <- classes[1,1] |> purrr::as_vector()
    # and therefore removed
    classes <- classes[-1, -2] |> purrr::as_vector()

    # change all different column to NA
    for (i in 1:length(classes)) {
      idx <- which(classes[i] == col_classes[, col])[[1]]
      data_list[[idx]][, col] <- NA
    }
  }

  res <- data_list |> purrr::map_dfr(function(x){x})
  return(res)
}
