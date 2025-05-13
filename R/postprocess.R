library(purrr)
library(tibble)
library(dplyr)

## Results Processing ----
show_scales_result <- function(result) {
  if (tibble::is_tibble(result)) {
    result |>
      dplyr::rowwise() |>
      dplyr::mutate(
        items = paste(items, collapse = ' – ')
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-mask, -texts)
  } else if (is.list(result) | !is.data.frame(result)) {
    purrr::map(result, show_scales_result)
  }
}
