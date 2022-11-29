#' Restore upper triangular data to full matrix
#'
#' @title
#' @param neural_data
#' @param subjs_info_merged
#' @return
#' @author Liang Zhang
#' @export
restore_full_fc <- function(neural_data, subjs_info_merged) {
  # discard non-matching data to reduce data size
  neural_data <- neural_data[, subjs_info_merged$id]
  # the answer to equation: n * (n - 1) / 2 = num_edges
  num_vars <- (sqrt((8 * nrow(neural_data)) + 1) + 1) / 2
  num_subjs <- ncol(neural_data)
  res <- array(0, dim = c(num_vars, num_vars, num_subjs))
  for (subj in seq_len(num_subjs)) {
    cor_mat <- matrix(0, nrow = num_vars, ncol = num_vars)
    cor_mat[upper.tri(cor_mat)] <- neural_data[, subj]
    cor_mat <- cor_mat + t(cor_mat)
    res[, , subj] <- cor_mat
  }
  res
}

#' Match subjects from neural and behavioral data
#'
#' @title
#' @param subjs_info
#' @param indices_wider_clean
#' @return
#' @author Liang Zhang
#' @export
match_subjs <- function(subjs_info, indices_wider_clean) {
  subj_vars <- names(subjs_info)
  subjs_info |>
    mutate(id = row_number()) |>
    filter(!if_any(c(age, gender, scanner), is.na)) |>
    inner_join(indices_wider_clean, by = "sub_id") |>
    select(id, all_of(subj_vars))
}

#' Use CPM method to correlate to neural data
#'
#' @title
#' @param g_scores
#' @param neural_full
#' @param subjs_info_merged
#' @param ... Further arguments passed to [NetworkToolbox::cpmIV()].
#' @return
#' @author Liang Zhang
#' @export
correlate_neural <- function(g_scores, neural_full, subjs_info_merged, ...) {
  behav <- subjs_info_merged |>
    left_join(g_scores, by = "sub_id") |>
    pull(g)
  if (anyNA(behav)) {
    return(NULL)
  }
  covars <- as.list(select(subjs_info_merged, age, gender, scanner))
  cpmIV(
    neural_full,
    behav,
    kfolds = 10,
    # covar = covars,
    cores = 1,
    plots = FALSE,
    progBar = FALSE,
    ...
  )
}
