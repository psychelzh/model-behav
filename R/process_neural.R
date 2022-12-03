#' Restore upper triangular data to full matrix
#'
#' @title
#' @param neural_data
#' @param subset
#' @return
#' @author Liang Zhang
#' @export
restore_full_fc <- function(neural_data, subset = NULL) {
  if (!is.null(subset)) {
    neural_data <- neural_data[, subset]
  }
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

#' Regress out co-variates
#'
#' @title
#' @param neural_data
#' @param subjs_info
#' @param name_covars
#' @return
#' @author Liang Zhang
#' @export
regress_neural_covar <- function(neural_data, subjs_info,
                                 name_covars = c("age", "gender", "scanner")) {
  # discard non-matching data to reduce data size
  neural_data <- neural_data[, subjs_info$id]
  apply(
    neural_data,
    1,
    \(x) {
      with(
        subjs_info,
        str_c("x ~ ", str_c(name_covars, collapse = "+")) |>
          as.formula() |>
          lm() |>
          resid()
      )
    }
  ) |>
    t()
}
regress_behav_covar <- function(behav, subjs_info,
                                name_resp = "g",
                                name_covars = c("age", "gender", "scanner")) {
  data <- subjs_info |>
    left_join(behav, by = "sub_id")
  lm(
    str_c(name_resp, " ~ ", str_c(name_covars, collapse = "+")) |>
      as.formula(),
    data,
    na.action = na.exclude
  ) |>
    resid()
}

#' Use CPM method to correlate to neural data
#'
#' @title
#' @param behav
#' @param neural
#' @param ... Further arguments passed to [NetworkToolbox::cpmIV()].
#' @return
#' @author Liang Zhang
#' @export
correlate_neural <- function(behav, neural, ...) {
  # remove subjects with NA values (only from behavioral measures)
  keep_subjs <- !is.na(behav)
  cpmIV(
    neural[, , keep_subjs],
    behav[keep_subjs],
    kfolds = 10,
    cores = 1,
    plots = FALSE,
    progBar = FALSE,
    ...
  )
}

extract_cpm_corcoef <- function(cpm) {
  if (!is.null(cpm)) {
    cpm |>
      pluck("results") |>
      as_tibble() |>
      transmute(r = as.numeric(r))
  }
}

calc_mask_simil <- function(cpm_pairs) {
  map(cpm_pairs, "Mask") |>
    map(~ .[upper.tri(.)]) |>
    bind_cols() |>
    t() |>
    proxy::simil(method = "dice") |>
    unclass() |>
    as_tibble_col("dice_mask")
}
