do_cpm <- function(fc_data, scores, thresh_method, thresh_level) {
  thresh_level <- switch(
    thresh_method,
    alpha = c(0.01, 0.005, 0.001)[thresh_level],
    sparsity = c(0.01, 0.025, 0.05, 0.1)[thresh_level]
  )
  data <- fc_data |>
    tidytable::inner_join(scores, by = "sub_id") |>
    select(-sub_id) |>
    as.matrix()
  cpm(
    data,
    kfolds = 10,
    thresh_method = thresh_method,
    thresh_level = thresh_level
  )
}

config_fc_data <- tidyr::expand_grid(
  modal = c("emotion", "facename",
            "Nbackrun1", "Nbackrun2",
            "4tasks", "rest", "4tasksrest"),
  parcel = c("nn268", "Power264"),
  gsr = c("with", "without"),
  latent = c("g", "Speed", "WM", "Memory", "Flex"),
  dplyr::bind_rows(
    tibble::tibble(
      thresh_method = "alpha",
      thresh_level = 1 # 1 = 0.01
    ),
    tibble::tibble(
      thresh_method = "sparsity",
      thresh_level = 1 # 1 = 0.01
    )
  )
)

targets_cpm <- tarchetypes::tar_map(
  config_fc_data,
  list(
    tarchetypes::tar_file_read(
      fc_data,
      sprintf(
        "data/neural/FC_modal-%s_parcel-%s_gsr-%s.arrow",
        modal, parcel, gsr
      ),
      read = arrow::read_feather(!!.x)
    ),
    tarchetypes::tar_rep(
      result_cpm,
      do_cpm(
        fc_data,
        scores_latent[, c("sub_id", latent)],
        thresh_method = thresh_method,
        thresh_level = thresh_level
      ),
      batches = 25,
      reps = 4,
      iteration = "list"
    ),
    tarchetypes::tar_rep2(
      correlations,
      data.frame(
        cor_pos = result_cpm[[1]]$cor_pos$estimate,
        cor_neg = result_cpm[[1]]$cor_neg$estimate,
        cor_all = result_cpm[[1]]$cor_all$estimate
      ),
      result_cpm
    )
  )
)
