library(targets)
conflicted::conflict_prefer("desc", "dplyr", quiet = TRUE)
tar_option_set(
  packages = c("tidyverse", "umx", "NetworkToolbox"),
  memory = "transient",
  garbage_collection = TRUE,
  error = "null",
  format = "qs"
)
tar_source()
future::plan(future.callr::callr)

list(
  tarchetypes::tar_file_read(
    indices_selection,
    "config/indices_selection.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tarchetypes::tar_file_read(
    sub_id_transform,
    "config/sub_id_transform.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tarchetypes::tar_file_read(
    data_clean,
    "data/behav/data_clean.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(
    indices_keepTrack,
    preproc_existed(
      data_clean,
      Keeptrack_ScoreAll,
      disp_name = "keep_track"
    )
  ),
  tar_target(
    indices_FM,
    preproc_existed(
      data_clean,
      starts_with("FM"),
      disp_name = "false_mem"
    )
  ),
  tar_target(
    indices_Raven,
    preproc_existed(
      data_clean,
      all_of(c(Raven_score = "Raven2")),
      disp_name = "rapm"
    )
  ),
  # targets_clean_behav.R
  load_data_behav,
  tarchetypes::tar_combine(
    indices,
    load_data_behav[[1]],
    command = bind_rows(
      !!!.x,
      .id = "task"
    ) |>
      mutate(task = str_remove(task, "indices_")) |>
      left_join(config, by = "task") |>
      select(-preproc) |>
      bind_rows(
        indices_keepTrack,
        indices_FM,
        indices_Raven
      ) |>
      left_join(sub_id_transform, by = c("sub_id" = "behav_id")) |>
      mutate(sub_id = coalesce(fmri_id, sub_id), .keep = "unused")
  ),
  tar_target(indices_clean, clean_indices(indices, indices_selection)),
  tar_target(
    indices_rapm,
    clean_indices(
      indices,
      tribble(
        ~task, ~index, ~selected, ~reversed,
        "Raven", "score", TRUE, FALSE
      )
    ) |>
      reshape_data_wider()
  ),
  tar_target(
    indices_rapm_no_covar,
    regress_behav_covar(
      indices_rapm, subjs_info_merged,
      name_resp = "rapm_score"
    )
  ),
  tar_target(
    cpm_rapm,
    correlate_neural(
      indices_rapm_no_covar,
      neural_full_no_covar,
      connections = "overall"
    )
  ),
  tar_target(
    indices_wider,
    reshape_data_wider(indices_clean)
  ),
  tar_target(
    indices_wider_clean,
    indices_wider |>
      rowwise() |>
      filter(mean(is.na(c_across(-sub_id))) < 0.2) |>
      ungroup()
  ),
  tarchetypes::tar_quarto(quarto_site),
  tar_target(
    full_g_mdl,
    build_model(indices_wider_clean)
  ),
  tar_target(
    full_g_scores,
    extract_g_scores(indices_wider_clean, full_g_mdl)
  ),
  tar_target(
    full_g_scores_no_covar,
    regress_behav_covar(full_g_scores, subjs_info_merged)
  ),
  tar_target(
    cpm_full_g,
    correlate_neural(
      full_g_scores_no_covar,
      neural_full_no_covar,
      connections = "overall"
    )
  ),
  # targets_neural_prediction.R
  neural_prediction,
  tarchetypes::tar_combine(
    cor_sims,
    neural_prediction[[2]],
    command = bind_rows(!!!.x, .id = "name") |>
      mutate(num_vars = parse_number(name), .keep = "unused")
  ),
  tarchetypes::tar_file_read(
    subjs_info,
    "data/neural/subjs.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tarchetypes::tar_file_read(
    neural_data,
    "data/neural/fc_matrix.arrow",
    read = as.matrix(arrow::read_feather(!!.x))
  ),
  tar_target(
    subjs_info_merged,
    match_subjs(subjs_info, indices_wider_clean)
  ),
  tar_target(
    neural_full,
    restore_full_fc(neural_data, subjs_info_merged$id)
  ),
  tar_target(
    neural_data_no_covar,
    regress_neural_covar(neural_data, subjs_info_merged)
  ),
  tar_target(
    neural_full_no_covar,
    restore_full_fc(neural_data_no_covar)
  ),
  # targets_stability.R
  factor_scores_stability,
  tarchetypes::tar_combine(
    intra_cor_g_scores_pairs,
    factor_scores_stability[[1]],
    command = bind_rows(!!!.x, .id = "num_vars") |>
      mutate(num_vars = parse_number(num_vars))
  ),
  tarchetypes::tar_combine(
    dice_mask_pairs,
    factor_scores_stability[[2]],
    command = bind_rows(!!!.x, .id = "num_vars") |>
      mutate(num_vars = parse_number(num_vars))
  ),
  tarchetypes::tar_combine(
    pred_neural_pairs,
    factor_scores_stability[[3]],
    command = list(!!!.x) |>
      map(~ bind_rows(., .id = "src")) |>
      bind_rows(.id = "num_vars") |>
      mutate(num_vars = parse_number(num_vars))
  )
)
