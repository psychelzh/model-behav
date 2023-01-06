library(targets)
tar_option_set(
  packages = c("tidyverse", "umx", "NetworkToolbox", "conflicted"),
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
    data_penncnp,
    "data/behav/penncnp.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(
    indices_penncnp,
    preproc_penncnp(data_penncnp)
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
      disp_name = "KPTRK"
    )
  ),
  tar_target(
    indices_FM,
    preproc_existed(
      data_clean,
      starts_with("FM"),
      disp_name = "FM"
    )
  ),
  tar_target(
    indices_Raven,
    preproc_existed(
      data_clean,
      all_of(c(Raven_score = "Raven2")),
      disp_name = "RAPM"
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
        indices_penncnp,
        indices_Raven
      ) |>
      left_join(sub_id_transform, by = c("sub_id" = "behav_id")) |>
      mutate(sub_id = coalesce(fmri_id, sub_id), .keep = "unused")
  ),
  tar_target(indices_clean, clean_indices(indices, indices_selection)),
  tar_target(
    indices_rapm,
    indices |>
      filter(task == "Raven", index == "score") |>
      filter(!performance::check_outliers(score, method = "iqr")) |>
      reshape_data_wider()
  ),
  tar_target(
    indices_wider,
    reshape_data_wider(indices_clean, name_score = "score_norm")
  ),
  tar_target(
    indices_wider_clean,
    indices_wider |>
      filter(sub_id < 17000) |>
      rowwise() |>
      filter(mean(is.na(c_across(-sub_id))) < 0.2) |>
      ungroup()
  ),
  tar_target(
    full_g_mdl,
    build_model(indices_wider_clean)
  ),
  tar_target(
    full_g_fitmeasure,
    summary(full_g_mdl, refModels = mxRefModels(full_g_mdl, run = TRUE))
  ),
  tar_target(
    mdl_bifac, {
      mdl_data <- select(indices_wider_clean, -sub_id)
      mdl_bifac <- psych::omega(mdl_data, 6, plot = FALSE)
      lavaan::cfa(
        mdl_bifac$model$lavaan, mdl_data, missing = "fiml",
        std.ov = T, std.lv = T, orthogonal = TRUE
      )
    }
  ),
  tar_target(
    mdl_no_rt, {
      mdl_data_no_rt <- indices_wider_clean |>
        select(-sub_id, -contains("mrt"), -crt_ies)
      umxEFA(mdl_data_no_rt, factors = "g")
    }
  ),
  tar_target(
    mdl_no_rt_fitmeasure,
    summary(mdl_no_rt, refModels = mxRefModels(mdl_no_rt, run = TRUE))
  )
)
