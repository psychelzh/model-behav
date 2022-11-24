library(targets)
tar_option_set(packages = "tidyverse")
tar_source()
future::plan(future.callr::callr)
config <- readr::read_csv("config/task_preproc.csv", show_col_types = FALSE) |>
  tidyr::drop_na() |>
  dplyr::mutate(preproc = rlang::syms(paste0("preproc_", preproc)))
static_branches <- tarchetypes::tar_map(
  config,
  names = task,
  list(
    tar_target(
      indices,
      preproc(data_clean) |>
        pivot_longer(
          -any_of(id_cols()),
          names_to = "index",
          values_to = "score"
        ) |>
        select(-task_datetime)
    ),
    tarchetypes::tar_file_read(
      data,
      fs::path("data", sprintf("%s.arrow", task)),
      read = arrow::read_feather(!!.x)
    ),
    tar_target(data_clean, screen_data(data))
  )
)
list(
  tarchetypes::tar_file_read(
    task_indices,
    "config/task_indices.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tarchetypes::tar_file_read(
    sub_id_transform,
    "config/sub_id_transform.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tarchetypes::tar_file_read(
    data_clean,
    "data/data_clean.csv",
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
  static_branches,
  tarchetypes::tar_combine(
    indices,
    static_branches[[1]],
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
      mutate(sub_id = coalesce(fmri_id, sub_id), .keep = "unused") |>
      semi_join(
        filter(task_indices, selected),
        by = c("task", "index")
      )
  ),
  tar_target(
    indices_clean,
    indices |>
      group_by(disp_name, index) |>
      filter(!performance::check_outliers(score, method = "iqr")) |>
      ungroup()
  ),
  tar_target(
    indices_wider,
    indices_clean |>
      unite("task_index", disp_name, index, remove = FALSE) |>
      pivot_wider(
        id_cols = sub_id,
        names_from = task_index,
        values_from = score
      )
  ),
  tar_target(
    indices_wider_clean,
    indices_wider |>
      rowwise() |>
      filter(mean(is.na(c_across(-sub_id))) < 0.2) |>
      ungroup()
  ),
  tar_target(
    mdl_data,
    select(indices_wider_clean, -sub_id)
  )
)
