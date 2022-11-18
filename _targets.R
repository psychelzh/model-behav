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
    indices_keepTrack,
    "data/keepTrack_4scores_all.csv",
    read = read_csv(!!.x, show_col_types = FALSE) |>
      rowwise() |>
      mutate(score = sum(c_across(starts_with("score")))) |>
      ungroup() |>
      transmute(
        task = "keepTrack",
        disp_name = "keep_track",
        sub_id = ID,
        index = "score",
        score
      ) |>
      group_by(sub_id) |>
      filter(row_number() == 1) |>
      ungroup()
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
      semi_join(
        filter(task_indices, selected),
        by = c("task", "index")
      ) |>
      left_join(config, by = "task") |>
      select(-preproc) |>
      bind_rows(indices_keepTrack)
  )
)
