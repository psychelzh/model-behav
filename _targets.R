library(targets)
tar_option_set(packages = "tidyverse")
tar_source()
future::plan(future.callr::callr)
config <- readr::read_csv("config.csv", show_col_types = FALSE) |>
  tidyr::drop_na() |>
  dplyr::mutate(preproc = rlang::syms(paste0("preproc_", preproc)))
static_branches <- tarchetypes::tar_map(
  config,
  names = task,
  list(
    tarchetypes::tar_file_read(
      data,
      fs::path("data", sprintf("%s.arrow", task)),
      read = arrow::read_feather(!!.x)
    ),
    tar_target(data_clean, screen_data(data)),
    tar_target(
      indices,
      preproc(data_clean) |>
        pivot_longer(
          -any_of(id_cols()),
          names_to = "index",
          values_to = "score"
        ) |>
        select(-task_datetime)
    )
  )
)
list(
  static_branches,
  tarchetypes::tar_combine(
    indices,
    static_branches[[4]],
    command = bind_rows(
      !!!.x,
      .id = "name"
    ) |>
      mutate(name = str_remove(name, "indices_"))
  )
)
