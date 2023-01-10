resample_data <- function(data, num_vars = NULL, id_cols = "sub_id") {
  name_vars <- setdiff(names(data), id_cols)
  if (is.null(num_vars)) {
    num_vars <- length(name_vars)
  }
  name_vars_sel <- sample(name_vars, num_vars)
  bind_cols(
    select(data, all_of(id_cols)),
    select(data, all_of(name_vars_sel))
  )
}

resample_data_pairs <- function(data, num_vars, id_cols = "sub_id") {
  name_vars <- setdiff(names(data), id_cols)
  stopifnot(num_vars * 2 <= length(name_vars))
  name_vars_first <- sample(name_vars, num_vars)
  name_vars_second <- sample(setdiff(name_vars, name_vars_first), num_vars)
  list(first = name_vars_first, second = name_vars_second) |>
    map(
      ~ bind_cols(
        select(data, all_of(id_cols)),
        select(data, all_of(.))
      )
    )
}

build_model <- function(data, id_cols = "sub_id") {
  data |>
    select(-all_of(id_cols), -any_of(contains("tar"))) |>
    umxEFA(factors = "g")
}

extract_g_scores <- function(data, mdl, id_cols = "sub_id") {
  bind_cols(
    select(data, all_of(id_cols), any_of(contains("tar"))),
    umxFactorScores(mdl, type = "WeightedML", minManifests = 1)
  )
}

correlate_full_g <- function(g_scores, full_g_scores) {
  g_scores |>
    bind_rows() |>
    inner_join(rename(full_g_scores, full_g = g), by = "sub_id") |>
    group_by(across(starts_with("tar"))) |>
    summarise(r = cor(g, full_g, use = "pairwise"), .groups = "drop")
}

correlate_rapm <- function(g_scores, indices_rapm) {
  g_scores |>
    bind_rows() |>
    inner_join(indices_rapm, by = "sub_id") |>
    group_by(across(starts_with("tar"))) |>
    summarise(r = cor(g, rapm_score, use = "pairwise"), .groups = "drop")
}

# special for paired analysis
extract_pairs_cor <- function(g_scores_pairs) {
  g_scores_pairs |>
    bind_rows(.id = "part") |>
    pivot_wider(
      names_from = part,
      names_prefix = "g_",
      values_from = "g"
    ) |>
    summarise(r = cor(g_first, g_second, use = "pairwise"))
}

extract_g_scores_pairs <- function(data, mdl, id_cols = "sub_id") {
  parts <- c("first", "second")
  scores <- list()
  for (part in parts) {
    scores[[part]] <- extract_g_scores(data[[part]], mdl[[part]], id_cols)
  }
  scores
}

correlate_full_g_pairs <- function(g_scores_pairs, full_g_scores) {
  map_at(
    g_scores_pairs,
    c("first", "second"),
    ~ . |>
      inner_join(rename(full_g_scores, full_g = g), by = "sub_id") |>
      summarise(r = cor(g, full_g, use = "pairwise"))
  ) |>
    bind_rows(.id = "part")
}

correlate_rapm_pairs <- function(g_scores_pairs, indices_rapm) {
  map_at(
    g_scores_pairs,
    c("first", "second"),
    ~ . |>
      inner_join(indices_rapm, by = "sub_id") |>
      summarise(r = cor(g, rapm_score, use = "pairwise"))
  ) |>
    bind_rows(.id = "part")
}
