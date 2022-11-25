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

build_model <- function(data, id_cols = "sub_id") {
  data |>
    select(-all_of(id_cols), -any_of(contains("tar"))) |>
    as.data.frame() |>
    umxEFA(factors = "g")
}

extract_g_scores <- function(data, mdl, id_cols = "sub_id") {
  bind_cols(
    select(data, all_of(id_cols), any_of(contains("tar"))),
    umxFactorScores(mdl, type = "WeightedML", minManifests = 1)
  )
}
