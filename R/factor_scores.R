calc_g_scores <- function(data, id_cols = "sub_id") {
  g_scores <- extract_g_scores(select(data, -all_of(id_cols)))
  bind_cols(data[, id_cols], g_scores)
}

extract_g_scores <- function(data) {
  mdl <- umxEFA(as.data.frame(data), factors = "g")
  umxFactorScores(mdl, type = "ML", minManifests = 3)
}
