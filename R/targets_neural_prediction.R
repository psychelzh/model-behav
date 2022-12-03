# for g score estimation
hypers <- data.frame(num_vars = seq(11, 18))
neural_prediction <- tarchetypes::tar_map(
  hypers,
  list(
    tar_target(
      pred_neural,
      map_df(cpm, extract_cpm_corcoef),
      pattern = map(cpm)
    ),
    tar_target(
      cor_sims,
      correlate_full_g(g_scores, full_g_scores)
    ),
    tar_target(
      g_scores,
      map2(data, mdl, extract_g_scores),
      pattern = map(data, mdl)
    ),
    tar_target(
      g_scores_no_cover,
      map(g_scores, regress_behav_covar, subjs_info = subjs_info_merged),
      pattern = map(g_scores)
    ),
    tar_target(
      cpm,
      map(
        g_scores_no_cover,
        correlate_neural,
        neural = neural_full_no_covar,
        connections = "overall"
      ),
      pattern = map(g_scores_no_cover)
    ),
    tarchetypes::tar_rep(
      data,
      resample_data(indices_wider_clean, num_vars),
      iteration = "list",
      batches = 25,
      reps = 4
    ),
    tar_target(
      mdl,
      map(data, build_model),
      pattern = map(data)
    )
  )
)
