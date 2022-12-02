# for g score estimation
hypers <- data.frame(num_vars = seq(3, 18, 3))
neural_prediction <- tarchetypes::tar_map(
  hypers,
  list(
    tar_target(
      g_scores,
      map2(data, mdl, extract_g_scores),
      pattern = map(data, mdl)
    ),
    tar_target(
      cor_sims,
      correlate_full_g(g_scores, full_g_scores)
    ),
    tarchetypes::tar_rep(
      data,
      resample_data(indices_wider_clean, num_vars),
      iteration = "list",
      batches = 50,
      reps = 10
    ),
    tar_target(
      mdl,
      map(data, build_model),
      pattern = map(data)
    )
  )
)
