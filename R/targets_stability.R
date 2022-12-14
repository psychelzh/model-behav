# g scores stability
hypers_stability <- data.frame(num_vars = 3:10)
factor_scores_stability <- tarchetypes::tar_map(
  hypers_stability,
  list(
    tar_target(
      intra_cor_g_scores_pairs,
      map_df(g_scores_pairs, extract_pairs_cor),
      pattern = map(g_scores_pairs)
    ),
    tar_target(
      dice_mask_pairs,
      map_df(cpm_pairs, calc_mask_simil),
      pattern = map(cpm_pairs)
    ),
    tar_target(
      pred_neural_pairs,
      map(
        cpm_pairs,
        do_pairs,
        .fun = extract_cpm_corcoef,
        .bind = TRUE
      ),
      pattern = map(cpm_pairs)
    ),
    tar_target(
      cor_sims_pairs,
      map(
        g_scores_pairs,
        correlate_full_g_pairs,
        full_g_scores = full_g_scores
      )
    ),
    tar_target(
      pred_efficiencies_pairs,
      map(
        g_scores_pairs,
        do_pairs,
        .fun = correlate_efficiency,
        efficiency = global_efficencies,
        subjs_info = subjs_info_merged,
        .bind = TRUE
      )
    ),
    tar_target(
      cor_rapm_pairs,
      map(
        g_scores_pairs,
        correlate_rapm_pairs,
        indices_rapm = indices_rapm
      )
    ),
    tar_target(
      cpm_pairs,
      map(
        g_scores_pairs_no_cover,
        do_pairs,
        .fun = correlate_neural,
        neural = neural_full_no_covar,
        connections = "overall"
      ),
      pattern = map(g_scores_pairs_no_cover)
    ),
    tarchetypes::tar_rep(
      data_pairs,
      resample_data_pairs(indices_wider_clean, num_vars),
      iteration = "list",
      batches = 25,
      reps = 4
    ),
    tar_target(
      mdl_pairs,
      map(data_pairs, do_pairs, .fun = build_model),
      pattern = map(data_pairs)
    ),
    tar_target(
      g_scores_pairs,
      map2(data_pairs, mdl_pairs, extract_g_scores_pairs),
      pattern = map(data_pairs, mdl_pairs)
    ),
    tar_target(
      g_scores_pairs_no_cover,
      map(
        g_scores_pairs,
        do_pairs,
        .fun = regress_behav_covar,
        subjs_info = subjs_info_merged
      ),
      pattern = map(g_scores_pairs)
    )
  )
)
