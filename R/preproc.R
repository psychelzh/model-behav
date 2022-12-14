preproc_antisac <- function(data) {
  calc_spd_acc(data, .by = id_cols())
}

#' Switch Cost
#'
#' These tasks are relative easy, subjects with a low total accuracy is screened
#' out.
preproc_cateswitch <- function(data) {
  data |>
    group_by(across(all_of(id_cols()))) |>
    filter(sum(acc == 1) > qbinom(0.95, n(), 0.5)) |>
    ungroup() |>
    calc_cost()
}
preproc_shifting <- function(data) {
  data |>
    group_by(across(all_of(id_cols()))) |>
    mutate(
      last_task = lag(task),
      task_switch = case_when(
        is.na(last_task) ~ "filler",
        task == last_task ~ "repeat",
        TRUE ~ "switch"
      )
    ) |>
    ungroup() |>
    preproc_cateswitch()
}

preproc_crt <- function(data) {
  data |>
    group_by(across(all_of(id_cols()))) |>
    filter(mean(Correct == 1) > 0.8) |>
    ungroup() |>
    calc_spd_acc(
      name_rt = "Response_Time",
      name_acc = "Correct",
      .by = id_cols()
    )
}
preproc_srt <- function(data) {
  data |>
    group_by(across(all_of(id_cols()))) |>
    summarise(
      mrt = median(Response),
      .groups = "drop"
    )
}

preproc_filtering <- function(data) {
  data_clean <- data |>
    # missing trials are treated as incorrect
    mutate(acc = coalesce(acc, 0)) |>
    group_by(across(all_of(id_cols()))) |>
    filter(sum(acc == 1) > qbinom(0.95, n(), 0.5)) |>
    mutate(grp_id = cur_group_id(), .before = 1L) |>
    ungroup()
  # this step is time consuming
  fit <- lme4::glmer(
    acc ~ n_distractor + (n_distractor | grp_id),
    data_clean,
    family = binomial()
  )
  slopes <- lme4::ranef(fit)$grp_id |>
    as_tibble(rownames = "grp_id") |>
    transmute(
      grp_id = as.integer(grp_id),
      filt_eff = n_distractor + lme4::fixef(fit)["n_distractor"]
    )
  capacity <- data_clean |>
    filter(n_distractor == 0) |>
    group_by(grp_id, rotated) |>
    summarise(
      pc = mean(acc == 1),
      .groups = "drop_last"
    ) |>
    summarise(k = 2 * sum(pc))
  data_clean |>
    distinct(across(all_of(c("grp_id", id_cols())))) |>
    inner_join(slopes, by = "grp_id") |>
    inner_join(capacity, by = "grp_id") |>
    select(-grp_id)
}

#' Long-term Memory
#'
#' "FNRecog, KRecog"
preproc_ltm <- function(data) {
  data |>
    # remove subjects with too much quick responses
    group_by(across(all_of(id_cols()))) |>
    filter(mean(rt > 1) > 0.8) |>
    ungroup() |>
    mutate(type_sig = if_else(type == "old", "s", "n")) |>
    calc_auc_ltm(
      name_type = "type_sig",
      name_resp = "resp",
      .by = id_cols()
    ) |>
    filter(auc > 0.5)
}
#' Complex Span
preproc_ospan <- function(data) {
  data |>
    filter(math_error < 18) |>
    select(all_of(c(id_cols(), score = "ospan_total")))
}
preproc_sspan <- function(data) {
  data |>
    filter(symm_error < 18) |>
    select(all_of(c(id_cols(), score = "sspan_total")))
}

#' N-back
preproc_twoback <- function(data) {
  data |>
    group_by(across(all_of(c(id_cols(), "block")))) |>
    mutate(base_loc = lag(loc, 2)) |>
    ungroup() |>
    mutate(
      type = case_when(
        is.na(base_loc) ~ "filler",
        loc == base_loc ~ "same",
        TRUE ~ "diff"
      ),
      # no response means error
      acc = coalesce(acc, 0)
    ) |>
    group_by(across(all_of(id_cols()))) |>
    filter(sum(acc == 1) > qbinom(0.95, n(), 0.5)) |>
    ungroup() |>
    preproc.iquizoo::nback(.by = id_cols())
}
preproc_threeback <- function(data) {
  data |>
    filter(!is.na(type)) |>
    mutate(
      type = if_else(type == "match", "same", "diff"),
      # no response means error
      acc = coalesce(acc, 0)
    ) |>
    group_by(across(all_of(id_cols()))) |>
    filter(sum(acc == 1) > qbinom(0.95, n(), 0.5)) |>
    ungroup() |>
    preproc.iquizoo::nback(.by = id_cols())
}

preproc_spst <- function(data) {
  inner_join(
    data |>
      group_by(across(all_of(c(id_cols(), "condition")))) |>
      summarise(p_sim = mean(response == "similar"), .groups = "drop_last") |>
      summarise(
        bps_score = p_sim[condition == "lure"] - p_sim[condition == "foil"],
        .groups = "drop"
      ),
    data |>
      mutate(LureBin = if_else(condition == "foil", 6, LureBin)) |>
      group_by(across(all_of(c(id_cols(), "LureBin")))) |>
      summarise(p_old = mean(response == "old"), .groups = "drop_last") |>
      summarise(auc = DescTools::AUC(LureBin, 1 - p_old), .groups = "drop"),
    by = id_cols()
  )
}

preproc_stopsignal <- function(data) {
  # remove negative ssd subjects
  data_clean <- data |>
    group_by(across(all_of(id_cols()))) |>
    filter(!any(ssd < 0)) |>
    ungroup()
  ssd <- data_clean |>
    filter(is_stop == 1) |>
    group_by(across(all_of(c(id_cols(), "stair_case")))) |>
    summarise(ssd = summarise_ssd(ssd), .groups = "drop_last") |>
    summarise(mssd = mean(ssd), .groups = "drop")
  rt_go <- data_clean |>
    filter(is_stop == 1) |>
    group_by(across(all_of(c(id_cols())))) |>
    summarise(
      pe_stop = mean(resp != "none"),
      .groups = "drop"
    ) |>
    inner_join(
      data_clean |>
        filter(is_stop == 0) |>
        group_nest(across(all_of(id_cols()))),
      by = id_cols()
    ) |>
    mutate(
      rt_nth = map2_dbl(
        data, pe_stop,
        ~ quantile(.x$rt, .y)
      )
    ) |>
    select(-data)
  inner_join(rt_go, ssd, by = id_cols()) |>
    mutate(ssrt = rt_nth - mssd / 1000) |>
    filter(
      rt_nth > 0,
      between(pe_stop, 0.25, 0.75)
    )
}

preproc_stroop <- function(data) {
  data |>
    group_by(across(all_of(id_cols()))) |>
    filter(sum(acc == 1) > qbinom(0.95, n(), 0.25)) |>
    ungroup() |>
    tidytable::mutate(
      char = tidytable::recode(
        char,
        ??? = "r",
        ??? = "y",
        ??? = "g",
        ??? = "b"
      ),
      type = tidytable::if_else(
        color == char,
        "con",
        "inc"
      )
    ) |>
    calc_spd_acc(
      name_rt = "rt",
      name_acc = "acc",
      .by = c(id_cols(), "type")
    ) |>
    pivot_longer(
      -any_of(c(id_cols(), "type")),
      names_to = "index_name",
      values_to = "index_value"
    ) |>
    pivot_wider(
      names_from = type,
      values_from = index_value
    ) |>
    mutate(
      con_eff = inc - con,
      .keep = "unused"
    ) |>
    pivot_wider(
      names_from = index_name,
      names_prefix = "con_eff_",
      values_from = con_eff
    )
}

preproc_existed <- function(data, ..., disp_name) {
  data |>
    select(sub_id = ID, ...) |>
    group_by(sub_id) |>
    filter(row_number() == 1) |>
    ungroup() |>
    pivot_longer(
      -sub_id,
      names_to = "task_index",
      values_to = "score"
    ) |>
    separate(task_index, c("task", "index")) |>
    mutate(index = tolower(index)) |>
    add_column(disp_name = disp_name)
}

screen_data <- function(data) {
  data |>
    distinct(across(all_of(id_cols()))) |>
    group_by(sub_id) |>
    # keep the last commit only
    filter(
      case_when(
        all(is.na(task_datetime)) ~ file == file[[1]],
        all(!is.na(task_datetime)) ~ row_number(desc(task_datetime)) == 1,
        TRUE ~ !is.na(task_datetime)
      )
    ) |>
    ungroup() |>
    left_join(data, by = id_cols())
}

clean_indices <- function(indices, indices_selection) {
  indices |>
    inner_join(
      filter(indices_selection, selected),
      by = c("task", "index")
    ) |>
    mutate(score_norm = if_else(reversed, -score, score)) |>
    group_by(disp_name, index) |>
    filter(!performance::check_outliers(score, method = "iqr")) |>
    ungroup()
}

reshape_data_wider <- function(indices) {
  indices |>
    unite("task_index", disp_name, index, remove = FALSE) |>
    pivot_wider(
      id_cols = sub_id,
      names_from = task_index,
      values_from = score_norm
    )
}
