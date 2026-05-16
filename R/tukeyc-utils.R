## Internal helpers for TukeyC (not exported).

.tukeyc_emmeans_ci_cols <- function(aux_mt) {
  ci_lower <- intersect(c("lower.CL", "asymp.LCL"), names(aux_mt))[1]
  ci_upper <- intersect(c("upper.CL", "asymp.UCL"), names(aux_mt))[1]
  if (is.na(ci_lower) || is.na(ci_upper)) {
    stop("emmeans output missing confidence-interval columns.", call. = FALSE)
  }
  c(lower = ci_lower, upper = ci_upper)
}

.tukeyc_replace_within_errors <- function(many_errors) {
  if (any(many_errors == "Within")) {
    gsub("Within", "Residuals", many_errors)
  } else {
    many_errors
  }
}

.tukeyc_which_parts <- function(which) {
  unlist(strsplit(which, ":", fixed = TRUE))
}

.tukeyc_filter_nest <- function(data, which, fl1, fl2, level_env) {
  parts <- .tukeyc_which_parts(which)
  nf1 <- parts[1]
  nf2 <- parts[2]

  if (is.null(fl2)) {
    f2 <- levels(level_env[[nf1]])[fl1]
    data[data[[nf1]] == f2, , drop = FALSE]
  } else {
    f2 <- levels(level_env[[nf2]])[fl2]
    f3 <- levels(level_env[[nf1]])[fl1]
    data[data[[nf1]] == f3 & data[[nf2]] == f2, , drop = FALSE]
  }
}

.tukeyc_interaction_label <- function(data, treat) {
  factors <- unlist(strsplit(treat, ":", fixed = TRUE))
  ints <- do.call(interaction, c(data[factors], list(drop = TRUE)))
  gsub(":", "/", as.character(ints))
}
