m.infos.nest.lmerMod <- function(x,
                                 my,
                                 forminter,
                                 which,
                                 fl1,
                                 fl2,
                                 sig.level,
                                 aux_mt,
                                 ...) {
  ci <- .tukeyc_emmeans_ci_cols(aux_mt)

  aux_m.inf <- aggregate(forminter,
    data = x@frame,
    function(x) {
      c(
        min = min(x),
        max = max(x),
        sd = sd(x),
        se = sd(x) / length(x)
      )
    }
  )

  aux_m.inf1 <- data.frame(aux_m.inf[names(aux_m.inf) != my],
    means = with(aux_mt, emmean),
    aux_m.inf[[my]][, 1:2],
    "linf_sd" = with(aux_mt, emmean) - aux_m.inf[[my]][, 3],
    "lsup_sd" = with(aux_mt, emmean) + aux_m.inf[[my]][, 3],
    "linf_se" = with(aux_mt, emmean) - abs(qt(sig.level, with(aux_mt, emmean / SE))) * aux_m.inf[[my]][, 4],
    "lsup_se" = with(aux_mt, emmean) + abs(qt(sig.level, with(aux_mt, emmean / SE))) * aux_m.inf[[my]][, 4],
    "linf_sepool" = aux_mt[[ci["lower"]]],
    "lsup_sepool" = aux_mt[[ci["upper"]]]
  )

  aux_m.inf2 <- aux_m.inf1[order(aux_m.inf1[["means"]],
    decreasing = TRUE
  ), ]

  nf1 <- unlist(strsplit(which,
    split = ":"
  ))[1] # First factor in which

  nf2 <- unlist(strsplit(which,
    split = ":"
  ))[2] # Second factor in which

  nf3 <- unlist(strsplit(which,
    split = ":"
  ))[3] # Third factor in which

  if (is.null(fl2)) {
    f2 <- levels(x@frame[[nf1]])[fl1] # Level of the factor used for the split comparison

    aux_m.inf21 <- .tukeyc_filter_nest(aux_m.inf2, which, fl1, fl2, x@frame)

    m.inf <- list(
      Means = aux_m.inf21[, c(1:3)],
      mm = aux_m.inf21[, c(1:2, 4:5)],
      sd = aux_m.inf21[, c(1:2, 6:7)],
      ic = aux_m.inf21[, c(1:2, 8:9)],
      icpool = aux_m.inf21[, c(1:2, 10:11)]
    )
  } else {
    f2 <- levels(x@frame[[nf2]])[fl2]

    f3 <- levels(x@frame[[nf1]])[fl1]

    aux_m.inf21 <- .tukeyc_filter_nest(aux_m.inf2, which, fl1, fl2, x@frame)

    m.inf <- list(
      Means = aux_m.inf21[, c(1:4)],
      mm = aux_m.inf21[, c(1:3, 5:6)],
      sd = aux_m.inf21[, c(1:3, 7:8)],
      ic = aux_m.inf21[, c(1:3, 9:10)],
      icpool = aux_m.inf21[, c(1:3, 11:12)]
    )
  }
}
