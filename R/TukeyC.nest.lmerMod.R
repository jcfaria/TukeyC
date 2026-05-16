TukeyC.nest.lmerMod <- function(x,
                                which,
                                fl1,
                                fl2,
                                MSE,
                                dfr,
                                sig.level,
                                round,
                                adjusted.pvalue, ...) {
  my <- as.character(formula(x)[[2]])
  m1 <- gsub("\\:", "\\+", which)
  m2 <- unlist(strsplit(
    which,
    "[[:punct:]]"
  ))

  forminter <- as.formula(paste(my, "~", m1))

  aux_r <- aggregate(forminter,
    data = x@frame,
    function(x) r <- length(x)
  )
  reps <- aux_r[[my]]

  aux_mt <- suppressMessages(emmeans::emmeans(x,
    specs = m2,
    level = 1 - sig.level
  ))
  aux_mt_df <- as.data.frame(aux_mt)

  aux_mt1 <- with(aux_mt_df, emmean)

  aux_mt2 <- data.frame(aux_r[1:length(names(aux_r)) - 1],
    means = aux_mt1,
    reps = reps
  )

  aux_mt3 <- aux_mt2[order(aux_mt2[["means"]],
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
    # Only the two-way interaction is of interest

    f1 <- levels(x@frame[[nf2]]) # Factor levels to compare

    f2 <- levels(x@frame[[nf1]])[fl1] # Level of the factor used for the split comparison

    mt <- .tukeyc_filter_nest(aux_mt3, which, fl1, fl2, x@frame)

    row.names(mt) <- paste(mt[, 1],
      mt[, 2],
      sep = "/"
    )
  } # Three-way interaction of interest
  else {
    f1 <- levels(x@frame[[nf3]])

    f2 <- levels(x@frame[[nf2]])[fl2]

    f3 <- levels(x@frame[[nf1]])[fl1]

    mt <- .tukeyc_filter_nest(aux_mt3, which, fl1, fl2, x@frame)

    row.names(mt) <- paste(mt[, 1],
      mt[, 2],
      mt[, 3],
      sep = "/"
    )
  }

  out <- make.TukeyC.test(
    obj = mt,
    MSE = MSE,
    sig.level = sig.level,
    dfr = dfr,
    round = round,
    adjusted.pvalue = adjusted.pvalue
  )

  m.inf <- m.infos.nest.lmerMod(
    x = x,
    my = my,
    forminter = forminter,
    which = which,
    fl1 = fl1,
    fl2 = fl2,
    sig.level = sig.level,
    aux_mt = aux_mt_df
  )

  res <- list(
    out = out,
    info = m.inf
  )
}
