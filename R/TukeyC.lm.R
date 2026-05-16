TukeyC.lm <- function(x,
                      which = NULL,
                      fl1 = NULL,
                      fl2 = NULL,
                      error = NULL,
                      sig.level = .05,
                      round = 2,
                      adjusted.pvalue = "none", ...) {
  if (is.null(which)) {
    which <- names(x$model)[2]
  }

  # Interactions with experimental error
  if (!is.null(fl1) & is.null(error)) {
    SSE <- deviance(x) # sum square error
    dfr <- df.residual(x) # residual degrees of freedom
    MSE <- SSE / dfr

    cl <- match.call()
    class(x) <- c("nest.lm", class(x))

    res <- TukeyC(
      x = x,
      which = which,
      fl1 = fl1,
      fl2 = fl2,
      MSE = MSE,
      dfr = dfr,
      sig.level = sig.level,
      round = round,
      adjusted.pvalue = adjusted.pvalue,
      ...
    )

    res$call <- cl
    class(res) <- c(
      "TukeyC.lm",
      "TukeyC",
      "list"
    )

    return(res)
  }

  # Interactions with other error strata
  if (!is.null(fl1) & !is.null(error)) {
    many_errors <- unlist(strsplit(
      error,
      "\\/"
    ))

    many_errors <- .tukeyc_replace_within_errors(many_errors)

    n_errors <- length(many_errors)

    if (n_errors > 1) { # Combined error terms

      aux_MSE <- NULL
      aux_dfr <- NULL
      anov <- anova(x)

      for (i in 1:n_errors) {
        aux_MSE[i] <- anov[rownames(anov) == many_errors[i], ][1, 3]
        aux_dfr[i] <- anov[rownames(anov) == many_errors[i], ][1, 1]
      }

      factors <- unlist(strsplit(
        which,
        "[[:punct:]]"
      ))

      aux_levels <- x$xlevel

      aux_levels1 <- lapply(
        aux_levels,
        length
      )

      levelss <- unlist(aux_levels1[factors])

      if (length(aux_MSE) == 2) {
        cp <- c(
          levelss[1] - 1,
          1
        )

        MSE <- (cp %*% aux_MSE) / levelss[1]

        numer <- (cp %*% aux_MSE)^2
        denom <- (cp[1] * aux_MSE[1])^2 / aux_dfr[1] + aux_MSE[2]^2 / aux_dfr[2]
        dfr <- numer / denom
      } else {
        cp <- c(
          levelss[2] * (levelss[1] - 1),
          levelss[2] - 1,
          1
        )

        MSE <- (cp %*% aux_MSE) / prod(levelss[1:2])

        numer <- (cp %*% aux_MSE)^2
        denom <- (cp[1] * aux_MSE[1])^2 / aux_dfr[1] + (cp[2] * aux_MSE[2])^2 / aux_dfr[2] + aux_MSE[3]^2 / aux_dfr[3]
        dfr <- numer / denom
      }
    } else { # No combined error terms

      anov <- anova(x)
      SSE <- anov[rownames(anov) == error, ][1, 2] # sum square error
      dfr <- anov[rownames(anov) == error, ][1, 1] # residual degrees of freedom
      MSE <- SSE / dfr
    }

    cl <- match.call()
    class(x) <- c("nest.lm", class(x))

    res <- TukeyC(
      x = x,
      which = which,
      fl1 = fl1,
      fl2 = fl2,
      MSE = MSE,
      dfr = dfr,
      sig.level = sig.level,
      round = round,
      adjusted.pvalue = adjusted.pvalue,
      ...
    )

    res$call <- cl
    class(res) <- c(
      "TukeyC.lm",
      "TukeyC",
      "list"
    )

    return(res)
  }

  # No interaction terms of interest here
  if (is.null(fl1) & !is.null(error)) { # User-specified error term

    anov <- anova(x)
    SSE <- anov[rownames(anov) == error, ][1, 2] # sum square error
    dfr <- anov[rownames(anov) == error, ][1, 1] # residual degrees of freedom
    MSE <- SSE / dfr
  } else { # Experimental error

    SSE <- deviance(x) # sum square error
    dfr <- df.residual(x) # residual degrees of freedom
    MSE <- SSE / dfr
  }

  my <- as.character(formula(x)[[2]])

  forminter <- as.formula(paste(
    my,
    "~",
    which
  ))

  aux_r <- aggregate(forminter,
    data = x$model,
    function(x) r <- length(x)
  )
  reps <- aux_r[[my]]

  aux_mt <- suppressMessages(emmeans::emmeans(x,
    specs = which,
    level = 1 - sig.level
  ))

  # aux_mt1 <- with(aux_mt,estimate)
  aux_mt_df <- as.data.frame(aux_mt)

  aux_mt1 <- with(aux_mt_df, emmean)

  aux_mt2 <- data.frame(
    means = aux_mt1,
    reps = reps
  )

  row.names(aux_mt2) <- aux_r[, 1]

  mt <- aux_mt2[order(aux_mt2[, 1],
    decreasing = TRUE
  ), ]

  cl <- match.call()
  out <- make.TukeyC.test(
    obj = mt,
    MSE = MSE,
    sig.level = sig.level,
    dfr = dfr,
    round = round,
    adjusted.pvalue = adjusted.pvalue
  )

  m.inf <- m.infos.lm(
    x = x,
    my = my,
    forminter = forminter,
    which = which,
    sig.level = sig.level,
    aux_mt = aux_mt_df
  )

  res <- list(
    out = out,
    info = m.inf
  )

  res$call <- cl

  class(res) <- c(
    "TukeyC.lm",
    "TukeyC",
    "list"
  )
  return(res)
}
