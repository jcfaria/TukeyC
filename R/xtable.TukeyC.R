xtable.TukeyC <- function(x, ...) {
  aux1 <- x$out$Result
  aux2 <- c(
    x$out$MSD[2, 1],
    rep(NA, nrow(aux1) - 1)
  )
  aux3 <- c(
    x$out$Sig.level,
    rep(NA, nrow(aux1) - 1)
  )

  ## Move row names to an explicit column so that print.xtable does not
  ## generate row-name cells with inconsistent leading-space indentation.
  aux4 <- data.frame(
    Treatment = rownames(aux1),
    aux1,
    `Minimum Significant Difference` = aux2,
    Sig.level = aux3,
    row.names = NULL,
    check.names = FALSE
  )

  res <- xtable::xtable(aux4, ...)
  class(res) <- c("xtable.TukeyC", "xtable", "data.frame")
  return(res)
}

print.xtable.TukeyC <- function(x, include.rownames = FALSE, ...) {
  ## Capture the raw LaTeX/HTML output from print.xtable.
  out <- utils::capture.output(
    xtable::print.xtable(x,
      include.rownames = include.rownames,
      ...
    )
  )

  ## print.xtable indents all data rows after the first with two leading
  ## spaces, which become significant whitespace inside tabular cells and
  ## cause visual misalignment in the compiled document.  Strip that prefix.
  out <- gsub("^ {2}([^\\\\%])", "\\1",
    out,
    perl = TRUE
  )

  cat(out,
    sep = "\n"
  )
  invisible(x)
}
