##
## Function to group means
##
make.TukeyC.groups <- function(x) {
  ## ++++++++
  # Remove duplicate columns (no point keeping identical grouping columns):
  # t1  a  b
  # t2  a  b
  # (example: all rows labeled "a")
  x[upper.tri(x)] <- FALSE
  auxx <- t(x)
  auxy <- unique(auxx)
  newmat <- t(auxy)
  ## ++++++++

  ncolnew <- ncol(newmat)
  nrownew <- nrow(newmat)

  ## +++++++++
  # Mark the first row from which each letter column should be filled
  mat24 <- sapply(colnames(newmat), function(x) x == rownames(newmat))
  mat25 <- which(mat24 == TRUE, arr.ind = TRUE)
  ## +++++++++

  ## +++++++++
  # Build the indicator matrix for letter assignment
  matzero <- matrix(NA, nrow = nrownew, ncol = ncolnew)
  for (i in 1:dim(mat25)[1]) {
    matzero[seq(mat25[i, 1], nrownew), i] <- newmat[seq(mat25[i, 1], nrownew), i]
  }
  ## +++++++++

  ## +++++++++
  # Keep only the columns that will actually receive letters
  aux <- apply(matzero, 2, function(x) all(x == FALSE, na.rm = TRUE))
  aux3 <- matzero[, 1:(length(aux[aux == FALSE]) + 1)]
  ## +++++++++

  ## +++++++++
  # When Tukey is run without a prior ANOVA, treatments may be equal; guard below
  if (!is.vector(aux3)) {
    matreal <- apply(aux3, 2, function(x) gsub(TRUE, "", x))
  } else {
    matreal <- matzero
  }

  matreal[is.na(matreal)] <- ""

  ## +++++++++
  # Letter pool: R letters plus suffixes extend beyond 52 pairwise groups
  letras <- c(letters, paste(letters, rep(0:9, rep(26, 10)), sep = ""))
  ## +++++++++

  matnew <- lapply(
    1:dim(matreal)[2],
    function(i) gsub(FALSE, letras[i], matreal[, i])
  )
  matnew1 <- do.call("cbind", matnew)
}
