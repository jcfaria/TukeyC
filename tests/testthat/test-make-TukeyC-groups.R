test_that("make.TukeyC.groups respects all pairwise comparisons", {
  eq <- matrix(
    c(
      FALSE, TRUE, FALSE,
      TRUE, FALSE, TRUE,
      FALSE, TRUE, FALSE
    ),
    3, 3,
    byrow = TRUE,
    dimnames = list(paste0("T", 1:3), paste0("T", 1:3))
  )

  g <- TukeyC:::make.TukeyC.groups(eq)

  expect_false(any(g[1, ] != "" & g[2, ] != "" & g[1, ] == g[2, ]))
  expect_false(any(g[2, ] != "" & g[3, ] != "" & g[2, ] == g[3, ]))
  expect_true(any(g[1, ] != "" & g[3, ] != "" & g[1, ] == g[3, ]))
})

test_that("make.TukeyC.groups matches TukeyC on unbalanced CRD subset", {
  data(CRD2, package = "TukeyC")
  u <- CRD2$dfm
  u <- u[as.integer(u$x) <= 12, ]
  u[c(3, 5, 10), "y"] <- NA

  tk <- TukeyC(y ~ x, data = u, which = "x")
  o <- unclass(tk$out)
  difm <- as.matrix(o$Diff_Prob)
  difm[upper.tri(difm)] <- t(difm)[upper.tri(difm)]
  eq <- ifelse(difm >= o$Sig.level, FALSE, TRUE)
  diag(eq) <- FALSE

  g <- as.matrix(o$Result[, -1, drop = FALSE])
  n <- nrow(eq)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      shared <- any(g[i, ] != "" & g[j, ] != "" & g[i, ] == g[j, ])
      if (!eq[i, j]) {
        expect_true(shared)
      } else {
        expect_false(shared)
      }
    }
  }
})

test_that("make.TukeyC.test returns Result with Means column", {
  obj <- data.frame(
    means = c(10, 8, 6),
    reps = c(5, 5, 5),
    row.names = c("A", "B", "C")
  )
  out <- TukeyC:::make.TukeyC.test(
    obj = obj,
    MSE = 1,
    sig.level = 0.05,
    dfr = 10,
    round = 2,
    adjusted.pvalue = "none"
  )
  expect_true("Means" %in% names(out$Result))
  expect_s3_class(out$Result, "data.frame")
})
