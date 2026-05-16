# test-TukeyC-core.R — basic regression and smoke tests for the TukeyC package.
#
# Organisation:
#   1. Class and components of the TukeyC object
#   2. Balanced designs: formula, aov, lm inputs
#   3. Unbalanced data
#   4. summary and print
#   5. plot.TukeyC (smoke tests)
#   6. xtable.TukeyC

library(TukeyC)

# ===========================================================================
# 1. Class and components
# ===========================================================================

test_that("TukeyC returns an object of class 'TukeyC'", {
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  expect_s3_class(tk, "TukeyC")
})

test_that("TukeyC object contains the expected components", {
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  expect_true(all(c("out", "info") %in% names(tk)))
})

# ===========================================================================
# 2. Balanced designs
# ===========================================================================

test_that("TukeyC works with formula input — CRD", {
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  expect_s3_class(tk, "TukeyC")
})

test_that("TukeyC works with aov input — CRD", {
  data(CRD1)
  av <- with(CRD1, aov(y ~ x, data = dfm))
  tk <- TukeyC(av, which = "x")
  expect_s3_class(tk, "TukeyC")
})

test_that("TukeyC works with lm input — CRD", {
  data(CRD1)
  lm1 <- with(CRD1, lm(y ~ x, data = dfm))
  tk <- TukeyC(lm1, which = "x")
  expect_s3_class(tk, "TukeyC")
})

test_that("TukeyC works with formula input — RCBD", {
  data(RCBD)
  tk <- with(RCBD, TukeyC(y ~ blk + tra, data = dfm, which = "tra"))
  expect_s3_class(tk, "TukeyC")
})

test_that("TukeyC works with aov input — RCBD", {
  data(RCBD)
  av <- with(RCBD, aov(y ~ blk + tra, data = dfm))
  tk <- TukeyC(av, which = "tra")
  expect_s3_class(tk, "TukeyC")
})

test_that("TukeyC respects sig.level argument", {
  data(CRD1)
  tk_05 <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x", sig.level = 0.05))
  tk_01 <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x", sig.level = 0.01))
  # A stricter level must produce at least as many groups as a looser one.
  expect_gte(
    length(unique(tk_01$out$Result[, 2])),
    length(unique(tk_05$out$Result[, 2]))
  )
})

# ===========================================================================
# 3. Unbalanced data
# ===========================================================================

test_that("TukeyC handles unbalanced data via formula — CRD", {
  data(CRD2)
  uCRD2 <- CRD2$dfm
  uCRD2[c(3, 5, 10), 3] <- NA
  tk <- TukeyC(y ~ x, data = uCRD2, which = "x")
  expect_s3_class(tk, "TukeyC")
})

test_that("TukeyC handles unbalanced data via lm — CRD", {
  data(CRD2)
  uCRD2 <- CRD2$dfm
  uCRD2[c(3, 5, 10), 3] <- NA
  lm1 <- lm(y ~ x, data = uCRD2)
  tk <- TukeyC(lm1, which = "x")
  expect_s3_class(tk, "TukeyC")
})

# ===========================================================================
# 4. summary and print
# ===========================================================================

test_that("summary.TukeyC produces output without error", {
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  expect_no_error(summary(tk))
})

test_that("print.TukeyC produces output without error", {
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  expect_no_error(print(tk))
})

# ===========================================================================
# 5. plot.TukeyC
# ===========================================================================

test_that("plot.TukeyC runs without error — basic", {
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  expect_no_error(plot(tk))
})

test_that("plot.TukeyC runs without error for all dispersion options", {
  data(CRD2)
  tk <- with(CRD2, TukeyC(y ~ x, data = dfm, which = "x"))
  for (disp in c("mm", "sd", "ci", "cip"))
    expect_no_error(plot(tk, dispersion = disp))
})

# ===========================================================================
# 6. xtable.TukeyC
# ===========================================================================

test_that("xtable.TukeyC returns an object of class 'xtable'", {
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  xt <- xtable(tk)
  expect_s3_class(xt, "xtable")
})
