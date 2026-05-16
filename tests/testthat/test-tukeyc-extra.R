# Additional tests: boxplot, FE nest, xtable re-export

test_that("boxplot.TukeyC runs without error", {
  data(CRD1)
  tk <- TukeyC(y ~ x, data = CRD1$dfm, which = "x")
  expect_no_error(boxplot(tk))
})

test_that("xtable generic is re-exported from TukeyC", {
  expect_true("xtable" %in% getNamespaceExports("TukeyC"))
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  xt <- xtable(tk)
  expect_s3_class(xt, "xtable")
})

test_that("TukeyC FE nested comparison runs", {
  data(FE)
  tk <- with(FE,
            TukeyC(y ~ blk + N * P * K,
                   data = dfm,
                   which = "P:N",
                   fl1 = 1))
  expect_s3_class(tk, "TukeyC")
})

test_that("plot.TukeyC default dispersion draws segments", {
  data(CRD1)
  tk <- with(CRD1, TukeyC(y ~ x, data = dfm, which = "x"))
  expect_no_error({
    plot(tk, dispersion = "mm")
    plot(tk, dispersion = "sd")
  })
})
