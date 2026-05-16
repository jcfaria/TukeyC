# TukeyC 1.4-0 (2026-05-16)

## New Features

- Added a basic `testthat` test suite covering `TukeyC` with `formula`, `aov`,
  and `lm` inputs for balanced and unbalanced designs, as well as `summary`,
  `print`, `plot`, and `xtable` methods.
- Added two vignettes: `TukeyC_intro` (HTML, via R Markdown) and
  `TukeyC_intro_pdf` (PDF, via knitr/LaTeX), covering quick start, accepted
  input classes, unbalanced data, RCBD, factorial and split-plot designs,
  visualisation options, tabular output, and mixed models.

## Changes

- `README.md` fully rewritten: badges (CRAN status, downloads, checks,
  lifecycle, licence), key features, installation instructions (CRAN and
  GitHub), quick-start examples, project layout, contributing guide, and
  roadmap.
- `DESCRIPTION`: version bumped to 1.4-0; author fields standardised to match
  the ScottKnott package template; `R (>= 4.0.0)`; added `BugReports` URL and
  LEC software page; added `knitr`, `rmarkdown`, and `testthat (>= 3.0.0)` to
  `Suggests`; set `VignetteBuilder: knitr` and `Config/testthat/edition: 3`.
- Added `NEWS.md` (this file), migrated from the historical `ChangeLog`.
- `plot.TukeyC()`: removed the `none` dispersion option; default is now `mm`
  (min–max range), matching `plot.SK()`.
- `man/`: author fields standardised to scientific name order (Faria, J. C.;
  Jelihovschi, E. G.; Allaman, I. B.), matching the ScottKnott package.
- Removed `ChangeLog`; release history is maintained in `NEWS.md` only.

---

# TukeyC 1.3-44 (2026-02-23)

## Changes

- Removed the dependency on the `doBy` package. `doBy::LSmeans` produced
  unwanted side-effects in the console and in Sweave/knitr documents.
- Added the `emmeans` package as a dependency.
- All internal LSmeans calls have been replaced with `emmeans`.

---

# TukeyC 1.3-43 (2025-04-02)

## Bug Fixes

- Fixed an `mtext` call that was incorrectly placed outside the scope of the
  `axis1` argument.
- Fixed missing package anchors in `xtable.TukeyC.Rd`.
- In all functions returning an `LSmeans` object, replaced `aux_mt$coef[,1]`
  with `with(aux_mt, estimate)`.
- In `boxplot.TukeyC` and `plot.TukeyC`, replaced `par(mar = op)` with
  `on.exit(par(mar = np))` to restore graphics parameters reliably.

## Changes

- The English in the entire documentation has been reviewed.

---

# TukeyC 1.3-42 (2023-08-30)

## Changes

- Files adjusted to meet CRAN requirements.

---

# TukeyC 1.3-41 (2023-06-21)

## New Features

- Added new arguments `axis1` and `axis2` to the `plot` function, giving
  users greater flexibility to customise their own axes.

## Changes

- Files adjusted to meet CRAN requirements.

---

# TukeyC 1.3-4 (2020-11-04)

## Bug Fixes

- Fixed bug in `TukeyC.lmerMod`: corrected the calculation of MSE and `dfr`.
- Fixed bug in `TukeyC.lmerMod`, `TukeyC.lm`, and `TukeyC.aovlist`: substituted
  `levelss` with `aux_MSE` for split-plot and split-split-plot designs.
- Fixed bug in `plot.TukeyC` and `boxplot.TukeyC`: in `plot.TukeyC`, changed
  `names(x$out$Result[,1])` to `row.names(x$out$Result)`; in
  `boxplot.TukeyC`, added `treat <- 'treat'`.
- Fixed bug in `m.infos.lmerMod`: corrected the degrees of freedom column used
  for confidence intervals (`aux_mt$coef[,3]` → `aux_mt$coef[,4]`).

## New Features

- Added `xtable.TukeyC` method and corresponding documentation.

## Changes

- Fixed warning in `boxplot.TukeyC.Rd`.
- Improvements in `TukeyC.Rd`, `plot.TukeyC.Rd`, and `boxplot.TukeyC.Rd`.
- `NAMESPACE` adjustment.

---

# TukeyC 1.3-3 (2019-01-15)

## New Features

- Added `boxplot.TukeyC` method.

---

# TukeyC 1.3-2 (2018-11-05)

## Bug Fixes

- Fixed bug in `make.TukeyC.groups` that caused an infinite loop with severely
  unbalanced data.

## Changes

- The algorithm for `make.TukeyC.groups` was replaced.
- Added the `parallel` package as an import (later removed for CRAN policy).

---

# TukeyC 1.3-1 (2018-01-13)

## Changes

- Overall improvements in the demos, examples, and documentation.
- Version number adjusted.

---

# TukeyC 1.3-0 (2018-01-12)

## Bug Fixes

- Fixed warning in `make.TukeyC.test`: `vece` object now uses `as.vector(MSE)`.

## Changes

- Added `lme4` and `pbkrtest` packages to `Suggests`.

---

# TukeyC 1.2-8 (2018-01-09)

## Bug Fixes

- Bug fixed in `TukeyC.nest.aovlist`.

## New Features

- Added `TukeyC.lmerMod` method.

## Changes

- Added `lme4` to `Imports` in `DESCRIPTION` (later moved to `Suggests`).

---

# TukeyC 1.2-7 (2017-09-13)

## New Features

- Added `ci` (confidence interval) and `cip` (pooled confidence interval)
  dispersion options to `plot.TukeyC`.
- Added `match.call()` internally to `TukeyC.lm` to retrieve arguments.

---

# TukeyC 1.2-6 (2016-11-21)

## New Features

- Added the `adjusted.pvalue` argument to `TukeyC`.
- Added methods `print.TukeyC`, `TukeyC.formula`, and `TukeyC.lm`.
- Replaced `TukeyC.aov` with `TukeyC.lm`.
- Replaced `m.inf.1a`, `m.inf.1b`, `m.inf.2a`, `m.inf.2b`, `m.inf.3a`, and
  `m.inf.3b` with `m.infos.lm`, `m.infos.aovlist`, `m.infos.nest.lm`, and
  `m.infos.nest.aovlist`.

## Changes

- Removed the `dispersion` argument from `TukeyC` and added it to `plot`.
- `TukeyC.nest.lm` and `TukeyC.nest.aovlist` are now internal functions.
- The `TukeyC.nest` functions were optimised.
- Added the `doBy` package as a dependency (replaced by `emmeans` in 1.3-44).
- Added the official URL for package development to `DESCRIPTION`.

---

# TukeyC 1.2-5 (2014-08-16)

## New Features

- Added `cv` function to calculate the coefficient of variation for `lm`, `aov`,
  and `aovlist` objects.

## Changes

- Small changes were made to meet CRAN requirements.

---

# TukeyC 1.2-4 (2013-11-21)

## Changes

- Replaced all direct calls to the deprecated `model.frame.aovlist` method
  with calls to the generic, to meet CRAN requirements.
- Released to CRAN.

---

# TukeyC 1.2-3 (2013-04-16)

## Changes

- Source code substantially improved.
- Documentation substantially updated.

---

# TukeyC 1.2-2 (2013-04-11)

## New Features

- New options related to the dispersion of the means.

---

# TukeyC 1.2-1 (2012-12-10)

## Bug Fixes

- Fixed a bug in `TukeyC.nest.aovlist.R`: occurrences of `[:punct:]` (incorrect)
  replaced with `[[:punct:]]` (correct).

## New Features

- New dataset: `SPET` (Split-plot in time).

---

# TukeyC 1.2-0 (2012-10-06)

## Changes

- The notation for FE, SPE, and SSPE experiments was simplified.
- Overall improvements in code, documentation, and demos.

---

# TukeyC 1.1-0 (2012-06-22)

## Changes

- Ivan B. Allaman joined the project as co-author.
- Overall improvements in the documentation.
- First version released on CRAN.

---

# TukeyC 1.0-5 (2012-05-27)

## Changes

- Overall improvements in the demos, examples, and documentation.

---

# TukeyC 1.0-4 (2012-05-25)

## Changes

- The slot `Differences` of the returned list was renamed to `Diff_Prob`.
  The upper triangle of the matrix stores the differences between means and
  the lower triangle stores the respective Tukey-test p-values.

---

# TukeyC 1.0-0 (2010-05-27)

## Changes

- Initial development release (restricted to testers).
