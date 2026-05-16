# TukeyC

`TukeyC` is an R package that implements Tukey's Honestly Significant Difference (HSD) test as a multiple comparison method in the Analysis of Variance (ANOVA) context, including intuitive letter grouping of means for balanced and unbalanced designs.

[![CRAN status](https://www.r-pkg.org/badges/version/TukeyC)](https://cran.r-project.org/package=TukeyC)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/TukeyC)](https://cran.r-project.org/package=TukeyC)
[![CRAN checks](https://badges.cranchecks.info/worst/TukeyC.svg)](https://cran.r-project.org/web/checks/check_results_TukeyC.html)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: GPL (>= 2)](https://img.shields.io/badge/License-GPL%20(%3E%3D%202)-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

## Key Features

- Performs the conventional Tukey HSD test with overlapping letter groups for means.
- Accepts input from `formula`, `aov`, `lm`, `aovlist`, and `lmerMod` objects.
- Supports single, factorial, split-plot, split-plot in time, and split-split-plot experiments.
- Straightforward handling of interactions via `which`, `fl1`, and `fl2`.
- Adjusted means via Least-Squares Means (`emmeans`) for unbalanced data.
- Rich `plot` method with customisable dispersion bands (min–max, SD, CI, pooled CI).
- Reporting support with `xtable`; additional utilities include `boxplot` and `cv`.

## Installation

Install from CRAN:

```r
install.packages("TukeyC")
```

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("jcfaria/TukeyC")
```

## Quick Start

```r
library(TukeyC)

## Completely Randomized Design (CRD) — balanced
data(CRD1)

tk1 <- with(CRD1,
            TukeyC(y ~ x,
                   data = dfm,
                   which = 'x'))
summary(tk1)
plot(tk1,
     dispersion = 'sd',
     d.col = 'steelblue')

## Randomized Complete Block Design (RCBD)
data(RCBD)

tk2 <- with(RCBD,
            TukeyC(y ~ blk + tra,
                   data = dfm,
                   which = 'tra'))
summary(tk2)
plot(tk2,
     dispersion = 'ci',
     d.col = 'red')
```

## Project Layout

- `/R`: Core functions and S3 methods.
- `/man`: Reference documentation (`.Rd` files) and dataset documentation.
- `/demo`: Runnable demos for each experimental design.
- `/inst`: Package citation file.

## Contributing

Contributions are welcome. Open an issue or submit a pull request with:

- Bug fixes and performance improvements.
- Documentation and usability updates.
- New ideas for grouping procedures or graphical displays.

To check and build locally:

```bash
R CMD check TukeyC
R CMD build TukeyC
R CMD INSTALL TukeyC_X.X-X.tar.gz
```

## Roadmap

- Add automated tests (`testthat`) for all experimental designs.
- Expand vignettes covering balanced and unbalanced use cases.
- Keep documentation aligned with current S3 behaviour.

---

Developed by:  
Faria, J. C.; Jelihovschi, E. G.; Allaman, I. B.  
Universidade Estadual de Santa Cruz - UESC  
Departamento de Ciencias Exatas - DCEX  
Ilheus - Bahia - Brasil
