# The Impact of Mixed Modes on Multiple Types of Measurement Error

This is the syntax for the paper: Cernat, A. & Sakshoug, J. (2019) - "The Impact of Mixed Modes on Multiple Types of Measurement Error", Survey Research Methods.

The paper used the Understanding Society Innovation Panel waves 7, 8 and 9. The data used was already cleaned in another paper (will add link to github project when paper is accepted). 

The data was cleaned in R, exported and anlysed in Mplus, and the the results were reimported in R for making graphs and tables.

There are two main R syntaxes:
1. [data_cleaning.R](https://github.com/alex-cernat/MTME-MM/blob/master/data_cleaning.R)
2. [posterior.R](https://github.com/alex-cernat/MTME-MM/blob/master/posterior.R)

The Mplus models are in the folder [Mplus models](https://github.com/alex-cernat/MTME-MM/tree/master/Mplus%20models).

`r pander(sessionInfo())`

**R version 3.6.1 (2019-07-05)**

**Platform:** x86_64-w64-mingw32/x64 (64-bit) 

**locale:**
_LC_COLLATE=English_United Kingdom.1252_, _LC_CTYPE=English_United Kingdom.1252_, _LC_MONETARY=English_United Kingdom.1252_, _LC_NUMERIC=C_ and _LC_TIME=English_United Kingdom.1252_

**attached base packages:** 
_stats_, _graphics_, _grDevices_, _utils_, _datasets_, _methods_ and _base_

**other attached packages:** 
_lavaan(v.0.6-5)_, _gridExtra(v.2.3)_, _ggthemes(v.4.2.0)_, _MplusAutomation(v.0.7-3)_, _MCMCpack(v.1.4-4)_, _MASS(v.7.3-51.4)_, _coda(v.0.19-3)_, _forcats(v.0.4.0)_, _stringr(v.1.4.0)_, _dplyr(v.0.8.3)_, _purrr(v.0.3.2)_, _readr(v.1.3.1)_, _tidyr(v.1.0.0)_, _tibble(v.2.1.3)_, _ggplot2(v.3.2.1)_ and _tidyverse(v.1.2.1)_

**loaded via a namespace (and not attached):** 
_Rcpp(v.1.0.2)_, _lubridate(v.1.7.4)_, _lattice(v.0.20-38)_, _assertthat(v.0.2.1)_, _zeallot(v.0.1.0)_, _packrat(v.0.5.0)_, _digest(v.0.6.21)_, _R6(v.2.4.0)_, _cellranger(v.1.1.0)_, _plyr(v.1.8.4)_, _backports(v.1.1.4)_, _MatrixModels(v.0.4-1)_, _stats4(v.3.6.1)_, _httr(v.1.4.1)_, _pillar(v.1.4.2)_, _rlang(v.0.4.0)_, _lazyeval(v.0.2.2)_, _readxl(v.1.3.1)_, _rstudioapi(v.0.10)_, _data.table(v.1.12.2)_, _SparseM(v.1.77)_, _texreg(v.1.36.23)_, _Matrix(v.1.2-17)_, _pbivnorm(v.0.6.0)_, _gsubfn(v.0.7)_, _proto(v.1.0.0)_, _pander(v.0.6.3)_, _munsell(v.0.5.0)_, _broom(v.0.5.2)_, _compiler(v.3.6.1)_, _modelr(v.0.1.5)_, _pkgconfig(v.2.0.3)_, _mnormt(v.1.5-5)_, _mcmc(v.0.9-6)_, _tidyselect(v.0.2.5)_, _crayon(v.1.3.4)_, _withr(v.2.1.2)_, _grid(v.3.6.1)_, _nlme(v.3.1-140)_, _jsonlite(v.1.6)_, _xtable(v.1.8-4)_, _gtable(v.0.3.0)_, _lifecycle(v.0.1.0)_, _magrittr(v.1.5)_, _scales(v.1.0.0)_, _cli(v.1.1.0)_, _stringi(v.1.4.3)_, _xml2(v.1.2.2)_, _generics(v.0.0.2)_, _vctrs(v.0.2.0)_, _boot(v.1.3-22)_, _tools(v.3.6.1)_, _glue(v.1.3.1)_, _hms(v.0.5.1)_, _parallel(v.3.6.1)_, _colorspace(v.1.4-1)_, _rvest(v.0.3.4)_, _haven(v.2.1.1)_ and _quantreg(v.5.51)_
