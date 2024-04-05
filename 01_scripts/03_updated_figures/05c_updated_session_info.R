
#' Author: Steven Ponce
#' Date: 2024-04-02

session_info <- sessioninfo::session_info(include_base = TRUE) 
session_info

# if needed
writeLines(capture.output(print(session_info)), "session_info.txt")

# ─ Session info ────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-11-17
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────
# package        * version  date (UTC) lib source
# base           * 4.3.1    2023-06-16 [2] local
# camcorder        0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cli              3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace       2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# compiler         4.3.1    2023-06-16 [2] local
# data.table     * 1.14.8   2023-02-17 [1] CRAN (R 4.3.0)
# datasets       * 4.3.1    2023-06-16 [2] local
# distributional   0.3.2    2023-03-22 [1] CRAN (R 4.3.0)
# dplyr          * 1.1.3    2023-09-03 [1] CRAN (R 4.3.1)
# extrafont        0.19     2023-01-18 [1] CRAN (R 4.3.0)
# extrafontdb      1.0      2012-06-11 [1] CRAN (R 4.3.0)
# fansi            1.0.4    2023-01-22 [1] CRAN (R 4.3.0)
# farver           2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# forcats        * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# generics         0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggdist         * 3.3.0    2023-05-13 [1] CRAN (R 4.3.0)
# ggplot2        * 3.4.3    2023-08-14 [1] CRAN (R 4.3.1)
# ggrepel          0.9.4    2023-10-13 [1] CRAN (R 4.3.2)
# ggtext         * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski           1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue             1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics       * 4.3.1    2023-06-16 [2] local
# grDevices      * 4.3.1    2023-06-16 [2] local
# grid             4.3.1    2023-06-16 [2] local
# gridExtra        2.3      2017-09-09 [1] CRAN (R 4.3.0)
# gridtext         0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable           0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here           * 1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms              1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# janitor        * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite         1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# lifecycle        1.0.4    2023-11-07 [1] CRAN (R 4.3.2)
# lubridate      * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick           2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr         2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# methods        * 4.3.1    2023-06-16 [2] local
# munsell          0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman           0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# pillar           1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig        2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# purrr          * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6               2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# Rcpp             1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr          * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# rlang            1.1.1    2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot        2.0.3    2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi       0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg             2.4.0    2022-11-21 [1] CRAN (R 4.3.0)
# Rttf2pt1         1.3.12   2023-01-22 [1] CRAN (R 4.3.0)
# scales         * 1.2.1    2022-08-20 [1] CRAN (R 4.3.1)
# sessioninfo      1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext       * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb     * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# snakecase        0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# stats          * 4.3.1    2023-06-16 [2] local
# stringi          1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr        * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite          2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts       * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts      1.0.4    2022-02-11 [1] CRAN (R 4.3.0)
# tibble         * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidyr          * 1.3.0    2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect       1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidyverse      * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange       0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools            4.3.1    2023-06-16 [2] local
# tzdb             0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# urbnthemes     * 0.0.2    2023-11-17 [1] Github (UrbanInstitute/urbnthemes@f6a368d)
# utf8             1.2.3    2023-01-31 [1] CRAN (R 4.3.0)
# utils          * 4.3.1    2023-06-16 [2] local
# vctrs            0.6.3    2023-06-14 [1] CRAN (R 4.3.1)
# withr            2.5.2    2023-10-30 [1] CRAN (R 4.3.2)
# xml2             1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ───────────────────────────────────────────────────────────────────────
# > 
  
  
