
#' Author: Steven Ponce
#' Date: 2024-04-02

session_info <- sessioninfo::session_info(include_base = TRUE) 
session_info

# if needed
writeLines(capture.output(print(session_info)), "session_info.txt")

# ─ Session info ────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-05-03
# rstudio  2024.04.0+735 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────
# ! package        * version  date (UTC) lib source
# base           * 4.4.0    2024-04-24 [2] local
# P camcorder        0.1.0    2022-10-03 [?] RSPM (R 4.4.0)
# cli              3.6.2    2023-12-11 [1] RSPM (R 4.4.0)
# colorspace       2.1-0    2023-01-23 [1] RSPM (R 4.4.0)
# P compiler         4.4.0    2024-04-24 [?] local
# P datasets       * 4.4.0    2024-04-24 [?] local
# P distributional   0.4.0    2024-02-07 [?] RSPM (R 4.4.0)
# P dplyr            1.1.4    2023-11-17 [?] RSPM (R 4.4.0)
# extrafont        0.19     2023-01-18 [1] RSPM (R 4.4.0)
# extrafontdb      1.0      2012-06-11 [1] RSPM (R 4.4.0)
# fansi            1.0.6    2023-12-08 [1] RSPM (R 4.4.0)
# P generics         0.1.3    2022-07-05 [?] RSPM (R 4.4.0)
# P ggdist           3.3.2    2024-03-05 [?] RSPM (R 4.4.0)
# ggplot2          3.5.1    2024-04-23 [1] RSPM (R 4.4.0)
# ggrepel          0.9.5    2024-01-10 [1] RSPM (R 4.4.0)
# P gifski           1.12.0-2 2023-08-12 [?] RSPM (R 4.4.0)
# glue             1.7.0    2024-01-09 [1] RSPM (R 4.4.0)
# P graphics       * 4.4.0    2024-04-24 [?] local
# P grDevices      * 4.4.0    2024-04-24 [?] local
# P grid             4.4.0    2024-04-24 [?] local
# gridExtra        2.3      2017-09-09 [1] RSPM (R 4.4.0)
# gtable           0.3.5    2024-04-22 [1] RSPM (R 4.4.0)
# P jsonlite         1.8.8    2023-12-04 [?] RSPM (R 4.4.0)
# lifecycle        1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# P magick           2.8.3    2024-02-18 [?] RSPM (R 4.4.0)
# magrittr         2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# P methods        * 4.4.0    2024-04-24 [?] local
# munsell          0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P pacman           0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# pillar           1.9.0    2023-03-22 [1] RSPM (R 4.4.0)
# pkgconfig        2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# R6               2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# Rcpp             1.0.12   2024-01-09 [1] RSPM (R 4.4.0)
# renv             1.0.3    2023-09-19 [1] CRAN (R 4.4.0)
# rlang            1.1.3    2024-01-10 [1] RSPM (R 4.4.0)
# P rstudioapi       0.16.0   2024-03-24 [?] RSPM (R 4.4.0)
# P rsvg             2.6.0    2023-10-08 [?] RSPM (R 4.4.0)
# Rttf2pt1         1.3.12   2023-01-22 [1] RSPM (R 4.4.0)
# scales           1.3.0    2023-11-28 [1] RSPM (R 4.4.0)
# P sessioninfo      1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P stats          * 4.4.0    2024-04-24 [?] local
# P svglite          2.1.3    2023-12-08 [?] RSPM (R 4.4.0)
# systemfonts      1.0.6    2024-03-07 [1] RSPM (R 4.4.0)
# tibble           3.2.1    2023-03-20 [1] RSPM (R 4.4.0)
# P tidyselect       1.2.1    2024-03-11 [?] RSPM (R 4.4.0)
# P tools            4.4.0    2024-04-24 [?] local
# urbnthemes       0.0.2    2024-05-03 [1] Github (UrbanInstitute/urbnthemes@f6a368d)
# utf8             1.2.4    2023-10-22 [1] RSPM (R 4.4.0)
# P utils          * 4.4.0    2024-04-24 [?] local
# vctrs            0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/work/PHUSE_RBM_QTL/renv/library/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.4/x86_64-w64-mingw32/ebed5364
# 
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────
# > 