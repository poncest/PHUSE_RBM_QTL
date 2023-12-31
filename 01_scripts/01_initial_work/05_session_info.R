

session_info <- sessioninfo::session_info(include_base = TRUE) 
session_info

# if needed
#writeLines(capture.output(print(session_info)), "session_info.txt")

# ─ Session info ─────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-12-10
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────
# ! package        * version  date (UTC) lib source
# base           * 4.3.1    2023-06-16 [2] local
# P camcorder        0.1.0    2022-10-03 [?] CRAN (R 4.3.0)
# P cli              3.6.1    2023-03-23 [?] CRAN (R 4.3.0)
# P colorspace       2.1-0    2023-01-23 [?] CRAN (R 4.3.0)
# P compiler         4.3.1    2023-06-16 [?] local
# P data.table     * 1.14.8   2023-02-17 [?] CRAN (R 4.3.0)
# P datasets       * 4.3.1    2023-06-16 [?] local
# P distributional   0.3.2    2023-03-22 [?] CRAN (R 4.3.0)
# P dplyr          * 1.1.3    2023-09-03 [?] CRAN (R 4.3.1)
# P extrafont        0.19     2023-01-18 [?] CRAN (R 4.3.0)
# P extrafontdb      1.0      2012-06-11 [?] CRAN (R 4.3.0)
# P fansi            1.0.5    2023-10-08 [?] CRAN (R 4.3.2)
# P farver           2.1.1    2022-07-06 [?] CRAN (R 4.3.0)
# P forcats        * 1.0.0    2023-01-29 [?] CRAN (R 4.3.0)
# P generics         0.1.3    2022-07-05 [?] CRAN (R 4.3.0)
# P ggdist         * 3.3.0    2023-05-13 [?] CRAN (R 4.3.0)
# P ggplot2        * 3.4.4    2023-10-12 [?] CRAN (R 4.3.2)
# P ggrepel          0.9.4    2023-10-13 [?] CRAN (R 4.3.2)
# P ggtext         * 0.1.2    2022-09-16 [?] CRAN (R 4.3.0)
# P gifski           1.12.0-2 2023-08-12 [?] CRAN (R 4.3.1)
# P glue             1.6.2    2022-02-24 [?] CRAN (R 4.3.0)
# P graphics       * 4.3.1    2023-06-16 [?] local
# P grDevices      * 4.3.1    2023-06-16 [?] local
# P grid             4.3.1    2023-06-16 [?] local
# P gridExtra        2.3      2017-09-09 [?] CRAN (R 4.3.0)
# P gridtext         0.1.5    2022-09-16 [?] CRAN (R 4.3.0)
# P gtable           0.3.4    2023-08-21 [?] CRAN (R 4.3.1)
# P here           * 1.0.1    2020-12-13 [?] CRAN (R 4.3.0)
# P hms              1.1.3    2023-03-21 [?] CRAN (R 4.3.0)
# P janitor        * 2.2.0    2023-02-02 [?] CRAN (R 4.3.0)
# P jsonlite         1.8.7    2023-06-29 [?] CRAN (R 4.3.1)
# P lifecycle        1.0.4    2023-11-07 [?] CRAN (R 4.3.2)
# P lubridate      * 1.9.2    2023-02-10 [?] CRAN (R 4.3.0)
# P magick           2.7.5    2023-08-07 [?] CRAN (R 4.3.1)
# P magrittr         2.0.3    2022-03-30 [?] CRAN (R 4.3.0)
# P methods        * 4.3.1    2023-06-16 [?] local
# P munsell          0.5.0    2018-06-12 [?] CRAN (R 4.3.0)
# P pacman           0.5.1    2019-03-11 [?] CRAN (R 4.3.0)
# P pillar           1.9.0    2023-03-22 [?] CRAN (R 4.3.0)
# P pkgconfig        2.0.3    2019-09-22 [?] CRAN (R 4.3.0)
# P purrr          * 1.0.2    2023-08-10 [?] CRAN (R 4.3.1)
# P R6               2.5.1    2021-08-19 [?] CRAN (R 4.3.0)
# P Rcpp             1.0.11   2023-07-06 [?] CRAN (R 4.3.1)
# P readr          * 2.1.4    2023-02-10 [?] CRAN (R 4.3.0)
# renv             1.0.3    2023-09-19 [1] CRAN (R 4.3.2)
# P rlang            1.1.2    2023-11-04 [?] CRAN (R 4.3.2)
# P rprojroot        2.0.3    2022-04-02 [?] CRAN (R 4.3.0)
# P rstudioapi       0.15.0   2023-07-07 [?] CRAN (R 4.3.1)
# P rsvg             2.4.0    2022-11-21 [?] CRAN (R 4.3.0)
# P Rttf2pt1         1.3.12   2023-01-22 [?] CRAN (R 4.3.0)
# P scales         * 1.3.0    2023-11-28 [?] CRAN (R 4.3.2)
# P sessioninfo      1.2.2    2021-12-06 [?] CRAN (R 4.3.0)
# P showtext       * 0.9-6    2023-05-03 [?] CRAN (R 4.3.0)
# P showtextdb     * 3.0      2020-06-04 [?] CRAN (R 4.3.0)
# P snakecase        0.11.1   2023-08-27 [?] CRAN (R 4.3.1)
# P stats          * 4.3.1    2023-06-16 [?] local
# P stringi          1.7.12   2023-01-11 [?] CRAN (R 4.3.0)
# P stringr        * 1.5.0    2022-12-02 [?] CRAN (R 4.3.0)
# P svglite          2.1.1    2023-01-10 [?] CRAN (R 4.3.0)
# P sysfonts       * 0.8.8    2022-03-13 [?] CRAN (R 4.3.0)
# P systemfonts      1.0.4    2022-02-11 [?] CRAN (R 4.3.0)
# P tibble         * 3.2.1    2023-03-20 [?] CRAN (R 4.3.0)
# P tidyr          * 1.3.0    2023-01-24 [?] CRAN (R 4.3.0)
# P tidyselect       1.2.0    2022-10-10 [?] CRAN (R 4.3.0)
# P tidyverse      * 2.0.0    2023-02-22 [?] CRAN (R 4.3.0)
# P timechange       0.2.0    2023-01-11 [?] CRAN (R 4.3.0)
# P tools            4.3.1    2023-06-16 [?] local
# P tzdb             0.4.0    2023-05-12 [?] CRAN (R 4.3.0)
# P urbnthemes     * 0.0.2    2023-11-17 [?] Github (UrbanInstitute/urbnthemes@f6a368d)
# P utf8             1.2.4    2023-10-22 [?] CRAN (R 4.3.2)
# P utils          * 4.3.1    2023-06-16 [?] local
# P vctrs            0.6.5    2023-12-01 [?] CRAN (R 4.3.2)
# P withr            2.5.2    2023-10-30 [?] CRAN (R 4.3.2)
# P xml2             1.3.5    2023-07-06 [?] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/work/PHUSE_RBM_QTL/renv/library/R-4.3/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/da822c09
# 
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────────────
# > 
