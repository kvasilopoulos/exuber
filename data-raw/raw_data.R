require("readxl")

sp500 <- read_excel("data-raw/SP_DV.xlsx",
                    col_types = c("text", "numeric", "numeric",
                                  "numeric", "numeric")) %>%
  na.omit() %>%
  dplyr::mutate(Date = Date %>%
                   zoo::as.yearmon("%Y.%m") %>%
                   lubridate::myd(truncated = 1)) %>%
  as.data.frame()

testSP500 <- radf(sp500 %>% select(Date, PVratio))
critSP500 <- mc_cv(NROW(sp500), parallel = TRUE)

usethis::use_data(sp500, overwrite = T)
usethis::use_data(testSP500, overwrite = T)
usethis::use_data(critSP500, overwrite = T)
