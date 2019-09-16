library(magrittr)
# dplyr::starwars %>%
#   readr::write_csv()
readr::spec_csv("inst/test-data/subject-1.csv") %>%
  capture.output() %>%
  tibble::as_tibble() %>%
  dplyr::slice(-1, -nrow(.)) %>%
  dplyr::mutate(
    value       = sub(" = ", " = readr::", value),
    # has_ticks   = grepl("\\s+`", value),
    # qualified   = sub("\\s+(.+?) = ", "`\\1`", value),
    qualified   = sub("(\\s+)([^`]+?) = ", "\\1`\\2` = ", value),
    l1           = sub("^(.+) = .+", "\\1", qualified),
    l2          = nchar(l1)

  ) %>%
  dplyr::select(-value)

