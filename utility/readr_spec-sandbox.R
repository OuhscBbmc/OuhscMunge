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
    value   = sub("(\\s+)([^`]+?) = ", "\\1`\\2` = ", value),
    left    = sub("\\s+(.+)\\s+=\\s+(.+)$", "\\1", value),
    right   = sub("\\s+(.+)\\s+=\\s+(.+)$", "\, left, right)

  ) %>%
\2", value),
    l1           = nchar(sub("^(.+) = .+", "\\1", value)),

    v = sprintf("  %-*s = %s", max(l1), left, right)

  ) %>%
  dplyr::select(-left, -right)

