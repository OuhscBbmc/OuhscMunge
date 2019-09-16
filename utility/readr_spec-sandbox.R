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
    right   = sub("\\s+(.+)\\s+=\\s+(.+)$", "\\2", value),

    # Pad an odd number of spaces -just beyond the longest variable name.
    padding = nchar(sub("^(.+) = .+", "\\1", value)),
    padding = max(padding) %/%2 * 2 + 1,
    aligned = sprintf("  %-*s = %s", padding, left, right)
  ) %>%
  dplyr::select(-left, -right, -padding) %>%
  dplyr::pull(aligned) %>%
  paste(collapse="\n") %>%
  paste0(
    "col_types <- readr::cols_only(\n",
    .,
    "\n)\n"
  ) %>%
  cat()

