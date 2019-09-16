library(magrittr)
# dplyr::starwars %>%
#   readr::write_csv()
readr::spec_csv("inst/test-data/subject-1.csv") %>%
  capture.output() %>%
  tibble::as_tibble() %>%
  dplyr::slice(-1, -nrow(.)) %>%
  dplyr::mutate(
    value       = sub(" = ", " = readr::", value),
    has_ticks   = grepl("\\s+`", value),
    dd   = grepl("\\s+[^`]", value)
  )

  .[c(-1, -length(.))]
a

%>%
  readr::spec()
b <- readr::read_csv("inst/test-data/safety-long.csv") %>%
  readr::spec()



f <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(),
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "",
    trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(),
    skip_empty_rows = TRUE)

attr((function (file, col_names = TRUE, col_types = NULL, locale = default_locale(),
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "",
    trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(),
    skip_empty_rows = TRUE)
{
    tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na,
        quote = quote, comment = comment, trim_ws = trim_ws,
        skip_empty_rows = skip_empty_rows)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types,
        locale = locale, skip = skip, skip_empty_rows = skip_empty_rows,
        comment = comment, n_max = n_max, guess_max = guess_max,
        progress = progress)
})(file = file, col_names = col_names, col_types = col_types,
    locale = locale, na = na, quoted_na = quoted_na, quote = quote,
    comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max,
    guess_max = guess_max, progress = progress, skip_empty_rows = skip_empty_rows),
    "spec")
