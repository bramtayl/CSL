library(magrittr)
library(stringi)

extract_percent_lines <- function(input_file, output_file) {
  readLines(input_file, warn = FALSE) %>%
    .[stri_detect_regex(., "^% ")] %>%
    stri_replace_first_fixed("% ", "") %>%
    writeLines(output_file)
}

extract_percent_lines("CSL.Rtex", "code.R")