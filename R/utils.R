library(hunspell)

check_name_vals_spelling <- function(name_vals, allowed_words) {
  cells_with_bad_spelling <- name_vals %>%
    mutate(across(-case_study_number, as.character)) %>%
    pivot_longer(
      cols = -case_study_number,
      names_to = "field",
      values_to = "value"
    ) %>%
    filter(!is.na(value), value != "") %>%
    mutate(
      bad_words = hunspell(value),
      bad_words = lapply(bad_words, setdiff, allowed_words),
      has_bad_spelling = lengths(bad_words) > 0,
      bad_words = sapply(bad_words, paste, collapse = ", ")
    ) %>%
    filter(has_bad_spelling)
  return(cells_with_bad_spelling)
}
