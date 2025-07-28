clean_text <- function(text) {
    text %>%
        stringr::str_remove_all("[<>+]") %>%
        stringr::str_remove_all("U00..")
}
