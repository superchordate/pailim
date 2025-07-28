add_type_of_action = function(x) {

    # Identify columns related to violence.
    combine_cols = grep("iolence", colnames(x), value = TRUE)

    # Combine these columns into a ;-separated list.
    x %<>%
        mutate(
            `Type of Action` = apply(select(., all_of(combine_cols)), 1, function(x) {
                paste(unique(x[!is.na(x)]), collapse = ";")
            })
        ) %>%
        mutate(`Type of Action` = ifelse(`Type of Action` == "", NA, `Type of Action`))

    return(x)
}