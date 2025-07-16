# ===== DATE PROCESSING =====
add_date_variables <- function(df) {
    df %>%
        mutate(
            Date = lubridate::ymd(as.character(Date)),
            MonthNum = lubridate::month(Date),
            Month = factor(MonthNum, levels = 1:12, labels = month.abb),
            Week = lubridate::week(Date),
            Quarter = lubridate::quarter(Date)
        )
}