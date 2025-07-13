# Separate covariates into their own datasets
# This reduces memory usage and makes covariates easier to work with
    
# Define covariate columns
time_varying_covariates = c(
    "Israeli.CPI", "Palestinian.CPI", 
    "Israeli.UE.Quarterly", "Palestinian.UE.Quarterly",
    "Israeli.Trade.Balance", "Palestinian.Trade.Balance", 
    "Exchange.Rate",
    "Demolished.Structures.Daily", 
    "TA125.PX_CLOSE", "PASISI.PX_CLOSE",
    "TAVG", "PRCP", 
    "Total.Entries.Exits.Gaza.Israel", 
    "Total.Imports.Gaza.Israel", "Total.Exports.Gaza.Israel"
)

geographic_varying_covariates = c(
    "Settler.Population",
    "N.Outposts",
    "Palestinian.Population",
    "Avg.Daily.Wage",
    "Crime",
    "Labor.Participation"
)

all_covariates = c(time_varying_covariates, geographic_varying_covariates)

# Create time-varying covariates dataset
# Use unique combinations of temporal columns with covariates
time_covariates_raw = cm %>% 
    select(
        Year, MonthNum, Quarter, Week, Date, 
        all_of(time_varying_covariates)
    )

# Handle duplicates by taking the mean across duplicates (removing NAs)
time_covariates = as.data.table(time_covariates_raw)[, lapply(.SD, function(x) {
    mean(x, na.rm = TRUE)
}), by = Date, .SDcols = c("Year", "MonthNum", "Quarter", "Week", time_varying_covariates)]
setorder(time_covariates, Date)

# Create geographic-varying covariates dataset  
# Use unique combinations of geographic and temporal columns with covariates
geo_covariates_raw = cm %>%
    select(
        Year, MonthNum, Quarter, Week, Date, 
        District, 
        all_of(geographic_varying_covariates)
    )

# Handle duplicates by taking the mean across duplicates (removing NAs)
geo_covariates = as.data.table(geo_covariates_raw)[, lapply(.SD, function(x) {
    mean(x, na.rm = TRUE)
}), by = .(Date, District), .SDcols = c("Year", "MonthNum", "Quarter", "Week", geographic_varying_covariates)]
setorder(geo_covariates, Date, District)

# Change to tibbles since they are easier to use that data.tables.
time_covariates %<>% tibble()
geo_covariates %<>% tibble()

# Housekeeping.
rm(time_covariates_raw, geo_covariates_raw)

# Remove covariates from main datasets
pa = pa %>% select(-all_of(intersect(all_covariates, colnames(pa))))
il = il %>% select(-all_of(intersect(all_covariates, colnames(il))))
cm = cm %>% select(-all_of(intersect(all_covariates, colnames(cm))))
