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
time_covariates %<>%
    select(Date, all_of(time_varying_covariates)) %>%
    add_date_variables()

# Create geographic-varying covariates dataset  
# Use unique combinations of geographic and temporal columns with covariates
geo_covariates %<>% 
  rename_with(~ gsub("\\.Gov\\.Year", "", .x)) %>%
  rename(
    Palestinian.Population = Pal.Population,
    Avg.Daily.Wage = Daily.Wage,
    Crime = Total.Crimes,
    Labor.Participation = Labor.Partic
  ) %>%
  select(
      Year, District, 
      all_of(geographic_varying_covariates)
    )

# Remove covariates from main datasets
pa = pa %>% select(-all_of(intersect(all_covariates, colnames(pa))))
il = il %>% select(-all_of(intersect(all_covariates, colnames(il))))
cm = cm %>% select(-all_of(intersect(all_covariates, colnames(cm))))
