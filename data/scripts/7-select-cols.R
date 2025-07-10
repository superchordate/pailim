# Select only columns that are actually used by the app
# This reduces memory usage and improves performance

# Core columns used across all datasets for basic functionality
    core_cols = c(
        # Temporal columns
        "Add", "Year", "Month", "Date", "Week", "MonthNum", "Quarter",
        
        # Geographic columns  
        "Longitude", "Latitude", "District", "City",
        
        # Casualty data
        "Casualties", "Killed", "Injured",
        
        # Report data
        "Verbatim.Report",
        
        # Dataset identifier
        "Palestine/Israel",
        
        # Action classification
        "Type of Action"
    )
    
    # Covariate columns used in line plots
    covariate_cols = c(
        "Israeli.CPI", "Palestinian.CPI", 
        "Israeli.UE.Quarterly", "Palestinian.UE.Quarterly",
        "Israeli.Trade.Balance", "Palestinian.Trade.Balance", 
        "Exchange.Rate",
        "Demolished.Structures.Daily", 
        "TA125.PX_CLOSE", "PASISI.PX_CLOSE",
        "TAVG", "PRCP", 
        "Total.Entries.Exits.Gaza.Israel", 
        "Total.Imports.Gaza.Israel", "Total.Exports.Gaza.Israel",
        # New geographic-varying covariates.
        "Settler.Population",
        "N.Outposts",
        "Palestinian.Population",
        "Avg.Daily.Wage",
        "Crime",
        "Labor.Participation"
    )
    
    # Palestinian-specific columns used in filtering/coloring
    pa_specific_cols = c(
        "Perpetrator Origin", "Region", 
        "Rocket.Number", "Balloon.Number", "Riot.SubCategory",
        "Victim.Type"
    )
    
    # Israeli-specific columns used in filtering/coloring  
    il_specific_cols = c(
        "Perpetrator Type", 
        "Detained.Arrested", "Victim.Type"
    )
    
    # Select columns for each dataset
    pa_cols = c(core_cols, covariate_cols, pa_specific_cols)
    il_cols = c(core_cols, covariate_cols, il_specific_cols)
    cm_cols = c(core_cols, covariate_cols, "Victim.Type")
    
    # Check for missing columns and error out if any are not found
    missing_pa_cols = setdiff(pa_cols, colnames(pa))
    missing_il_cols = setdiff(il_cols, colnames(il))
    missing_cm_cols = setdiff(cm_cols, colnames(cm))
    
    if(length(missing_pa_cols) > 0) {
        stop(paste("Missing columns in PA dataset:", paste(missing_pa_cols, collapse = ", ")))
    }
    if(length(missing_il_cols) > 0) {
        stop(paste("Missing columns in IL dataset:", paste(missing_il_cols, collapse = ", ")))
    }
    if(length(missing_cm_cols) > 0) {
        stop(paste("Missing columns in CM dataset:", paste(missing_cm_cols, collapse = ", ")))
    }
    
    # Apply column selection (now that we've confirmed all columns exist)
    pa = pa %>% select(all_of(pa_cols))
    il = il %>% select(all_of(il_cols))
    cm = cm %>% select(all_of(cm_cols))
    
    # Fill NA values with "Missing" for character columns that are used in filtering
    # This prevents records from being dropped when all choices are selected
    fill_na_columns = c("District", "City", "Region", "Perpetrator Origin", "Perpetrator Type", "Victim.Type")
    
    # Apply to PA dataset
    for(col in intersect(fill_na_columns, colnames(pa))) {
        if(is.character(pa[[col]]) || is.factor(pa[[col]])) {
            pa[[col]] = ifelse(is.na(pa[[col]]), "Missing", as.character(pa[[col]]))
        }
    }
    
    # Apply to IL dataset
    for(col in intersect(fill_na_columns, colnames(il))) {
        if(is.character(il[[col]]) || is.factor(il[[col]])) {
            il[[col]] = ifelse(is.na(il[[col]]), "Missing", as.character(il[[col]]))
        }
    }
    
    # Apply to CM dataset
    for(col in intersect(fill_na_columns, colnames(cm))) {
        if(is.character(cm[[col]]) || is.factor(cm[[col]])) {
            cm[[col]] = ifelse(is.na(cm[[col]]), "Missing", as.character(cm[[col]]))
        }
    }
    
    # Print summary of column reduction
    cat("Column reduction summary:\n")
    cat("PA dataset: reduced to", ncol(pa), "columns\n")
    cat("IL dataset: reduced to", ncol(il), "columns\n") 
    cat("CM dataset: reduced to", ncol(cm), "columns\n")
    cat("NA values filled with 'Missing' for filtering columns\n")