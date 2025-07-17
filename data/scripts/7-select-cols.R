# Select only columns that are actually used by the app
# This reduces memory usage and improves performance

# Core columns used across all datasets for basic functionality
    core_cols = c(
        "Add", "Year", "Month", "Date", "Week", "MonthNum", "Quarter",
        "Longitude", "Latitude", "District", "City",
        "Casualties", "Killed", "Injured",
        "Verbatim.Report",
        "Palestine/Israel",
        "Type of Action", "Victim.Type"
    )
    
    # Note: Covariate columns have been moved to separate datasets 
    # (time_covariates and geo_covariates) in script 6.5-separate-covariates.R
    # They are no longer included in the main datasets to reduce memory usage
    
    # Palestinian-specific columns used in filtering/coloring
    pa_specific_cols = c(
        "Perpetrator Origin", "Region", 
        "Rocket.Number", "Balloon.Number", "Riot Subcategories"
    )
    
    # Israeli-specific columns used in filtering/coloring  
    il_specific_cols = c("Perpetrator Type", "Detained.Arrested")
    
    # Select columns for each dataset (covariates removed - they're in separate datasets)
    pa_cols = c(core_cols, pa_specific_cols)
    il_cols = c(core_cols, il_specific_cols)
    cm_cols = c(core_cols)
    
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
    
    # Print summary of column reduction
    cat("Column reduction summary:\n")
    cat("PA dataset: reduced to", ncol(pa), "columns\n")
    cat("IL dataset: reduced to", ncol(il), "columns\n") 
    cat("CM dataset: reduced to", ncol(cm), "columns\n")
    cat("NA values filled with 'Missing' for filtering columns\n")