# Select only columns that are actually used by the app
# This reduces memory usage and improves performance

# Core columns used across all datasets for basic functionality
    core_cols = c(
        # Temporal columns
        "Add", "Year", "Month", "Date", "Week", "MonthNum", "Quarter",
        
        # Geographic columns  
        "Longitude", "Latitude",
        
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
        "Total.Imports.Gaza.Israel", "Total.Exports.Gaza.Israel"
    )
    
    # Palestinian-specific columns used in filtering/coloring
    pa_specific_cols = c(
        "Perpetrator.Type", "Perpetrator.Origin", "Region", 
        "Rocket.Number", "Balloon.Number", "Riot.SubCategory",
        "Victim.Type"
    )
    
    # Israeli-specific columns used in filtering/coloring  
    il_specific_cols = c(
        "Perpetrator.Type", "Area", "City",
        "Detained.Arrested", "Victim.Type"
    )
    
    # Select columns for each dataset
    pa_cols = c(core_cols, covariate_cols, pa_specific_cols)
    il_cols = c(core_cols, covariate_cols, il_specific_cols)
    cm_cols = c(core_cols, covariate_cols, "Victim.Type")
    
    # Filter to only existing columns to avoid errors
    pa_cols = intersect(pa_cols, colnames(pa))
    il_cols = intersect(il_cols, colnames(il))  
    cm_cols = intersect(cm_cols, colnames(cm))
    
    # Apply column selection
    pa = pa %>% select(all_of(pa_cols))
    il = il %>% select(all_of(il_cols))
    cm = cm %>% select(all_of(cm_cols))
    
    # Print summary of column reduction
    cat("Column reduction summary:\n")
    cat("PA dataset: reduced to", ncol(pa), "columns\n")
    cat("IL dataset: reduced to", ncol(il), "columns\n") 
    cat("CM dataset: reduced to", ncol(cm), "columns\n")