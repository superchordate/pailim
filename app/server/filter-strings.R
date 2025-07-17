# This file contains some functions used for building the filter notes that show up in a few different places in the app. 
# Search "filter-note" to find where these are used in the UI.
# They are called by app/server/data-dataPlot-filter-strings.R to keep the strings aligned with the data. 

# Generate year filter description string
format_year_filter = function(selected_years, display_threshold = 5) {
  if(length(selected_years) <= display_threshold) {
    # Check if years are contiguous
    sorted_years = sort(as.numeric(selected_years))
    if(length(sorted_years) > 1 && all(diff(sorted_years) == 1)) {
      # Years are contiguous, use range format
      return(paste("Years:", paste(min(sorted_years), max(sorted_years), sep = "-")))
    } else {
      # Years are not contiguous, list them
      return(paste("Years:", paste(sorted_years, collapse = ", ")))
    }
  } else {
    return(paste("Years:", length(selected_years), "selected"))
  }
}

# Generate month filter description string
format_month_filter = function(selected_months, display_threshold = 5) {
  # Convert month names to numbers for contiguous checking
  month_names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  month_numbers = match(selected_months, month_names)
  
  # Check if all months were matched (no NAs)
  if(any(is.na(month_numbers))) {
    # If any month names don't match, just list them as-is
    return(paste("Months:", paste(selected_months, collapse = ", ")))
  } else {
    sorted_month_numbers = sort(month_numbers)
    
    # Always check for contiguity first, regardless of count
    if(length(sorted_month_numbers) > 1 && all(diff(sorted_month_numbers) == 1)) {
      # Months are contiguous, use range format
      start_month = month_names[min(sorted_month_numbers)]
      end_month = month_names[max(sorted_month_numbers)]
      return(paste("Months:", paste(start_month, end_month, sep = "-")))
    } else if(length(selected_months) <= display_threshold) {
      # Months are not contiguous but few enough to list individually
      sorted_months = month_names[sorted_month_numbers]
      return(paste("Months:", paste(sorted_months, collapse = ", ")))
    } else {
      # Too many non-contiguous months, show count
      return(paste("Months:", length(selected_months), "selected"))
    }
  }
}

# Generate generic filter description string (for action types, regions, etc.)
format_generic_filter = function(selected_items, filter_label, display_threshold = 5) {
  if(length(selected_items) <= display_threshold) {
    return(paste(paste0(filter_label, ":"), paste(selected_items, collapse = ", ")))
  } else {
    return(paste(paste0(filter_label, ":"), length(selected_items), "selected"))
  }
}
