if (!cache.ok(3)) {
    
    # Create extra rows for multi-action events. 
    tmp <- list()    
    for (i in 1:nrow(pa)) {
        d <- pa[i, ]
        stmp <- list()
        
        for (j in 2:8) {
            x_col <- paste0('X.', j)
            y_col <- paste0('Y.', j)
            
            if (!is.na(d[[x_col]])) {
                stmp[[j]] <- tibble(
                    CaseNum = d[['CaseNum']],
                    X.1 = d[[x_col]],
                    Y.1 = d[[y_col]],
                    Add = 1
                )
                
                stmp[[j]] <- bind_cols(
                    stmp[[j]], 
                    d %>% select(-CaseNum, -X.1, -Y.1, -Add)
                )
            }
        }
        
        tmp[[i]] <- bind_rows(stmp)
    }
    
    tmp <- bind_rows(tmp) %>% 
        mutate(
            Longitude = X.1, 
            Latitude = Y.1
        ) %>% 
        filter(
            Longitude != 0,
            Latitude != 0
        ) %>% 
        distinct()
    
    pa <- bind_rows(pa, tmp)

    # Create `Type of Action` column to match il.
    combine_cols = grep("iolence", colnames(pa), value = TRUE)
    
    # Combine these columns into a ;-separated list. 
    pa <- pa %>%
        mutate(
            `Type of Action` = apply(select(., all_of(combine_cols)), 1, function(x) {
                paste(x[!is.na(x)], collapse = ";")
            })
        ) %>%
        mutate(`Type of Action` = ifelse(`Type of Action` == "", NA, `Type of Action`))
    
    save.cache(il, pa)
}