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
                    City = d[['City']],
                    Add = 1
                )
                
                stmp[[j]] <- bind_cols(
                    stmp[[j]], 
                    d %>% select(-CaseNum, -X.1, -Y.1, -City, -Add)
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
    
    save.cache(il, pa, time_covariates, geo_covariates)
}