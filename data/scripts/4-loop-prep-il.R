if (!cache.ok(4)) {
  
  # for each record, for each additional location, we create a new row setting the additional x/y as X.1, Y.1.
  tmp2 <- list()
  for(i in 2:6){
    
    idt = il %>% 
      
      # rename X.i Y.i to X.1 Y.1
      select(
        CaseNum,
        X.1 = !!sym(paste0('X.', i)), 
        Y.1 = !!sym(paste0('Y.', i)),
        City = !!sym(paste0('Town.', i)),
      ) %>%
      mutate(
        Add = 1 # indicate this record has been added.
      )

      # Convert to numeric.
      # There is a X.5 value "Battir" which is a string, so we suppress warnings. We want this to be NA and get filtered out.
      idt$X.1 = suppressWarnings(as.numeric(idt$X.1))
      idt$Y.1 = suppressWarnings(as.numeric(idt$Y.1))

    if(all(is.na(idt$X.1))) next
    
    # add this data to the running list. 
    tmp2[[i]] <- bind_cols(
      idt[!is.na(idt$X.1), ],
      il[!is.na(idt$X.1), ] %>% select(-CaseNum, -X.1, -Y.1, -City, -Add)
    )
    
  }

  tmp2 %<>% bind_rows() 
  tmp2 %<>% mutate(Add = as.integer(Add)) %>% arrange(CaseNum, Add)
  # tmp %<>% arrange(CaseNum, Add) # ensure tmp is also ordered by CaseNum and Add
  # validate.equal(tmp, tmp2)
  # stophere

  tmp2 %<>% 
    mutate(
      Longitude = X.1, 
      Latitude = Y.1
    ) %>% 
    filter(
      Longitude != 0, 
      Latitude != 0
    ) %>% 
    distinct()
  
  il <- bind_rows(il, tmp2)
  
  save.cache(il, pa, time_covariates, geo_covariates)

}