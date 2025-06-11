if (!cache.ok(4)) {
  
  # for each record, for each additional location, we create a new row setting the additional x/y as X.1, Y.1.
  tmp2 <- list()  
  extra_xy = unique(as.numeric(gsub("X\\.", "", grep("X\\.", colnames(il), value = TRUE))))
  extra_xy = setdiff(extra_xy, 1) # remove the first one, which is already X.1, Y.1 and has been captured elsewhere.
  for(i in extra_xy){
    
    idt = il %>% 
      
      # rename X.i Y.i to X.1 Y.1
      select(
        CaseNum,
        X.1 = !!sym(paste0('X.', i)), 
        Y.1 = !!sym(paste0('Y.', i))
      ) %>%
      mutate(
        Add = 1 # indicate this record has been added.
      )
    
    # add this data to the running list. 
    tmp2[[i]] <- bind_cols(
      idt[!is.na(idt$X.1), ],
      il[!is.na(idt$X.1), ] %>% select(-CaseNum, -X.1, -Y.1, -Add)
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
  
  save.cache(il, pa)

}