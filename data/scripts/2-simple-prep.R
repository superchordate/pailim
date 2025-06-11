if(!cache.ok(2)){

    pa %<>%
        as_tibble() %>%
        mutate(
            Longitude = X.1,
            Latitude = Y.1,
            CaseNum = row_number(),
            Add = 0
        ) %>%
        filter(Longitude != 0, Latitude != 0) %>%
        mutate_if(is.factor, as.character)

        
    il %<>%
        as_tibble() %>%
        mutate(
            CaseNum = row_number(),
            Longitude = X.1, 
            Latitude = Y.1,
            Add = 0
        ) %>%
        filter(Longitude != 0, Latitude != 0) %>%
        mutate_if(is.factor, as.character)

    save.cache(pa, il)

}