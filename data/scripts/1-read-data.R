if(!cache.ok(1)){
    
    # options.
    # raw_data_pa = 'Palestinian.Violence.Covariates_new.csv'
    # raw_data_il = 'Israeli.Violence.Covariates_new.csv'
    raw_data_pa = 'Palestinian Actions (2009-2023).csv'
    raw_data_il = 'Israeli Actions (2009-2023).csv'

    pa = read.csv(glue("raw-data/{raw_data_pa}"), fileEncoding = 'Windows-1252')
    il = read.csv(glue("raw-data/{raw_data_il}"), fileEncoding = 'Windows-1252')

    save.cache(pa, il)

}