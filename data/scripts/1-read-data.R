if(!cache.ok(1)){
    
    # options.
    raw_data_pa = 'Palestinian.Violence.Covariates_new.csv'
    raw_data_il = 'Israeli.Violence.Covariates_new.csv'

    pa = read.csv(glue("raw-data/{raw_data_pa}"), fileEncoding = 'Windows-1252')
    il = read.csv(glue("raw-data/{raw_data_il}"), fileEncoding = 'Windows-1252')

    save.cache(pa, il)

}