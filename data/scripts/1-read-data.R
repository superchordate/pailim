if(!cache.ok(1)){
    
    raw_data_il = 'Israeli Actions (2009-2023) v20250724.csv'
    raw_data_pa = 'Palestinian Actions (2009-2023) v20250724.csv'

    il = read.csv(glue("raw-data/{raw_data_il}"), fileEncoding = 'Windows-1252')
    pa = read.csv(glue("raw-data/{raw_data_pa}"), fileEncoding = 'Windows-1252')

    save.cache(pa, il)

}