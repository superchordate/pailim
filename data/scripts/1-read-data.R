if(!cache.ok(1)){
    
    # Data files names.
    filename_il = 'Israeli Actions (2009-2023) v20250724.csv'
    filename_pa = 'Palestinian Actions (2009-2023) v20250724.csv'
    filename_time_covariates = 'Covariates (2009-2023).csv'
    filename_time_district_covariates = 'District Covariates (2009-2023).csv'

    il = read.csv(glue("../files/{filename_il}"), fileEncoding = 'Windows-1252')
    pa = read.csv(glue("../files/{filename_pa}"), fileEncoding = 'Windows-1252')
    time_covariates = read.csv(glue("../files/{filename_time_covariates}"), fileEncoding = 'Windows-1252')
    geo_covariates = read.csv(glue("../files/{filename_time_district_covariates}"), fileEncoding = 'Windows-1252')

    save.cache(pa, il, time_covariates, geo_covariates)

}