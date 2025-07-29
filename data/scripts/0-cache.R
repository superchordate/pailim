cache.init(at.path = 'cache', save.only = !use_cache, caches = list(

    list(
        name = 'raw-data',
        depends.on = c('build-data.R', 'scripts/1-read-data.R', '../files')
    ),

    list(
        name = 'simple-prep',
        depends.on = c('scripts/2-simple-prep.R')
    ),

    list(
        name = 'loop-prep-pa',
        depends.on = c('scripts/3-loop-prep-pa.R')
    ),

    list(
        name = 'loop-prep-il',
        depends.on = c('scripts/4-loop-prep-il.R')
    ),

    list(
        name = 'postloop-prep',
        depends.on = c('scripts/5-postloop-prep.R')
    )

))