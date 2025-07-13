require(easyr)
require(qs)

begin()

qs::qreadm("../data/cache/1-raw-data.qs")

il %>% filter(!is.na(N.Outposts)) %>% pull(Date) %>% range()

t = il %>% select(Date, District, Settler.Population, N.Outposts, Palestinian.Population, Avg.Daily.Wage, Crime, Labor.Participation) %>% dict()

t %>% select(Name, `% Missing`) %>% atype() %>% arrange(desc(`% Missing`))
