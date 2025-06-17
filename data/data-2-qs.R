require(easyr)
require(qs2)
begin()

load("../app/data.RData")

qs2::qs_savem(cm, il, pa, options, file = "../app/data.qs2")
