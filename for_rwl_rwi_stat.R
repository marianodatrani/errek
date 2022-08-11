library(tidyverse)
library(dplR)

setwd("C:/sk/Feldolgozott/2021/for")

ma <- read.rwl("MA_mind_trw.rwl")
mf <- read.rwl("MF_mind_trw.rwl")
sk <- read.rwl("SK60_OF_TRW.rwl")
zs <- read.rwl("zsmind_trw.rwl")

mili <- ls()
mind <- do.call("list", mget(mili))



mirwls <- lapply(mind, rwl.stats)
mirwis <- lapply(mind, rwi.stats)

for (i in 1:length(mirwls)){
  mirwls[[i]] <- mirwls[[i]] %>% mutate(site=names(mirwls)[[i]])
}

for (i in 1:length(mirwis)){
  mirwis[[i]] <- mirwis[[i]] %>% mutate(site=names(mirwis)[[i]])
}

rwl_st_mind <- bind_rows(mirwls) %>% relocate(site)
rwi_st_mind <- bind_rows(mirwis) %>% relocate(site)

write_csv(rwl_st_mind, "rwl_stats_4_sites.csv")

seg.plot(ma)

rm(list = ls(pattern = "_rw[il]s"))
