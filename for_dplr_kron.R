library(tidyverse)
library(dplR)

# matra_dplr_kron alapján sk-ra átírva

setwd("F:/Síkfõ/Feldolgozott/2021/for")

##### fun ####

rwgather_matra <- function(x){
  xlo <- gather(x, fa, ert, -years)
  xlo <- xlo %>% 
    mutate(sz=str_sub(fa, 3, 4),
           szam=as.integer(str_remove(sz, "_")),
           ter=str_sub(fa, 1, 2),
           fa=str_c(ter, szam)) %>% 
    select(-sz, -ter)
}

rwgather_sk <- function(x){
  xlo <- gather(x, fa, ert, -year)
}

rwspread_sk <- function(x) {
  spread(x, fa, ert) %>% 
    column_to_rownames("year")
}

##### mátra rwl összefûzés, csak egyszer kellett

setwd("C:/sk/gozdarski/matrarwl")

files<- list.files(pattern = ".rwl")

list_for_files <- list()

for (i in 1:length(files)){
  
  list_for_files[[i]] <- read.rwl(files[i])
}

maxs <- list()
mins <- list()

for (i in 1:length(list_for_files)){
  maxs[i] <- max(as.numeric(row.names(list_for_files[[i]])))
  mins[i] <- min(as.numeric(row.names(list_for_files[[i]])))
}

DF <- data.frame(years = seq(min(unlist(mins)), max(unlist(maxs)), by = 1))

for (i in 1:length(list_for_files)){
  a <- list_for_files[[i]]
  a$samp.depth <- NULL
  a$years <- as.numeric(row.names(a))
  DF <- merge(DF, a, by = "years", all.x = TRUE)
}

DF %>% 
  column_to_rownames("years") %>% 
  select(contains("MA")) %>% 
  write.rwl("MA_mind_trw.rwl")

####### sk #########

# fájlok be és egybõl export, hogy eltûnjenek az elsõ oszlopok a sorszámokkal
# ezt csak egyszer kellett!

skli <- list.files(pattern = "^sk60.*w.csv$")
skfili <- list()

for (i in 1:length(skli)){
  skfili[[i]] <- read.csv(skli[i])
  skfili[[i]] <- select(skfili[[i]], -1)
  write.csv(skfili[[i]], file = skli[i], row.names = F)
}





#### prep ####

skli <- list.files(pattern = "^sk60.*w.csv$")
nevek <- str_remove(skli, ".csv")
skfili <- list()

for (i in 1:length(skli)){
  skfili[[i]] <- read.csv(skli[i])
}
names(skfili) <- nevek

for (i in 1:length(skfili)){
assign(nevek[[i]], skfili[[i]])
}



skfilidh <- lapply(skfili, column_to_rownames, "year")


minddt <- lapply(skfilidh, detrend, method="Spline", nyrs = 30)

mindcro <- lapply(minddt, chron, prewhiten = T, prefix = "")


for (i in 1:length(mindcro)) {
  df <- as.data.frame(mindcro[[i]])
  assign(str_c(names(skli)[[i]], "_cro"), df)
}

mindcro_exp <- lapply(mindcro, rownames_to_column, "Years")
mindcro_exp <- lapply(mindcro_exp, function(x) mutate(x, Years=as.integer(Years)))

for (i in 1:length(mindcro_exp)) {
  df <- as.data.frame(mindcro_exp[[i]])
  write.csv(mindcro_exp[i], file = str_c(nevek[i], "_spline30_kron.csv"), 
            row.names = FALSE)
}



##### sk60 egybe #####

sk60trw <- inner_join(sk60_kat1_trw, sk60_kat2_trw, by = "year") %>% 
  inner_join(sk60_kat3_trw, by = "year")

sk60ew <- inner_join(sk60_kat1_ew, sk60_kat2_ew, by = "year") %>% 
  inner_join(sk60_kat3_ew, by = "year")

sk60lw <- inner_join(sk60_kat1_lw, sk60_kat2_lw, by = "year") %>% 
  inner_join(sk60_kat3_lw, by = "year")

sk60li <- list(sk60trw, sk60ew, sk60lw)
names(sk60li) <- c("sk60trw", "sk60ew", "sk60lw")

skfilidh60 <- lapply(sk60li, column_to_rownames, "year")


minddt60 <- lapply(skfilidh60, detrend, method="Spline", nyrs = 30)

mindcro60 <- lapply(minddt60, chron, prewhiten = T, prefix = "")


for (i in 1:length(mindcro60)) {
  df <- as.data.frame(mindcro60[[i]])
  assign(names(sk60li)[[i]], df)
}

mindcro60_exp <- lapply(mindcro60, rownames_to_column, "Years")
mindcro60_exp <- lapply(mindcro60_exp, function(x) mutate(x, Years=as.integer(Years)))


for (i in 1:length(mindcro60_exp)) {
  df <- as.data.frame(mindcro60_exp[[i]])
  assign(paste0(names(sk60li)[[i]], "_exp"), df)
  write.csv(mindcro60_exp[i], file = paste0(names(sk60li)[i], "_spline30_kron.csv"), row.names = FALSE)
}

##############

####### zs #######

# zs kat rwl összefûzés egybe mind

setwd("C:/sk/gozdarski/zs")

lf <- list.files(pattern = "raw.rwl")

zsfilikat <- list()
zsflk <- list()

for (i in 1:length(lf)){
  zsfilikat[[i]] <- read.rwl(lf[i])
}

zsflk <- lapply(zsfilikat, as_tibble, rownames="year")

zsegy <- zsflk[[1]] %>% 
  inner_join(zsflk[[2]], by="year") %>% 
  inner_join(zsflk[[3]], by="year") %>% 
  column_to_rownames("year")

write.rwl(zsegy, "zsmind_trw.rwl")


#### prep ####

zsli <- list.files(pattern = "^zs.*w.csv$")
nevek <- str_remove(zsli, ".csv")
zsfili <- list()

for (i in 1:length(zsli)){
  zsfili[[i]] <- read.csv(zsli[i])
}
names(zsfili) <- nevek

for (i in 1:length(zsfili)){
  assign(nevek[[i]], zsfili[[i]])
}

##### zs egybe #####

zstrw <- inner_join(zs_kat1_fa_trw, zs_kat2_fa_trw, by = "year") %>% 
  inner_join(zs_kat3_fa_trw, by = "year")

zsew <- inner_join(zs_kat1_fa_ew, zs_kat2_fa_ew, by = "year") %>% 
  inner_join(zs_kat3_fa_ew, by = "year")

zslw <- inner_join(zs_kat1_fa_lw, zs_kat2_fa_lw, by = "year") %>% 
  inner_join(zs_kat3_fa_lw, by = "year")

zsli <- list(zstrw, zsew, zslw)
names(zsli) <- c("zs_trw", "zs_ew", "zs_lw")

zsfilidh <- lapply(zsli, column_to_rownames, "year")


minddt <- lapply(zsfilidh, detrend, method="Spline", nyrs = 30)

mindcro <- lapply(minddt, chron, prewhiten = T, prefix = "")


for (i in 1:length(mindcro)) {
  df <- as.data.frame(mindcro[[i]])
  assign(str_c(names(zsli)[[i]], "_cro"), df)
}

mindcro_exp <- lapply(mindcro, rownames_to_column, "Years")
mindcro_exp <- lapply(mindcro_exp, function(x) mutate(x, Years=as.integer(Years)))


for (i in 1:length(mindcro_exp)) {
  df <- as.data.frame(mindcro_exp[[i]])
  assign(paste0(names(zsli)[[i]], "_exp"), df)
  write.csv(mindcro_exp[i], file = paste0(names(zsli)[i], "_egybe_spline30_kron.csv"), row.names = FALSE)
}

rm(list = ls())

#### zs kategóriánként ####

zsli <- list.files(pattern = "^zs.*w.csv$")
nevek <- str_remove(str_remove(zsli, ".csv"), "_fa")
zsfili <- list()

for (i in 1:length(zsli)){
  zsfili[[i]] <- read.csv(zsli[i])
}
names(zsfili) <- nevek

for (i in 1:length(zsfili)){
  assign(nevek[[i]], zsfili[[i]])
}

zsfilidh <- lapply(zsfili, column_to_rownames, "year")


minddt <- lapply(zsfilidh, detrend, method="Spline", nyrs = 30)

mindcro <- lapply(minddt, chron, prewhiten = T, prefix = "")


for (i in 1:length(mindcro)) {
  df <- as.data.frame(mindcro[[i]])
  assign(str_c(names(zsli)[[i]], "_cro"), df)
}

mindcro_exp <- lapply(mindcro, rownames_to_column, "Years")
mindcro_exp <- lapply(mindcro_exp, function(x) mutate(x, Years=as.integer(Years)))

for (i in 1:length(mindcro_exp)) {
  df <- as.data.frame(mindcro_exp[[i]])
  assign(str_c(nevek[i], "_exp"), df)
  write.csv(mindcro_exp[i], file = str_c(nevek[i], "_spline30_kron.csv"), 
            row.names = FALSE)
}

for (i in 1:length(mindcro_exp)) {
  df <- as.data.frame(mindcro_exp[[i]])
  write.csv(mindcro_exp[i], file = str_c(nevek[i], "_spline30_kron.csv"), 
            row.names = FALSE)
}


##### dendroclimhez #####

mindcros <- lapply(mindcro, select, 1)
mindcror <- lapply(mindcro, select, 2)

mindcros_txt <- lapply(mindcros, rownames_to_column, var = "years")
mindcros_txt <- lapply(mindcros_txt, function(x) mutate(x, years=as.integer(years)))
  

for (i in 1:length(mindcro_exp)) {
  df <- as.data.frame(mindcros_txt[[i]])
  write.table(mindcros_txt[i], file = str_c(nevek[i], "_spline30_std_kron.txt"),
              sep = "\t   ", row.names = FALSE, col.names = FALSE)
}

mindcror_txt <- lapply(mindcror, rownames_to_column, var = "years")
mindcror_txt <- lapply(mindcror_txt, function(x) mutate(x, years=as.integer(years)))


for (i in 1:length(mindcro_exp)) {
  df <- as.data.frame(mindcror_txt[[i]])
  write.table(mindcror_txt[i], file = str_c(nevek[i], "_spline30_res_kron.txt"),
              sep = "\t   ", row.names = FALSE, col.names = FALSE)
} 



rm(list = ls())





#############

############





# detrendhez column to rownames

skfilidh <- lapply(skfili, column_to_rownames, "year")


#### kron ####

minddt <- lapply(skfilidh, detrend, method="Spline", nyrs = 30)

mindcro<- lapply(minddt, chron, prewhiten = T, prefix = "")

mindcro_exp <- lapply(mindcro, rownames_to_column, "Years")
mindcro_exp <- lapply(mindcro_exp, function(x) mutate(x, Years=as.integer(Years)) %>% 
  filter(!is.na(res)))

# export

for (i in 1:length(mindcro_exp)) {
  write.csv(mindcro_exp[i], file = str_c(names(mindcro_exp)[[i]], "_spline30_kron.csv"),
            row.names = FALSE)
}





for (i in 1:length(mindcro)) {
  df <- as.data.frame(mindcro[[i]])
  assign(names(mindcro)[[i]], df)
}


# export year oszlop nélkül

for (i in 1:length(mindcro)) {
  write.csv(mindcro[i], file = str_c(names(mindcro)[i], "_spline30_kron.csv"),
            row.names = TRUE)
}

########## egyebek ##########

mindcros<- lapply(mindcro, function(x) x %>% select(std))
mindcror<- lapply(mindcro, function(x) x %>% select(res))


#### export ####

mindcro_exp <- lapply(mindcro, rownames_to_column, "Years")
mindcro_exp <- lapply(mindcro_exp, function(x) mutate(x, Years=as.integer(Years)))

#exportnevek <- lapply(names(mindcro_exp), str_replace, "wk", "w_k")

for (i in 1:length(mindcro_exp)) {
  df <- as.data.frame(mindcro_exp[[i]])
  assign(paste0(names(mindcro_exp)[[i]], "_exp"), df)
  write.csv(mindcro_exp[i], file = paste0(names(mindcro_exp)[i], "_spline30_kron.csv"), row.names = FALSE)
}






rm(list = ls())
