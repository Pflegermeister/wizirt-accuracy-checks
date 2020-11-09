library(tidyverse)
library(wizirt)
library(irtoys)
library(PerFit)
source("PFSRasch.R")

my_model <- wizirt(data = d, item_type = '1PL', engine = 'ltm')

all(b == my_model$fit$parameters$coefficients$difficulty)
all(a == my_model$fit$parameters$coefficients$discrimination)

cor(thetaest,my_model$fit$parameters$persons$ability)

pfa <- irt_person_fit(my_model, stats = c("Ht", "lzstar", "U3"))

all(Hts == pfa$person_estimates$Ht)
all(lzs == pfa$person_estimates$lzstar)
cor(lzs, pfa$person_estimates$lzstar)
all(U3s == pfa$person_estimates$U3)

cor(lzs, Hts)
cor(pfa$person_estimates$lzstar, pfa$person_estimates$Ht)

flags <- pfa$person_estimates %>% transmute(Ht_flag = Ht < Ht_cut, #cH
                                           U3_flag = U3 > U3_cut,
                                           lz_flag = lzstar < lzstar_cut,
                                           flagged = dat[,1]) %>%
  rowwise() %>%
  mutate(any_flag = Ht_flag | U3_flag | lz_flag,
         and_flag = Ht_flag & U3_flag & lz_flag,
         two_flag = sum(Ht_flag, U3_flag, lz_flag) > 1)


flags %>% colMeans()


c(length(Hts[Hts<cH])/length(Hts),
  length(U3s[U3s>cU])/length(Hts),
  length(lzs[lzs<cL])/length(lzs))

flag=dat[,1]

round(cbind(flagged = c(0, 1),
  Ht = c(length(Hts[Hts<cH & flag==0])/length(Hts[flag==0]),
    length(Hts[Hts<cH & flag==1])/length(Hts[flag==1])),

  U3 = c(length(U3s[U3s>cU & flag==0])/length(U3s[flag==0]),
    length(U3s[U3s>cU & flag==1])/length(U3s[flag==1])),

  lz = c(length(lzs[lzs<cL & flag==0])/length(lzs[flag==0]),
    length(lzs[lzs<cL & flag==1])/length(lzs[flag==1]))), 2)

flags %>% group_by(flagged) %>%
  summarise(across(c(lz_flag, Ht_flag, U3_flag), mean)) %>%
  as.matrix()


