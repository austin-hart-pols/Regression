# CLASS SCRIPT
# MULTIPLE REGRESSION WEEK

# Setup -----------

  ## packages
  library(tidyverse)
  library(haven)
  library(stargazer)
  
  ## directory and data
  setwd("~/SIS600")
  load("anes2012 workshop.rdata")
  
  ## filter on white/non-hispanic
  df = filter(df, raceeth.selfid == 1)
  
# Analysis -------------
  
  ## global warming freq table  
  count(df, global.warming) %>%
    mutate(percent = n/sum(n) * 100)
  
  ## party id freq table
  count(df, party.id) %>%
    mutate(percent = n/sum(n) * 100)
  
  
# Regression -----------
  
  ## Unconditional estimate
  e1 = lm(ft.feminists ~ resent.index, data = df)
  
  ## Conditional model
  e2 = lm(ft.feminists ~ resent.index + ideology + party.id + national.economy + age, data = df)
  
  ## stargazer table     
  stargazer(e1, e2, type = 'text', keep.stat='n')


