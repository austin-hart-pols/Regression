#+ Multiple regression
#+ March 20, 2024

# SETUP ---------------------
## load a package
  library(stargazer)
  library(tidyverse)  
  library(haven)

## set your directory
  setwd("C:/Users/ahart/Desktop/sis600") # change to your path

## load workshop data
  load("anes2012 workshop.RData") # must wrap in quotes

## White non-Hispanic
  df = df |>
    filter(raceeth.selfid == 1)

# Review ---------------
## global warming
  df |>
    count(global.warming) |>
    mutate(percent = n / sum(n) * 100)
  
## Party id
  df |>
    count(party.id) |>
    mutate(percent = n / sum(n) * 100)

  summary(df$party.id)
  
  
# Regression ----------------
## Unconditional
  m1 = lm(ft.feminists ~ resent.index, df)
  
## Conditional
  m2 = lm(ft.feminists ~ resent.index +
            party.id + ideology +
            national.economy + age, df)

## Table
  stargazer(m1, m2, type = 'text', keep.stat = 'n')
  
  
#+ The unconditional model (1) shows that a one-point increase
#+ in resentment score is associated with a 36-point drop in 
#+ feeling thermometer (FT) ratings of feminists. Model 2 
#+ introduces controls for party identification, ideology,
#+ economic evaluations, and age. Net of these controls, 
#+ I find that a one-point increase in resentment reduces
#+ FT ratings by almost 17 points. Both estimates are 
#+ satistically significant at 5%. 