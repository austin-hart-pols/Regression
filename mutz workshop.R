#+ WROKSHOP: Multiple regression
#+ March 27 2024

# SETUP ---------------------
## load packages
  library(tidyverse)  
  library(haven)
  library(stargazer)

## set your directory
  setwd("C:/Users/ahart/Desktop/sis600") 

## load data
  df = read_dta('obamasdog.dta')

# EXPLORE -------------------
## Feeling thermometer Adv
  summary(df$ObamaAdv)
  hist(df$ObamaAdv)
  
## Dog ownership
  count(df, PetDog) |>
    mutate(percent = 100 * n / sum(n))

# REGRESSION ----------------
## Plot
  boxplot(ObamaAdv ~ PetDog, df)

  b1 = df |>
    group_by(PetDog) |>
    summarise(avg = mean(ObamaAdv, na.rm = T))

  barplot(avg ~ PetDog, b1, 
          ylab = 'Avg Obama Advantage',
          xlab = 'Owns Dog')
  
## Models
  e1 = lm(ObamaAdv ~ PetDog, df)  
  e2 = lm(ObamaAdv ~ PetDog + PetCat + PetFish + PetBird + 
            PetRodent + PetHorse + PetReptile + PetOther +
            PartyID + IdeologyLR + 
            NationalEconomy + FamilyFinance, df)
  e3 = lm(ObamaAdv ~ PetDog + PetCat + PetFish + PetBird + 
            PetRodent + PetHorse + PetReptile + PetOther +
            PartyID + IdeologyLR + 
            NationalEconomy + FamilyFinance +
            Income + Age, df)
  
  stargazer(e1, e2, e3, type = 'text', keep.stat = 'n')
  
