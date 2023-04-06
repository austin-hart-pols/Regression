# Mutz workshop day
# Austin Hart
# ----------------------

# Setup
  library(tidyverse)
  library(haven)
  library(stargazer)

  df = read_dta('obamasdog.dta') %>%
    mutate(PID3 = case_when(
      PartyID == 3 ~ 'DEM',
      PartyID == 1 ~ 'IND',
      PartyID == 2 ~ 'REP'
    ))
  

# Exploratory analysis
# ---------------------- 

  # Candidate ratings
  hist(df$ObamaAdv)
  summary(df$ObamaAdv)  

  # Pet ownership
  count(df, PetDog)

  # Party identification
  count(df, PID3)
  

# Unconditional reg
# ----------------------
  
  # plot
  boxplot(ObamaAdv ~ PetDog, data = df)
  
  # est regression
  m1 = lm(ObamaAdv ~ PetDog, data = df)

  # estimates to table
  stargazer(m1, type = 'text', keep.stat = 'n')
    
  
# PID as confounder
# ----------------------
  
  # PID and candidate eval
  group_by(df, PID3) %>%
    summarize(Avg = mean(ObamaAdv, na.rm=T))

  # PID and dogs
  group_by(df, PID3) %>%
    summarize(Dogs = mean(PetDog, na.rm = T))


# Multiple reg
# ----------------------
  
  # sparse controls
  m2 = lm(ObamaAdv ~ PetDog + PID3 + IdeologyLR + NationalEconomy + FamilyFinance, data = df)
  
  # fuller control 
  m3 = lm(ObamaAdv ~ PetDog + PetHorse + 
            Age + Income, data = df)
  
  # stargazer output
  stargazer(m2, m3, type = 'text', keep.stat = 'n')
  