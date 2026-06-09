# Loading libraries
library(nhanesA)
library(tidyverse)

# Pull demo data for 2017-2018 cycle
demo <- nhanes('DEMO_J')

# Quick look
glimpse(demo)
names(demo)

# Checking military service var
table(demo$DMQMILIZ, useNA = "always")
