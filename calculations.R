#====================================================
# Code related to the paper:
# Santana et al. "Biometric and biomass analysis of Quaternary Uvigerinidae (Foraminifera) from the Southern Brazilian continental slope." 
# Marine Micropaleontology, 2021. https://doi.org/10.1016/j.marmicro.2021.102041
#
# GitHub: https://github.com/ThaiseRF/Santana_etal_2021
#
# Authors: Beatriz Santana, Thaise Freitas, Juliana Leonel, Carla Bonetti
# Date: 21/08/2021
#====================================================

# clear workspace
rm(list = ls())
gc()

# load packages --------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(readxl, tidyverse, tidypaleo, patchwork, forImage)

##Import size data -----------------------------

sis188 <- read_excel("sis188.xlsx")

# Uvigerinids size was measured with the forImage package. 
# The model choosed is cone + half ellipsoid. 
# This model requires three measurements, test height (h), diameter one (d_one) and two (d_two).
# In the manuscript, we called d_one = thickness, h = length, and d_two = width.
# The measured size needs to be wrangled to fit the volumetric functions requirements, so the values match the attributed measure.
# So we modified the column in relation to the value of the two dimensional measures.
# In case diamA (h) is bigger than diamB (d_two) - diamA remains, if FALSE the columns values are exchanged.


t188 <- sis188 %>%
  rowwise() %>%
  mutate(h = case_when(diamA > diamB ~ diamA, TRUE ~ diamB),
         d_two = case_when(diamA > diamB ~ diamB, TRUE ~ diamA)) %>%
  select(-diamA, -diamB)

#renaming Z as d_one ((Z = thickness) was retrieved with forImage depth.xml function)
names(t188)[8] <- "d_one"


## Calculating biovolume and biomass for all individuals --------------------

# The genus Angulogerina was used as base for the percent cell occupancy for the uvigerinids (70.57% PCO).
t188 <- bio.volume(t188, genus = "angulogerina")

t188 <- biomass(t188, method = "michaels")

### Calculating Elongation (width/length) -------------------------

t188 <- t188 %>%
  rowwise() %>%
  mutate(elong = d_two/h)


write.table(t188, "t188.csv", sep = ";", dec = ",")