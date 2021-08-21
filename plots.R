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
pacman::p_load(readxl, ggplot2, tidyverse, tidypaleo, patchwork, plyr)


# DATA -----------------------------------------
## Abiotic data -----------------------------------

sis188_abi <- read_excel("Tabela sis 188 e sis 249 dados abioticos.xlsx", 
                         sheet = "sis188") #read data


sis249_abi <- read_excel("Tabela sis 188 e sis 249 dados abioticos.xlsx", 
                         sheet = "sis249") #read data

abi <- read_excel("Tabela sis 188 e sis 249 dados abioticos.xlsx", 
                  sheet = "abi")

abi <- select(abi, -`Volume    (µm3)`, 
              -`Elongation Index`,
              -`Length  (µm)`,
              -`TOC (%)`,
              -`Accum Biomass`,
              -`Uvigerinidae`)


### checking plot abi
ggplot() +
  geom_line(abi, mapping = aes(x = age, y = `d13C Uvig`, col = CORE))

### transpose abiotic data for plots

abi_trans <- pivot_longer(abi, cols = `Detrital mud  (%) `:`d18O Uvig`, 
                                 names_to = "param", values_to = "value")

### change parameters name to plot labels
abi_trans$param <- recode(abi_trans$param, 
                          `Detrital mud  (%) ` = 'paste("Detrital mud (%)")',
                          `Carbonate (%)   ` = 'paste("Carbonate  (%)")',
                          `d13C Uvig` = 'paste(delta^13*C, " Uvig ", "(\u2030)")',
                          `d18O Uvig` = 'paste(delta^18*O, " Uvig ", "(\u2030)")')


## Biotic data -------------
### Relative abundance by species -----------------------------------
#sis 188
sis188_abund <- read_excel("dados.xlsx", sheet = "sis188_abund") #read data

sis188_abund <- pivot_longer(sis188_abund, cols = Tangulosa:Udirrupta,
                                    names_to = "spp", values_to = "abund")

#sis249
sis249_abund <- read_excel("dados.xlsx", sheet = "sis249_abund") #read data


sis249_abund <- pivot_longer(sis249_abund, cols = Tangulosa:Uhispidocostata, 
                                    names_to = "spp", values_to = "abund")


abundancia <- bind_rows("sis249" = sis249_abund, 
                        "sis188" =  sis188_abund, .id = 'core')

#abund <- mutate(abund, age_norm = age / 1000)
abundancia$spp <- recode(abundancia$spp, Tangulosa = "T. angulosa",
                         Uauberiana = "U. auberiana",
                         Umediterranea = "U. mediterranea",
                         Uhispidocostata = "U. hispidocostata",
                         Uperegrina = "U. peregrina",
                         Udirrupta = "U. dirrupta")



### Biomass by species -----------------------------------

#sis 188
sis188_bio <- read_excel("dados.xlsx", sheet = "sis188_bio") #read data


#sis249
sis249_bio <- read_excel("dados.xlsx", sheet = "sis249_bio") #read data


biomassa <- bind_rows("sis249" = sis249_bio, 
                      "sis188" =  sis188_bio, .id = 'core')

#abund <- mutate(abund, age_norm = age / 1000)
biomassa$spp <- recode(biomassa$spp, "Trifarina angulosa" = "T. angulosa",
                       "Uvigerina auberiana" = "U. auberiana",
                       "Uvigerina mediterranea" = "U. mediterranea",
                       "Uvigerina hispidocostata" = "U. hispidocostata",
                       "Uvigerina peregrina" = "U. peregrina",
                       "Uvigerina dirrupta" = "U. dirrupta")



#Average biomass by species - sis 188
mean_biomassa <-ddply(biomassa,c("core","spp","age"),
                      summarise,
                      mean=mean(biomass)) #average 
mean_biomassa$sd <-ddply(biomassa,c("core","spp","age"),
                         summarise,
                         sd=sd(biomass))$sd #standard deviation
mean_biomassa$se <-ddply(biomassa,c("core","spp","age"),
                         summarise,
                         se=sd(biomass)/sqrt(length(biomass)))$se #standard error


## Age = idade -----------------------------------
idade <- read_excel("SIS188_modelo de idade atualizado.xlsx") 
idade <- mutate(idade, age_norm = age / 1000) %>%
  mutate_at(4, as.character)




# PLOTS  -----------------------------------
library(gsloid) #This package provides boundary ages for Marine Isotope Stages (MIS)
mis_last_125ka <- LR04_MISboundaries[LR04_MISboundaries$LR04_Age_ka_start <= 125, ]

mis_last_125ka <- add_column(mis_last_125ka, label_fill = c("a","b","a","b","a","b","a","b","a"))

## age/depth  -----------------------------------

ggplot()+
  geom_rect(data = mis_last_125ka, 
            aes(xmin=LR04_Age_ka_end, xmax=LR04_Age_ka_start, 
                ymin=-Inf, ymax=Inf, 
                fill=label_fill), 
            color = "transparent", alpha=0.5)+
  geom_text(data = mis_last_125ka,
            aes(x = LR04_Age_ka_mid, y = 330, label = label_MIS),
            size = 3, check_overlap = TRUE)+
  scale_fill_manual(values = c("grey70", "white"))+
  geom_lineh(data = idade, aes(x = age_norm, y = depth, linetype = Core), 
             size = .8)+
  geom_point(data = idade, aes(x = age_norm, y = depth, colour = Core))+
  scale_colour_manual(values = c("black", "black"))+
  theme_paleo()+
  theme(legend.position="bottom", 
        legend.title = element_blank(),
        legend.justification='left')+
  guides(fill=FALSE)+
  scale_y_reverse(name = "Depth (cm)", 
                  scales::pretty_breaks(n = 10), 
                  expand = c(0,0), limits = c(350, 0))+
  scale_x_continuous(name = "Age (ka BP)", 
                     scales::pretty_breaks(n = 10),
                     position = "top", expand = c(0,0), limits = c(0, 115)) +
  expand_limits(x = 0, y = 0)

ggsave(file="fig3.pdf", width = 91, height = 150, units = "mm", dpi = "print")

## Abiotic  -------------

ggplot()+
  geom_rect(data = mis_last_125ka, 
            aes(ymin=LR04_Age_ka_end, ymax=LR04_Age_ka_start, 
                xmin=-Inf, xmax=Inf, 
                fill=label_fill), 
            color = "transparent", alpha=0.5)+
  scale_fill_manual(values = c("grey70", "white"))+
  geom_lineh(data = abi_trans[!is.na(abi_trans$value),], 
             aes(x = value, y = age, linetype = CORE), size = .8)+
  geom_point(data = abi_trans, aes(x = value, y = age, colour = CORE))+
  scale_colour_manual(values = c("black", "black"))+
  scale_y_reverse(name = "Age (ka BP)", 
                  scales::pretty_breaks(n = 10), 
                  expand = c(0,0), limits = c(115, 0))+
  scale_x_continuous(name = "", 
                     scales::pretty_breaks(n = 5)) +
  theme_paleo()+
  theme(legend.position="top", 
        legend.title = element_blank(),
        legend.justification='left',
        strip.placement = "outside",
        strip.background = element_blank())+
  guides(fill=FALSE)+
  facet_wrap(~param, ncol = 6, strip.position = "bottom",
             scales = "free_x", labeller = label_parsed)

ggsave(file="fig4.pdf", width = 130, height = 180, units = "mm", dpi = "print")

## Relative abundance  -------------

g1 <- ggplot()+
  geom_rect(data = mis_last_125ka, 
            aes(ymin=LR04_Age_ka_end, ymax=LR04_Age_ka_start, 
                xmin=-Inf, xmax=Inf, fill=label_fill), color = "transparent", alpha=0.5)+
  scale_fill_manual(values = c("grey70", "white"))+
  geom_lineh(data = abundancia, 
             aes(x = abund, y = age, linetype = core), size = .8)+
  scale_y_reverse(name = "Age (ka BP)", 
                  scales::pretty_breaks(n = 10), 
                  expand = c(0,0), limits = c(115, 0))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_paleo()+
  theme(legend.position="top", 
        legend.title = element_blank(),
        legend.justification='left',
        strip.text = element_text(face = "italic"))+
  guides(fill=FALSE, linetype = FALSE)+
  facet_grid (.~ spp, space = "free_x", scales = "free_x")+
  rotated_facet_labels(
    angle = 30,
    direction = "x",
    remove_label_background = TRUE)+
  labs(x = "Relative abundance (%)", y = "Age (ka BP)")



## Biomass  -------------

g2 <- ggplot()+
  geom_rect(data = mis_last_125ka, 
            aes(ymin=LR04_Age_ka_end, ymax=LR04_Age_ka_start, 
                xmin=-Inf, xmax=Inf, fill=label_fill), color = "transparent", alpha=0.5)+
  scale_fill_manual(values = c("grey70", "white"))+
  geom_lineh(data = mean_biomassa, 
             aes(x = mean, y = age, linetype = core), size = .8)+
  scale_color_manual(values = c("black", "gray55"))+
  scale_y_reverse(name = "", 
                  scales::pretty_breaks(n = 10), 
                  expand = c(0,0), limits = c(115, 0))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_paleo()+
  theme(legend.title = element_blank(),
        legend.position = c(.78, 0.005),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(4, 4, 4, 4),
        strip.text = element_text(face = "italic"))+
  guides(fill=FALSE)+
  facet_grid (.~ spp, space = "free_x", scales = "free_x")+
  rotated_facet_labels(
    angle = 30,
    direction = "x",
    remove_label_background = TRUE)+
  labs(x = expression(paste("Biomass ","(", mu ,C[org],')')), 
       y = NULL)


(g1 + labs(title = "A")) | (g2 + labs(title = "B")) 

ggsave(file="fig5.pdf", width = 230, height = 190, units = "mm", dpi = "print")

### Rel. Abund x Acc. Biomass  -------------

abi <- read_excel("Tabela sis 188 e sis 249 dados abioticos.xlsx", 
                  sheet = "abi") # open abi again to include `Accum. Biomass`
b1 <- ggplot()+
  geom_point(data = abi, 
             aes(x = `Accum Biomass` , y = Uvigerinidae, shape = CORE), size = 2)+
  scale_shape_manual(values = c(16, 5))+
  geom_text(data=subset(abi, `Accum Biomass` > 18 | Uvigerinidae > 30),
            aes(x = `Accum Biomass` , 
                y = Uvigerinidae,
                label = paste0(round(age, digits = 1), " (ka BP)")), 
            vjust = 0, nudge_y = 1, size = 3)+
  theme_paleo()+
  theme(legend.title = element_blank(),
        legend.position = c(0.98, 0.98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2))+
  labs(y = "Relative abundance (%)", title = "A",
       x = expression(paste("Accumulated Biomass ","(", mu ,C[org],')')))


### acc. biomass in time -------------

b2 <- ggplot()+
  geom_rect(data = mis_last_125ka, 
            aes(ymin=LR04_Age_ka_end, ymax=LR04_Age_ka_start, 
                xmin=-Inf, xmax=Inf, fill=label_fill), color = "transparent", alpha=0.5)+
  scale_fill_manual(values = c("grey70", "white"))+
  geom_lineh(data = abi, 
             aes(x = `Accum Biomass`, y = age, linetype = CORE), size = .8)+
  scale_color_manual(values = c("black", "gray55"))+
  scale_y_reverse(name = "Age (ka BP)", 
                  scales::pretty_breaks(n = 10), 
                  expand = c(0,0), limits = c(115, 0))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_paleo()+
  theme(legend.title = element_blank(),
        legend.position = c(.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2))+
  guides(fill=FALSE)+
  labs(x = expression(paste("Accumulated Biomass ","(", mu ,C[org],')')), 
       y = "Age (ka BP)", title = "B")


wrap_plots(b1, b2, ncol = 1)

ggsave(file="fig6.pdf", width = 90, height = 170, units = "mm", dpi = "print")

### Size data -------------
# rename and transpose data to plot
biomassa <- bind_rows("sis249" = sis249_bio, 
                      "sis188" =  sis188_bio, .id = 'core')

biomassa$spp <- recode(biomassa$spp, "Trifarina angulosa" = "T. angulosa",
                       "Uvigerina auberiana" = "U. auberiana",
                       "Uvigerina mediterranea" = "U. mediterranea",
                       "Uvigerina hispidocostata" = "U. hispidocostata",
                       "Uvigerina peregrina" = "U. peregrina",
                       "Uvigerina dirrupta" = "U. dirrupta")

bio_trans <- select(biomassa, -fraction, -model, -vol, -biovol, 
                    -biomass, -z_a, -achat, -achat_norm)

bio_trans <- tidyr::pivot_longer(bio_trans, cols = depth:along, 
                                 names_to = "param", values_to = "value")

bio_trans <- arrange(bio_trans, desc(age))

bio_trans$param <- recode(bio_trans$param, 
                          `along` = 'paste("Elongation (",mu, "m)")',
                          `depth` = 'paste("Thickness (",mu, "m)")',
                          `length` = 'paste("Length (",mu, "m)")',
                          `width` = 'paste("Width (",mu, "m)")')

## plot

ggplot() +
  geom_boxplot(data = bio_trans, 
               aes(x = reorder(age, desc(age)), 
                   y = value))+
  geom_rect(xmax = as.numeric(mis_last_edited_188$LR04_Age_ka_end), 
            xmin=as.numeric(mis_last_edited_188$LR04_Age_ka_start), 
            ymin=-Inf, ymax=Inf, color = "transparent", alpha=0.5)+
  scale_fill_manual(values = c("grey70", "white"))+
  coord_flip()+
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  guides(fill=FALSE)+
  theme_paleo()+
  theme(strip.placement = "outside",
        strip.background = element_blank())+
  facet_grid(core~param, scales = "free", 
             labeller = label_parsed, switch="x")+
  labs(y = NULL, 
       x = "Age (ka BP)")


ggsave(file="fig7.pdf", width = 180, height = 220, units = "mm", dpi = "print")

