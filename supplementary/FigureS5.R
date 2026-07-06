#Load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(EDIutils)
#remotes::install_github("bmcafee/EDIutilsAddons")
library(EDIutilsAddons) #<- get_data("knb-lter-ntl.17.39", filenum = 4)
library(LakeMetabolizer)

#Install NEON dissovled gas package from github (requires devtools)
library(devtools)
#install_github("NEONScience/NEON-dissolved-gas/neonDissGas", force = TRUE, dependencies = TRUE)
library(neonDissGas)

##### Constants #####
cGas<-8.3144598 #universal gas constant (J K-1 mol-1)
cKelvin <- 273.15 #Conversion factor from Kelvin to Celsius
cPresConv <- 0.000001 # Constant to convert mixing ratio from umol/mol (ppmv) to mol/mol. Unit conversions from kPa to Pa, m^3 to L, cancel out.
cT0 <- 298.15#Henry's law constant T0
cConcPerc <- 100 #Convert to percent

#Henry's law constants and temperature dependence from Sander (2015) DOI: 10.5194/acp-15-4399-2015
ckHCO2 <- 0.00033 #mol m-3 Pa, range: 0.00031 - 0.00045
cdHdTCO2 <- 2400 #K, range: 2300 - 2600


#Read in data
gas_depart_calc<- read_csv("supplementary/data/gas_departure_calc.csv")
macrophyte<- read_csv("supplementary/data/sites.csv")

#Create new column with moles of O2
gas_depart_calc$ODO_uM <- (gas_depart_calc$DO_mgL / 1000) / 32 * 1e6

gas_depart_calc$CO2uM<- gas_depart_calc$headspaceCO2
Saturations <- gas_depart_calc %>%
  mutate(
    barom_pres = barometricPressure,  # Replace NA with mean if needed
    waterTemp = waterTemp,  # Ensure numeric conversion
    satConcCO2 = (ckHCO2 * exp(cdHdTCO2 * (1 / (waterTemp + cKelvin) - 1 / cT0))) * 
      concentrationCO2Air * barom_pres * cPresConv * 1000000) %>%
  mutate( CO2_dep = CO2uM - satConcCO2)%>%
  mutate(season = ifelse(month(date) < 3, "under-ice", 
                         ifelse(month(date) > 2 & month(date) <6, "Spring",
                                ifelse(month(date) > 8, "Fall", "Summer"))))%>%
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring", "under-ice")))

saturations_plot<- Saturations%>%
  group_by(date, site)%>%
  summarize(CO2_dep = mean(CO2_dep),
            o2_departure = mean(o2_departure))%>%
  left_join(macrophyte, by = c("site"="Site"))%>%
  mutate(zone = ifelse(site == 1 | site == 5, "pelagic", "littoral"))%>%
  mutate(season = ifelse(month(date) < 3, "under-ice", 
                         ifelse(month(date) > 2 & month(date) <6, "Spring",
                                ifelse(month(date) > 8, "Fall", "Summer"))))%>%
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring", "under-ice")))


ggplot(saturations_plot)+
  geom_hline(yintercept = 0, alpha = 0.75)+
  geom_vline(xintercept = 0, alpha = 0.75)+
  geom_point(aes(x = CO2_dep, y = o2_departure, fill = season), size = 2, shape =21)+
  geom_abline(slope = -1, intercept = 0, linetype= "dashed")+
  ylab(expression(paste(O[2], " departure (", µ,"mol ", L^-1,")")))+
  xlab(expression(paste("C", O[2], " departure (", µ,"mol ", L^-1,")")))+
  scale_fill_manual(values =c("#009E73","#D55E00", "#CC79A7", "darkblue"))+
  theme_bw(base_size = 16)+
  theme(legend.title=element_blank())
ggsave(filename = 'supplementary/figures/FigureS5.png',width = 6,height = 4,units = 'in')


