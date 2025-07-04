#####################################################################
# Rheadspace.R
#
# R function to calculate pCO2 in a water sample (micro-atm) using a complete headspace method accounting for the 
# carbonate ewuilibrium in the equilibration vessel. 
#
# Authors: Rafael Marcé (Catalan Institute for Water Research - ICRA)
#          Jihyeon Kim (Université du Québec à Montréal - UQAM) 
#          Yves T. Prairie (Université du Québec à Montréal - UQAM)
#
# Date: December 2020
#
# Copyright statement: This code is shared under GNU GENERAL PUBLIC LICENSE Version 3. 
# Refer to the LICENSE file in the Github repository for details.
# Please, when using this software for scientific purposes, cite this work as a source:
#
#   Koschorreck, M., Y.T. Prairie, J. Kim, and R. Marcé. 2020. Technical note: CO2 is not like CH4 – limits of the headspace method to analyse pCO2 in water. Biogeosciences, in revision
#
# Contact information: Rafael Marcé (rmarce@icra.cat)
#
# INPUT: 
#       You can either input a vector of 11 values for solving a single sample or a data frame of 11
#       columns and an arbitrary number of rows for batch processing of several samples.
#       
#       If supplying a vector for one sample, the vector should contain (in this order):
# 
#         1. The ID of the sample (arbitrary test, e.g."Sample_1")
#         2. mCO2 (ppmv) of the headspace "before" equilibration (e.g., zero for nitrogen)
#         3. mCO2 (ppmv) of the headspace "after" equilibration (e.g., as measured by a GC)
#         4. In situ (field) water temperature in degrees celsius
#         5. Water temperature after equilibration in degree celsius
#         6. Alkalinity (micro eq/L) of the water sample
#         7. Volume of gas in the headspace vessel (mL)
#         8. Volume of water in the headspace vessel (mL)
#         9. Barometric pressure at field conditions in kPa. 101.325 kPa = 1 atm 
#        10. Set of constants for carbonate equilibrium calculations (1=Freshwater, Millero 1979; 2=Estuarine, Millero 2010; 3=Marine, Dickson et al 2007) 
#        11. Salinity (PSU) # Set to zero if option in 10 is set to 1.
#
#       If supplying a data frame, you can build it importing of a csv file
#       Example: dataset <- read.csv("R_test_data.csv")
#       The first row of this file must contain column names, then one row for each sample to be solved.
#       The columns names must be:
# 
#         1. Sample.ID 
#         2. HS.mCO2.before
#         3. HS.mCO2.after
#         4. Temp.insitu
#         5. Temp.equil
#         6. Alkalinity.measured
#         7. Volume.gas
#         8. Volume.water
#         9. Bar.pressure
#        10. Constants
#        11. Salinity
#
#       For the different samples, values must be as follows:
#
#         Sample.ID #User defined text
#         HS.mCO2.before #the pCO2 (ppmv) of the headspace "before" equilibration (e.g. zero for nitrogen)
#         HS.mCO2.after #the measured pCO2 (ppmv) of the headspace "after" equilibration
#         Temp.insitu #in situ (field) water temperature in degrees celsius
#         Temp.equil #the water temperature after equilibration in degree celsius
#         Alkalinity.measured #Total alkalinity (micro eq/L) of the water sample
#         Volume.gas #Volume of gas in the headspace vessel (mL)
#         Volume.water #Volume of water in the headspace vessel (mL)
#         Bar.pressure #Barometric pressure at field conditions in kPa. 101.325 kPa = 1 atm   
#         Constants #Set of constants for carbonate equilibrium calculations (1=Freshwater; 2=Estuarine; 3=Marine) 
#         Salinity # Salinity in PSU, set to zero if option in 10 is set to 1.
#
#

#
# OUTPUT: a data frame containing:
#      1. Sample IDs
#      2. mCO2 complete headspace (ppmv) # mCO2 calculated using the complete headspace method accounting for the carbonate equilibrium
#      3. pCO2 complete headspace (micro-atm) # pCO2 calculated using the complete headspace method accounting for the carbonate equilibrium
#      4. CO2 concentration complete headspace (micro-mol/L) # CO2 concentration calculated using the complete headspace method accounting for the carbonate equilibrium
#      5. pH  # pH calculated for the sanple at in situ field conditions (using the complete headspace method)
#      6. mCO2 simple headspace (ppmv)  # mCO2 calculated using the simple headspace method NOT accounting for the carbonate equilibrium
#      7. pCO2 simple headspace (micro-atm)  # pCO2 calculated using the simple headspace method NOT accounting for the carbonate equilibrium
#      8. CO2 concentration simole headspace (micro-mol/L) # CO2 concentration calculated using the simple headspace method NOT accounting for the carbonate equilibrium
#      9. % error # error associated with using the simple headspace calculation
#
#
# REFERENCES
#
#  Dickson, A.G & J.P Riley (1979). The estimation of acid dissociation constants in sea-water media
#  from potentiometric titrations with strong base. II. The dissociation of phosphoric acid,
#  Marine Chemistry, 7(2), 101-109.  
#
#  Dickson, A. G., Sabine, C. L., and Christian, J. R. (2007). Guide to best practices for
#  ocean CO2 measurements, PICES Special Publication 3, 191 pp.
#
#  Millero, F. (1979). The thermodynamics of the carbonate system in seawater,
#  Geochimica et Cosmochimica Acta, 43(10), 1651-1661.  
#
#  Millero, F. (2010). Carbonate constants for estuarine waters,
#  Marine and Freshwater Research, 61(2), 139.
#
#  Orr, J. C., Epitalon, J.-M., and Gattuso, J.-P. (2015). Comparison of ten packages that compute
#  ocean carbonate chemistry, Biogeosciences, 12, 1483–1510. 
#
#  Weiss, R.F. (1974). Carbon dioxide in water and seawater: the solubility of a non-ideal gas,
#  Marine Chemistry, 2, 203-215.
# 

#####################################################################

Rheadspace <-  function(...){
  arguments <- list(...)
  
  # test arguments and initialize variables
  if (is.data.frame(arguments[[1]])) {
    input.table=arguments[[1]]
    if (dim(input.table)[2]!=11){
      stop("You should input a data frame with 11 columns. See the readme file or comments in the function", call.=FALSE)
    }else{
      Sample.ID = as.character(input.table$Sample.ID)
      mCO2_headspace = input.table$HS.mCO2.before #the mCO2 (ppmv) of the headspace "before" equilibration
      mCO2_eq = input.table$HS.mCO2.after #the measured mCO2 (ppmv) of the headspace "after" equilibration
      temp_insitu = input.table$Temp.insitu #in situ water temperature in degrees celsius
      temp_eq = input.table$Temp.equil #the water temperature after equilibration in degree celsius
      alk = input.table$Alkalinity.measured #Total alkalinity (micro eq/L) of the water sample
      vol_gas = input.table$Volume.gas #Volume of gas in the headspace vessel (mL)
      vol_water = input.table$Volume.water #Volume of water in the headspace vessel (mL)   
      Bar.pressure = input.table$Bar.pressure #Barometric pressure at field conditions in kPa. 101.325 kPa = 1 atm   
      c_constants = input.table$Constants #Constants for carbonate equilibrium (1=Freshwater; 2=Estuarine; 3=Marine) 
      Salinity = input.table$Salinity #Salinity in PSU. Set to zero if Constants = 1
    } 
  } else if (length(arguments)==11) {
    Sample.ID = as.character(arguments[[1]])
    mCO2_headspace = arguments[[2]] #the mCO2 (ppmv) of the headspace "before" equilibration
    mCO2_eq = arguments[[3]] #the measured mCO2 (ppmv) of the headspace "after" equilibration
    temp_insitu = arguments[[4]] #in situ water temperature in degrees celsius
    temp_eq = arguments[[5]] #the water temperature after equilibration in degree celsius
    alk = arguments[[6]] #Total alkalinity (micro eq/L) of the water sample
    vol_gas = arguments[[7]] #Volume of gas in the headspace vessel (mL)
    vol_water = arguments[[8]] #Volume of water in the headspace vessel (mL)   
    Bar.pressure = arguments[[9]] #Barometric pressure at field conditions in kPa. 101.325 kPa = 1 atm   
    c_constants = arguments[[10]] #Constants for carbonate equilibrium (1=Freshwater; 2=Estuarine; 3=Marine) 
    Salinity = arguments[[11]] #Salinity in PSU. Set to zero if Constants = 1
  } else {
    stop("You should input either a data frame or a vector of 11 values. See the readme file or comments in the function", call.=FALSE)
  }
  
  
  
  #initialization of variables
  pCO2_orig <- data.frame(matrix(NA,length(mCO2_headspace),9))
  names(pCO2_orig) <- c("Sample.ID","mCO2 complete headspace (ppmv)","pCO2 complete headspace (micro-atm)", "CO2 concentration complete headspace (micro-mol/L)", "pH", "mCO2 simple headspace (ppmv)", "pCO2 simple headspace (micro-atm)", "CO2 concentration simple headspace (micro-mol/L)", "% error")
  
  R <- 0.082057338 #L atm K-1 mol-1
  
  #the function uniroot cannot handle vectors, so we need a loop
  for (i in 1:length(mCO2_headspace)){ 
    
    AT = alk[i]*(1e-6) #conversion to mol/L
    
    #Constants of the carbonate ewuilibrium
    # Kw = the dissociation constant of H2O into H+ and OH-
    # Kh = the solubility of CO2 in water - equilibration conditions
    # Kh2 = the solubility of CO2 in water - in situ field conditions
    # K1 = the equilibrium constant between CO2 and HCO3-
    # K2 = the equilibrium constant between HCO3- and CO3 2-
    
    # Solubility coefficients from Weiss (1974) with Sal=0 for freshwater option
    # Dissociation of water from Dickson and Riley (1979)
    
    
    K1=10^-(-126.34048+6320.813/(temp_eq[i]+273.15)+19.568224*log(temp_eq[i]+273.15))
    K2=10^-(-90.18333+5143.692/(temp_eq[i]+273.15)+14.613358*log(temp_eq[i]+273.15))
    
    Kw = exp(148.9652-13847.26/(temp_eq[i]+273.15)-23.6521*log(273.15+temp_eq[i]))
    Kh = 10^((-60.2409+93.4517*(100/(273.15+temp_eq[i]))+23.3585*log((273.15+temp_eq[i])/100))/log(10)) # mol/L/atm equilibration conditions
    Kh2 = 10^((-60.2409+93.4517*(100/(273.15+temp_insitu[i]))+23.3585*log((273.15+temp_insitu[i])/100))/log(10)) # mol/L/atm original conditions
    
    
    HS.ratio <- vol_gas[i]/vol_water[i] #Headspace ratio (=vol of gas/vol of water)
    
    #The following calculations assume 1 atm, this is corrected later for measured pressure in the field.
    
    #DIC at equilibrium
    co2 <- Kh * mCO2_eq[i]/1000000
    h_all <- polyroot(c(-(2*K1*K2*co2),-(co2*K1+Kw),AT,1))
    real<-Re(h_all)
    h <-real[which(real>0)]
    DIC_eq <- co2 * (1 + K1/h + K1 * K2/(h * h))
    
    #DIC in the original sample
    DIC_ori <- DIC_eq + (mCO2_eq[i] - mCO2_headspace[i])/1000000/(R*(temp_eq[i]+273.15))*HS.ratio
    
    #pCO2 in the original sample
    h_all <- polyroot(c(-(K1*K2*Kw),K1*K2*AT-K1*Kw-2*DIC_ori*K1*K2,AT*K1-Kw+K1*K2-DIC_ori*K1,AT+K1,1))
    real<-Re(h_all)
    h <-real[which(real>0)]
    
    co2 <- h* (DIC_ori * h * K1/(h * h + K1 * h + K1 * K2)) / K1
    
    pCO2_orig[i,1] <- as.character(Sample.ID[i])
    pCO2_orig[i,2] <- co2/Kh2*1000000
    pCO2_orig[i,3] <- pCO2_orig[i,2]*Bar.pressure[i]/101.325
    pCO2_orig[i,4] <- co2*1000000
    pCO2_orig[i,5] <- -log10( h )
    
    
    #Calculation not accounting for alkalinity effects and associated error
    
    #concentration and total mass in the water sample assuming ideal gas from the pCO2 measured at the headspace
    CO2_solution <- mCO2_eq[i]/1000000*Kh #mol/L
    CO2_solution_mass <- CO2_solution * vol_water[i]/1000 #mol
    
    #mass of CO2 in the measured headspace
    final_C_headspace_mass <- mCO2_eq[i]/1000000*(vol_gas[i]/1000) / (R * (temp_eq[i]+273.15)) #mol
    
    mols_headspace <- mCO2_headspace[i]/1000000*(vol_gas[i]/1000)/(R * (temp_eq[i]+273.15)) #mol PV / RT = n
    
    #implication: mass, concentration, and partial pressure of CO2 in the original sample (aount in sample and headspace after equilibration minus original mass in the headspace)
    Sample_CO2_mass <- CO2_solution_mass + final_C_headspace_mass - mols_headspace #mol
    Sample_CO2_conc <- Sample_CO2_mass/(vol_water[i]/1000) #mol/L
    pCO2_orig[i,6] <- Sample_CO2_conc/Kh2*1000000 #ppmv
    pCO2_orig[i,7] <- pCO2_orig[i,6]*Bar.pressure[i]/101.325 # micro-atm
    pCO2_orig[i,8] <- Sample_CO2_conc*1000000 # micro-mol/L
    #calculation of the error
    pCO2_orig[i,9] <- (pCO2_orig[i,6]-pCO2_orig[i,2])/pCO2_orig[i,2] *100  #%
  }
  
  
  return(pCO2_orig) #Output data frame
  
}

############### Dissolved Gas #####################
dataset <- read_csv("data/dat_cross_headspace.csv")
pCO2 <- Rheadspace(dataset)
