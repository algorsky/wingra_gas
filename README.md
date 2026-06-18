# Lake Wingra Gas Dynamics

This repository contains the data and code associated with the manuscript:

**Littoral macrophytes drive large spatial and temporal variability of carbon dioxide flux in a shallow hard-water lake**

**Authors:** Gorsky, A.L., M.E. Perga, D.K. Szydlowski, E.H. Stanley, and H.A. Dugan

**Target Journal:** Limnology and Oceanography

## Overview

This project investigates the spatial and temporal variability of carbon dioxide (CO₂) fluxes in Lake Wingra, a shallow hard-water lake in Wisconsin. The study specifically examines the role of littoral macrophytes in driving these gas dynamics. The repository provides all necessary R scripts to reproduce the spatial interpolations, data processing, upscaling calculations, and figures presented in the manuscript.

## Repository Structure

### Data Processing & Analysis Scripts

*   `0_Alkheadspace.R`: Processes alkalinity headspace data.
*   `0_alkalinity.R`: General alkalinity calculations.
*   `0_diffusive.R`: Calculates diffusive gas fluxes.
*   `0_maps.R` & `0_WingraLanduseMap.R`: Generates basic site maps and land use contextual maps.
*   `0_PolygonsVoronoi.R`: Generates Voronoi polygons for spatial weighting.
*   `1_Upscaling.R` & `1_UpscalingWinter.R`: Scales point measurements up to whole-lake estimates for open water and winter periods.
*   `2_FLAMe.R` & `2_FLAMe_CO2_gam.R`: Processes and interpolates high-resolution spatial data from the FLAMe (Fast Limnology Automated Measurements) platform. The GAM script uses 2D thin-plate splines for optimal spatial interpolation of surface CO₂ concentrations.

### Manuscript Figures

The scripts numbered `Figure1.R` through `Figure6.R` correspond directly to the figures in the manuscript.

### Directories

*   `data/`: Contains the raw and processed datasets required to run the scripts (e.g., alkalinity, CO₂ measurements, profiles, and GIS shapefiles).
*   `figures/`: Output directory where generated plots and maps are saved.

## Requirements

The analysis is performed in R. Key packages required include:

*   `tidyverse` (data manipulation and plotting)
*   `sf`, `terra`, `raster`, `sp` (spatial data handling and GIS)
*   `mgcv` (Generalized Additive Models for spatial interpolation)
*   `viridis` (color palettes for mapping)

## License

*(Add license information here, e.g., MIT License, CC-BY)*
