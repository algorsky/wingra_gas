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

### Manuscript Figures

The scripts numbered `Figure1.R` through `Figure7.R` correspond directly to the figures in the manuscript.

### Directories

*   `data/`: Contains the raw and processed datasets required to run the scripts (e.g., alkalinity, CO₂ measurements, profiles, and GIS shapefiles).
*   `figures/`: Output directory where generated plots and maps are saved.

## Requirements

The analysis is performed in R. Key packages required include:

*   `tidyverse` (data manipulation and plotting)
*   `sf`, `terra`, `raster`, `sp` (spatial data handling and GIS)
*   `mgcv` (Generalized Additive Models for spatial interpolation)

## License

This information is released under the Creative Commons license - Attribution - CC BY (https://creativecommons.org/licenses/by/4.0/). The consumer of these data ("Data User" herein) is required to cite it appropriately in any publication that results from its use. The Data User should realize that these data may be actively used by others for ongoing research and that coordination may be necessary to prevent duplicate publication. The Data User is urged to contact the authors of these data if any questions about methodology or results occur. Where appropriate, the Data User is encouraged to consider collaboration or co-authorship with the authors. The Data User should realize that misinterpretation of data may occur if used out of context of the original study. While substantial efforts are made to ensure the accuracy of data and associated documentation, complete accuracy of data sets cannot be guaranteed. All data are made available "as is." The Data User should be aware, however, that data are updated periodically and it is the responsibility of the Data User to check for new versions of the data. The data authors and the repository where these data were obtained shall not be liable for damages resulting from any use or misinterpretation of the data. Thank you.
