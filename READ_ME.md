################ Read Me ################

Author: Charlie Dougherty

Contact information: `cedougherty@wisc.edu`

This repository contains all of the (somewhat cleaned) scripts from my Master's thesis at the Center for Limnology. It contains jupyter notebook files (in the python folder) that conduct spectral mixture 
analysis (SMA) on the lake ice of Taylor Valley lakes, in order to quantify sediment coverage through time using Landsagt 8 imagery. It also contains numerous data scripts breaking down the SMA outputs to compare against lake ice thicknesses. There is also an ice thickness model (uses the 1-D heat equation) for East Lobe Bonney. There's quite a bit in here, so if you have any questions please contact me!

Lake ice thickness and meteorological data comes from the McMurdo Long Term Ecological Research Project (MCM LTER, https://mcm.lternet.edu/). Please cite the LTER for any data usage (long term data is important!). 

Landsat 8 data was accessed through Google Earth Engine, and was provided courtesy of the United States Geological Survey. 

In terms of use scripts: 
R: 

_hotspot_analysis.R_ <- this recreates hotspot plots of sediment coverage over TV Lakes

_ice_drop_compared_to_sed.R_ <- this recreates the comparison plot of drop in ice vs. previous years sediment cover

_ice_thickness_model_east_lake_bonney.R_ <- Ice thickness model (using heat equation) of ELB. Very long script, requires download of LTER met data.

_Surface_sediment_data_munging.R_ <- this contains scripts to recreate the majority of plots in Chapter 1 of my thesis. 

_TVLakes_mean_sed_bb_fromraster.R_ <- This allows you to grab a mean sediment cover value from the GEE outputs at a single buffer distance or multiple distances. 

_TVLakes_RGB_plotting_separate_lakes.R_ <- allows you to plot RGB images of Landsat 8 images.

_Wind_DEM_for_Aeolian.R_ <- allows you to compare the alignment of a slope with wind for aeolian entrainment. Requires download of DEM of Taylor Valley. 

Python: 
I won't go through each script one by one, but the _pan_ scripts allows you to create outputs of a Landsat 8 repo exporting only the panchromatic band. The _RGB_ scripts allow you to output only bands 2-4 of landsat 8 to plot RGB images of scenes. The _SMA_ scripts are the spectral mixture scripts for all of the different lakes. 
