[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15486909.svg)](https://doi.org/10.5281/zenodo.15486909)
# Background
Damage by late spring frost is one of the main reasons for yield losses in German apple production. Over the last decades, a shift towards earlier apple blossom was observed. At the same time, an increased frost frequency was observed in some regions. 
We modelled the future start of apple bloom and the probability of frost damage for two future time periods: 2035-2065 and 2070-2100 under the four pathways SSP1-2.6, SSP2-4.5, SSP3-7.0 and SSP5-8.5. A detailed description of our method and the results can be found in the manuscript “Spatially explicit forecasting of frost damage in German apple cultivation under changing climatic conditions” by [Christine Schmitz](https://github.com/ChristineSchmitz), [Lars Caspersen](https://github.com/larscaspersen), [Johannes Kopton](https://github.com/johanneskopton), [Katja Schiffers](https://github.com/katjaschiffers) and [Eike Luedeling](https://github.com/eikeluedeling).
This repository contains all files needed to reproduce the modelling procedure presented in the manuscript. The code is mostly written in R (R Core team, 2021)[^1]. For phenology modelling, we mainly used functions from the packages `chillR` (Luedeling et al., 2024)[^2] and [`LarsChill`](https://github.com/larscaspersen/addition_chillR). We used several R packages to interpolate the results. The most important were `gstat` (Pebesma, 2004)[^3], `terra` (Hijmans, 2023)[^4], `stars` (Pebesma and Bivand, 2023)[^5] and `scam` (Pya, 2023)[^6]. The maps were produced with the package `rasterVis` (Perpi and Hijmans, 2023)[^7]. Further packages used in the analysis are mentioned within the code. 
Additionally, the STIF Python package (Kopton, 2024)[^8] was used to estimate the weather conditions at the exact location where the phenology observations were recorded.

# Repository content

## Code
The code folder contains all relevant scripts to prepare the data, predict future bloom dates, forecast frost damage, interpolate the results, and produce maps presenting the outcomes. 

[`01_data_preparation.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/01_data_preparation.R): This script automatically downloads weather data from the [DWD Climate Data Center](https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/), and combines the phenology observations from annual and immediate reporters to one file. 

[`02_frost_temp_kriging_paper.ipynb`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/02_frost_temp_kriging_paper.ipynb) is a Jupyter Notebook file. It is used to interpolate temperature data (observed at weather stations to the locations where apple phenology was observed. 

[`03_read_and_clean.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/03_read_and_clean.R) prepares phenology and weather data for the next steps. 

[`04_split_data_for_calibration.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/04_split_data_for_calibration.R) splits the phenology data into calibration and validation datasets. 

[`05_prepare_data_cluster_fitting.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/05_prepare_data_cluster_fitting.R) prepares data for model optimization on a computer cluster. We used the [Bonna](https://www.hpc.uni-bonn.de/en/systems/bonna) MPP cluster. 

[`06_run_calibration_hpc.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/06_run_calibration_hpc.R) optimizes the parameters of the PhenoFlex model based on temperature and phenology observations. We fitted the model with 20 different training datasets (10 repetitions for early and 10 repetitions for late ripening apple varieties). 

[`07_evaluate_calibrated_models.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/07_evaluate_calibrated_models.R) evaluates model performance. It calculates the RMSE and RPIQ for all repetitions for the calibration and the validation datasets. 

[`08_download_climate_scenarios.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/08_download_climate_scenarios.R) automatically downloads CMIP6 climate projections and baseline observations for Germany from the [Copernicus Climate Data store](https://cds.climate.copernicus.eu/). A free account to login to the database is required. Afterwards the temperature changes between the past and future scenarios (change scenarios) are calculated. 

[`09_summary_change_scenarios_and_weather_download.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/09_summary_change_scenarios_and_weather_download.R) summarizes the change scenarios generated based on several Generalized Climate Models. We summarized the change scenarios to optimistic, average, and pessimistic models and generated possible future weather (i.e. temperature) scenarios. 

[`10_sigmoide_curve_frost_damage.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/10_sigmoide_curve_frost_damage.R) fits sigmoid curves through the critical temperatures for 10% and 90% kill of flower buds (Ballard et al., 1981)[^9] to estimate frost damage depending on minimum temperature. 

[`11_stages_and_damage_future.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/11_stages_and_damage_future.R): this script uses the calibrated PhenoFlex model to predict the start of bloom, calculates earlier phenology stages based on heat accumulation, and estimates frost damage.

[`12_combine_frostdamage_files.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/12_combine_frostdamage_files.R) combines files produced by script [`11_stages_and_damage_future.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/11_stages_and_damage_future.R). This script is designed to run on a computer cluster. 

[`13_summarise_results.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/13_summarise_results.R) summarizes the results by calculating the number of cases with frost damage (any damage, damage >10% and damage >50%), giving the earliest, median and latest bloom date of the sample, as well as the minimal, median and maximum number of frost nights and the number of years with no blossom until end of June. The data is grouped by pathway (SSP1-2.6, SSP2-4.5, SSP3-7.0 and SSP5-8.5), model (optimistic, average, pessimistic), simulated year (2050, 2085), phenology station, repetition (1-100) and ripening period (early & late).

[`14_maps_interpolation_damage10.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/14_maps_interpolation_damage10.R) uses Kriging interpolation to produce raster data (cell size 0.05°) for the probability of frost damage >10% for Germany. The raster are used to plot the results in form of countywide maps. 

[`15_maps_interpolation_damage50.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/15_maps_interpolation_damage50.R) repeats he process of [`14_maps_interpolation_damage10.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/14_maps_interpolation_damage10.R) for frost damage >50%. 

[`16_hist_frost_frequency.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/16_hist_frost_frequency.R) applies the calibrated PhenoFlex model to the historical weather data, summarizes and interpolates the results to produce maps of the past frost frequency. 

[`17_hist_start_green_tip.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/17_hist_start_green_tip.R) applies the calibrated PhenoFlex model to the historic weather data to calculate the start of the green tip stage. We compare the start of green tip stage for early and late ripening varieties. 

[`18_data_analysis.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/18_data_analysis.R) contains several analyses needed for the manuscript. This includes counting the number of observations, plotting the shift of bloom data from past to future under different climate scenarios, a map with an overview about the station locations and some basic statistics. 

[`19_differences_calculation_and_maps.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/19_differences_calculation_and_maps.R) calculates and plots differences in frost damage probability between past and future time periods, early and late ripening varieties as well as optimistic, average and pessimistic model. 

The folder [util]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/tree/main/code/util) contains the script [`evaluation_function_gensa.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/util/evaluation_function_gensa.R) which is needed to execute script [`06_run_calibration_hpc.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/06_run_calibration_hpc.R). 

## Data
The [data]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/tree/main/data) folder contains all files which are crucial to run the code (as far as they are not publicly accessible elsewhere). Intermediate results which are needed for the following steps are not included. 

The files [`Apfel_Bluehdaten_fruehe_Reifezeit.csv`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/data/Apfel_Bluehdaten_fruehe_Reifezeit.csv) and [`Apfel_Bluehdaten_spaete_Reifezeit.csv`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/data/Apfel_Bluehdaten_spaete_Reifezeit.csv) contain the phenology observations used for the analysis. The raw phenology data used in script [`01_data_preparation.R`](https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/code/01_data_preparation.R) can be downloaded from the [DWD Climate data Center](https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/)

The files [`Stationen.txt `]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/data/Stationen.txt), [`DWD_weather_stations.csv `]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/data/DWD_weather_stations.csv), and [`all_pheno_stations.csv `]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/data/all_pheno_stations.csv) contain information about the locations of DWD phenology and weather stations. 

[`crit_temp_ballard.csv`]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/data/crit_temp_ballard.csv) contains critical temperatures for 10% and 90% flower bud kill in apple according to Ballard et al. (1981)[^9]. 

Elevation data used for interpolation is available on [OpenDEM](https://www.opendem.info/download_srtm.html). 

The shapefile for the borders of Germany used to reduce the interpolated raster to the shape of Germany is available from the [Bundesamt für Kartographie und Geodäsie](https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/verwaltungsgebiete-1-5-000-000-stand-01-01-vg5000-01-01.html).

## Results
The main results used to produce the maps are stored as `.RDS` files in the folder [map_preparation]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/tree/main/data/map_preparation) within the data folder. Files with “damage_50” in the file name are for frost damage at >50% of the buds and flowers. Files without this information refer to >10% damage.

Additionally, we provide a small subset of intermediate results here, which are useful to follow the modelling steps presented in the code. 

[`par_1993-2022-cluster.csv`]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/data/par_1993-2022-cluster.csv) contains the fitting result for ten repetitions of PhenoFlex fitting for apple bloom (early and late ripening varieties) in Germany. We used repetition one for all further analyses. 

[`frost_damage_summary_new.csv`]( https://github.com/ChristineSchmitz/supporting_information_frostrisk_maps/blob/main/data/frost_damage_summary_new.csv) contains the results of the frost damage prediction at station level. These results were used for interpolation. 

# References
[^1]: R Core Team, 2021. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.
[^2]: Luedeling, E., Caspersen, L., Fernandez, E., 2024. chillR: Statistical Methods for Phenology Analysis in Temperate Fruit Trees.
[^3]: Pebesma, E., 2004. Multivariable geostatistics in S: the gstat package. Computers & Geosciences 30, 683–691. https://doi.org/10.1016/j.cageo.2004.03.012
[^4]: Hijmans, R., 2023. terra: Spatial Data Analysis.
[^5]: Pebesma, E., Bivand, R., 2023. Spatial Data Science: With Applications in R, 1st ed. Chapman and Hall/CRC, New York. https://doi.org/10.1201/9780429459016
[^6]: Pya, N., 2023. scam: Shape Constrained Additive Models.
[^7]: Perpi, O., Hijmans, R., 2023. rasterVis.
[^8]: Kopton, J., 2024. STIF: Space-Time Interpolation and Forecasting. Zenodo. https://doi.org/10.5281/ZENODO.13271982
[^9]: Ballard, J.K., Proebsting, E.L., Tukey, R.B., 1981. Washington State University Extension Bulletin 0913 critical temperatures for blossom buds apples.

