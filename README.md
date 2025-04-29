# SGCN-occupancy-at-wetland-easements
Data are available to download at: https://www.sciencebase.gov/catalog/item/67f95964d4be022c3e84f405
DOI for code: https://doi.org/10.5281/zenodo.15299898

01_sp_accumulation_curve.R: Species accumulation curve analysis and visualization for all species and broken down
	by guild. Excludes likely migratory species.
	INPUT FILES:
	1. bird_data.csv: presence/absence data for all species detected at wetland easement properties in the Prairie
	   Pothole Region of Iowa, excluding likely migrant species. Includes a maximum of 6 visits, but most
	   properties were only visited ~3 times. Also includes guild information for use in species accumulation
	   curves.

02_occupancy.R: Single-species occupancy analyses for Iowa species of greatest conservation need (SGCN)
	INPUT FILES:
	1. bird_data.csv: presence/absence data for all species detected at wetland easement properties in the Prairie
	   Pothole Region of Iowa, excluding likely migrant species. Includes a maximum of 6 visits, but most
	   properties were only visited ~3 times.
	2. covs_combined.csv: covariate data corresponding with bird survey locations, including year, time period,
	   ordinal date, observer, percent woody plant cover, easement age, cluster ID, and number of points
	   surveyed.
	OUTPUT FILES:
	1. 60 RDS files containing model and output (3 files for each of the 20 species)
	2. model_selection.csv: file containing model selection criteria

03_occupancy_visualization.R: Plot estimates from single-species occupancy models. Also includes code to extract
	alpha and beta values from models that we included in a table in supplemental materials.
	INPUT FILES:
	1. 20 RDS files containing top models for each species based on model selection criteria.
	2. covs_combined.csv: covariate data corresponding with bird survey locations, including year, time period,
	   ordinal date, observer, percent woody plant cover, easement age, cluster ID, and number of points
	   surveyed.
