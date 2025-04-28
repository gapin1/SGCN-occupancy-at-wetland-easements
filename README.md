# SGCN-occupancy-at-wetland-easements
Data are available to download at: https://www.sciencebase.gov/catalog/item/67f95964d4be022c3e84f405

01_sp_accumulation_curve.R: Species accumulation curve analysis and visualization for all species and broken down
	by guild. Excludes likely migratory species.
	INPUT FILES:
	1. 07_09_all.csv: data formatted for species accumulation curve from 07-09 for all species.
	2. 22_23_all.csv: data formatted for species accumulation curve from 22-23 for all species.
	3. 07_09_forest.csv: data formatted for species accumulation curve from 07-09 for forest species.
	4. 22_23_forest.csv: data formatted for species accumulation curve from 22-23 for forest species.
	5. 07_09_grassland.csv: data formatted for species accumulation curve from 07-09 for grassland species.
	6. 22_23_grassland.csv: data formatted for species accumulation curve from 22-23 for grassland species.
	7. 07_09_openwood.csv: data formatted for species accumulation curve from 07-09 for open woodland species.
	8. 22_23_openwood.csv: data formatted for species accumulation curve from 22-23 for open woodland species.
	9. 07_09_shrub.csv: data formatted for species accumulation curve from 07-09 for shrubland species.
	10. 22_23_shrub.csv: data formatted for species accumulation curve from 22-23 for shrubland species.
	11. 07_09_wetland.csv: data formatted for species accumulation curve from 07-09 for wetland species.
	12. 22_23_wetland.csv: data formatted for species accumulation curve from 22-23 for wetland species.

02_occupancy.R: Single-species occupancy analyses for Iowa species of greatest conservation need (SGCN)
	INPUT FILES:
	1. bird_data.csv: presence/absence data for Iowa SGCN at wetland easement properties in the Prairie Pothole
	   Region of Iowa. Includes a maximum of 6 visits, but most properties were only visited ~3 times.
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
