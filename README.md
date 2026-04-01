# sounds-of-nature-shiny
*Interactive bioacoustics map for Sounds of Nature MN project*
<br> </br>

*Sounds of Nature MN* is an Environment and Natural Resources Trust Fund (ENRTF) sponsored study that leverages citizen science, passive acoustic monitoring, and AI neural networks to examine avian biodiversity on both public and private lands across Minnesota's three major biomes (Laurentian Mixed Forest, Eastern Broadleaf Forest, and Prairie Grasslands). Autonomous Recording Units (ARUs) were programmed to record around dusk and dawn from May 15th to June 30th, and deployed on the private lands of volunteer study participants and on a variety of nearby public lands, including Scientific Natural Areas (SNAs) and Wildlife Management Areas (WMAs).
	Over 30,000 hours of audio was recorded across 124 sites in 2025 alone. All of the recording data was run through BirdNET<sup>1</sup>, an AI neural network capable of rapidly identifying thousands of species through audio. To account for false positives produced by this tool, an extensive data validation process was performed for every species detected in our study, following the protocol laid out by Symes et al. (2024)<sup>2</sup>. 
<br> </br>

1. Kahl, S., Wood, C. M., Eibl, M., & Klinck, H. (2021). BirdNET: A deep learning solution for avian diversity monitoring. Ecological Informatics, 61, 101236. 
2. Symes L, Sugai LSMS, Gottesman B, Pitzrick M, Wood C, Charif, R. 2024. Acoustic analysis with BirdNET and (almost) no coding: practical instructions. 
<br> </br> 

## The Sounds of Nature Minnesota Avian Biodiversity Visualizer 

The Sounds of Nature Minnesota results can be accessed through an interactive dashboard built using Shiny and R. This visualization follows a multi-tab layout, where users can view avian acoustic detections by site or by species. In the Search by Site tab, users scroll on a map or enter in their site number and click on a pin representing their desired site. Site-specific statistics, an interactive avian family bar graph, and a data table containing species detections are populated below. In the Search by Species tab, users can select their desired species and use the map to explore where this bird was detected. A bar graph showing biome types of those sites containing the selected species and a data table containing the detection sites and detection probabilities are populated below.
<br> </br> 

*Data processing scripts*
* birdnet_analysis_thresholds.py: script analyzing which species were detected at each site and the number of detections of those species 

* confidence_thresholds.Rmd: modified R code taken from Cornell’s BirdNET/Raven guide calculating threshold values for each species (output is threshold_table.csv)

* rare_species.py: script compiling a list of all positive manual validations from “rare” species that do not have threshold values (output is validated_rare_species.csv)
<br> </br>

*Files required for Shiny app to run*
* aru_coords_2025.csv: CSV file containing coordinates and other site info for all public and private sites

* avian_biodiversity_shiny.R: R script containing code to build the Sounds of Nature Shiny app

* birdfamilies_updated.csv: CSV file containing up-to-date species names, common names, and links to all target birds (Cornell All About Birds)

* private_sites_results: folder containing one CSV file per private site that contains processed species detection data (output of birdnet_analysis_thresholds.py)

* public_sites_results: folder containing one CSV file per public site that contains processed species detection data (output of birdnet_analysis_thresholds.py)

* www: folder containing CSS stylesheet (biodiversity_styles.css) and footer logos used by the Sounds of Nature Shiny app
