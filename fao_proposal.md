# Proposal: Mapping Global Marine Aquaculture

### Summary
To our knowledge, no global map of farm-scale marine aquaculture currently exists. The University of California, Santa Barbara's (UCSB) Sustainable Fisheries Group (SFG) believes that, by partnering with Google and the Food and Agriculture Organization of the United Nations (FAO), we can produce such a map. By leveraging Google’s cutting edge spatial analysis and machine learning techniques, together with FAO's unique and detailed country-level expertise, SFG is confident that we can design an accurate and reproducible methodology for mapping global marine aquaculture at the farm level. Producing a global map of marine aquaculture would be a major achievement that could dramatically improve the ability for researchers and managers to study and steward this important use of marine space. Furthermore, a global map of marine aquaculture could be integrated into Google Earth Engine and the Global Fishing Watch platform to identify and highlight interactions between aquaculture, wild fisheries, and the environment at an unprecedented scale.

### Motivation
Marine aquaculture, known as mariculture, is an increasingly important and efficient healthy food source. Mariculture is growing at an average annual rate of 8% and a recent report by the FAO emphasized the large global potential for mariculture expansion. However, we still lack basic spatial knowledge about where mariculture production currently occurs.

Fortunately, there is growing interest in improving the spatial resolution of mariculture statistics, both for the purposes of monitoring the activity and for validating existing databases on aquaculture production, such as the FAO's. The FAO's National Aquaculture Sector Overview (NASO) initiative is working *"...to illustrate, in general, where aquaculture is taking place"* and is compiling maps of aquaculture production at an administrative, and some times farm, level. As part of the Sea Around Us Project, Campbell and Pauly (2012) constructed a Global Mariculture Database (GMD) that, where possible, spatialize aquaculture production to sub-national levels. This analysis concluded that the FAO's global aquaculture production database is reasonably accurate. Similarly, Trujillo, Piroddia, and Jacquet (2012) used Google Earth to ground truth tuna aquaculture statistics in the Mediterranean, with the authors also confirming the accuracy of recent FAO statistics. However, to our knowledge, no global map of farm-scale marine aquaculture currently exists. This information is fundamental to improving our understanding of mariculture's interactions with wild fisheries and the environment, and critical for our ability to understand where mariculture should be expanded and where it should be restrained.

With the advent of remote sensing, it is now possible to map Earth's ecosystems at a global scale and at a high (< 30 meter) resolution. As a result, there are now numerous initiatives, such as Global Forest Watch, designed to assist with monitoring and managing these systems. Furthermore, machine learning techniques are increasingly being applied to spatial analyses and offer the possibility of automating the detection and monitoring of different uses of terrestrial and marine space. Used together, these techniques offer the solution to mapping global marine aquaculture at the farm level. Lastly, FAO's in-depth knowledge of aquaculture activities at the country level, including the recent National Aquaculture Sector Overview (NASO) initiative, can provide an effective method for validating results. Working closely with the FAO would also allow Google and SFG to develop additional data products and analyses that support FAO's ongoing aquaculture work.

### Proposed Approach
Our proposed approach to this project involves a combination of aquaculture suitability modeling, crowdsourced image analysis, verification of identified farm sites, and machine learning. The overall goal of this approach is to use Google Earth Engine (GEE) to compile a data set and shapefile of verified aquaculture farms. Ideally, this data set would then be used to train machine learning algorithms to detect new or overlooked farms in satellite images. If successful, this algorithm could be used to monitor the expansion, estimate productivity, and observe the environmental conditions of aquaculture farms at the global level moving forward.

#### Project Steps:

1.	SFG uses GEE to conduct a preliminary analysis to identify areas that are potentially suitable for aquaculture, narrowing down the area over which to manually search for existing farms

2. SFG oversees a crowdsourcing campaign designed to identify existing farms within the area suitable for aquaculture as determined in step 1

3. Identified farms are cross-referenced with data on farm location provided by FAO and other management bodies

4. Where possible, data on farm location is supplemented with information on species, farm type (e.g. floating pen, cage, mussel line, etc), productivity, and other useful variables

5. Input data set of all identified farms as a “training” data set for machine learning algorithms to assist with future identification (and possibly classification) of aquaculture globally

6. Write up method and results for submission to a peer-reviewed journal

#### Data Sources:
+ Google Earth Engine satellite image collections
+ FAO NASO Map Library
+ Unpublished FAO records on farm location(?)
+ Publicly available national records of farm locations (if available)

### Anticipated Outcomes
The primary deliverables of this project will be a global shapefile (and/or raster) of marine aquaculture farms and an academic publication describing the methodology, results, and implications. The data product could be made publicly available as a data layer in GEE and, if efforts to develop a machine learning algorithm are successful, the aquaculture data layer could by automatically updated each time GEE receives new satellite data.

### References
Campbell, B., & Pauly, D. (2013). Mariculture: a global analysis of production trends since 1950. Marine Policy, 39, 94-100.

Trujillo P, Piroddi C, Jacquet J (2012) Fish Farms at Sea: The Ground Truth from Google Earth. PLoS ONE 7(2): e30546. doi:10.1371/journal.pone.0030546
