# Sinanodonta
A habitat model for the Chinese Pond Mussel (_Sinanodonta woodiana_; CPM) in North America. 

### Summary

The purpose of this repository is to make available the modeling code, public environmental data (climate, land cover, etc.), and theoretical justifications for a habitat modeling effort of the Chinese Pond Mussel (_Sinanodonta woodiana_) in North America. CPM is an established invasive species in Europe and was first discovered in North America in a New Jersey aquaculture pond in 2010 (Bogan et al., ). The habitat suitability model and resulting map will inform potential establishment risk and guide early detection and rapid response (EDDR) efforts to stem the invasion. The model was developed as part of a management and EDDR plan for the species in North America prepared by the Chinese Pond Mussel Management Plan Working Group.

### Modeling approach

We are employing a machine learning approach to predicting habitat suitability for CPM in North America. Following methods of Mehler et al. (2024), we will: 
1. create a model of habitat suitability for CPM in Europe based on spatial environmental covariate data (climate, land cover, etc.), fitting the model using MaxEnt.
2. collect and format suitabilty compatible saptial environmental covariate data for the Delaware River Watershed (and perhaps throughout the continental US as data permits)
3. fit the European model to the North American environmental data to yield a predicted map of habitat suitability 

### Environmental data sources for fitting

[Wordclim bioclimatic variables](https://www.worldclim.org/data/bioclim.html)
[Corine Land Cover (Europe)](https://land.copernicus.eu/en/products/corine-land-cover)
[Surface Geologic Units Lithology (1:1M, via European Geographical Data Infrastructure)](https://egdi.geology.cz/record/basic/5729ffdf-2558-48fc-a5d2-645a0a010855); WFS link: http://mapsrefdev.brgm.fr/wxs/1GE/EGDI_1M_INSPIRE_geolUnits
[EU-Hydro River Network Database 2006-2012](https://doi.org/10.2909/393359a7-7ebd-4a52-80ac-1a18d5f3db9c)
[Port locations (Eurostat)](https://ec.europa.eu/eurostat/web/gisco/geodata/transport-networks)
Elevation, derived from DEM
TWI	Topographic Wetness Index, derived from DEM
SPI	Stream Power Index, derived from DEM

### Environmental data sources for prediction

[Wordclim bioclimatic variables](https://www.worldclim.org/data/bioclim.html)
[National Land Cover Database (USGS, 2021)](https://www.usgs.gov/centers/eros/science/national-land-cover-database)
[National Hydrography Dataset (USGS)](https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/State/GPKG/)
[Port locations (USGS)](https://www.sciencebase.gov/catalog/item/5947f4a6e4b062508e34429b)
Elevation, derived from DEM
TWI	Topographic Wetness Index, derived from DEM
SPI	Stream Power Index, derived from DEM
