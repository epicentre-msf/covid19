
# COVID-19 Epi Dashboard

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of this dashboard is to provide an epidemiological overview of the COVID-19 pandemic, incorporating data from multiple sources. 

It is being actively developed by the data science team at [Epicentre MSF](https://epicentre.msf.org/en).

The tool is built using the R programming language and the shiny web framework. Packages used include the tidyverse, leaflet and highcharter.

### Setup Data

To set up data for the app, source [`data-raw/data.R`](data-raw/data.R) and [`data-raw/shapefiles.R`](data-raw/shapefiles.R).

### Development

The primary shiny code can be found in the [`R/mod_map.R`](R/mod_map.R) module.

### Run in dev mode

To run the application, source [`dev/run_dev.R`](dev/run_dev.R)

---

### Current data sources

#### European Centre for Disease Prevention and Control 

[Data on the geographic distribution of COVID-19 cases worldwide](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)

#### Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)

[Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE](https://github.com/CSSEGISandData/COVID-19)

#### ACAPS COVID-19: Government Measures Dataset 

[Data available on HDX](https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset)

#### Shapefiles

[Natural Earth](https://www.naturalearthdata.com/)




