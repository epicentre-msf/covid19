
# covid19

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

---

The goal of this dashboard is to provide an epidemiological overview of the COVID-19 pandemic, incorporating data from multiple sources. 

It is being actively developed by the data science team at [Epicentre MSF](https://epicentre.msf.org/en).

Future plans include more detailed analysis at sub-national levels and incorporation of data collected at MSF hospitals.

The tool is built using the R programming language and the shiny web framework. Packages used include the tidyverse, leaflet and highcharter.

## To Dos

---

- [x] Fix WHO data import and re-incorporate into dashboard
- [x] Add about page
- [x] Add epicentre + msf logos
- [ ] Add reactable for govt interventions
- [ ] Add interventions to time-series
- [ ] Add France sub-national analysis tab
- [ ] Add Iran sub-national analysis tab
- [ ] Add Afghanistan sub-national analysis tab


### Current data sources

---

#### European Centre for Disease Prevention and Control 

[Data on the geographic distribution of COVID-19 cases worldwide](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)

#### World Health Organisation

[WHO situation reports. Data extracted by Eric Brown](https://github.com/eebrown/data2019nCoV)

#### ACAPS COVID-19: Government Measures Dataset 

[Data available on HDX](https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset)

#### Shapefiles

[Natural Earth](https://www.naturalearthdata.com/)




