# Green Edge project

This is the repository for the Green Edge project (http://www.greenedgeproject.info/). This include the code and the data used to describe the physical conditions during both missions of the Green Edge project (2015-2016).

## Green Edge

### 2015

- 2015-04-01 to 2015-07-15

### 2016

- Ship: 2016-06-03 to 2016-07-04
- Ice camp: 2016-05-01 to 2016-07-27

## Products to use

### MODIS

- ftp://ladsweb.nascom.nasa.gov/allData/6/MYD08_D3/
- MOD08_D3 vs MYD08_D3: which one to use?

#### Cloud optical thickness

- Using `Cloud_Optical_Thickness_Combined_Mean` because it is also used in the PP model (see Eric and Maxime).

#### Cloud fraction

- Using `Cloud_Fraction_Mean`.

### AVHRR

- SST (ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/2016/AVHRR/)

### AMSR2

- Sea ice concentration (ftp://ftp-projects.zmaw.de/seaice/AMSR2/)
- See email from Julien.

### Other

Eric is having a look to these variables:

- Wind speed?
  - Quickscat
  - CONCEPTS model (Eric)
  - http://www.ospo.noaa.gov/Products/atmosphere/wind.html
  - Seadas?
  - http://images.remss.com/cdr/climate_data_record_browse.html?product=wind
  - http://www.remss.com/measurements/ccmp

- Moist?
