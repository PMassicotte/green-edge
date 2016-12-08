# ftp://ftp.eri.ucsb.edu/pub/org/oceancolor/MEaSUREs2014/GSM2km/binned/DAY/2016/

# ftp://ftp.eri.ucsb.edu/pub/org/oceancolor/MEaSUREs2014/GSM2km/binned/DAY/2016/

gsm <- ncdf4::nc_open("/home/pmassicotte/Downloads/gsm2016001.L3b_DAY_ca2km.AV.2.nc")

gsm2 <- ncdf4::ncvar_get(gsm, "chl")

gsm <- raster("/home/pmassicotte/Downloads/gsm2016001.L3b_DAY_ca2km.AV.2.nc", 
                    varname = "bins")
