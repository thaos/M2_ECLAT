library(ncdf4)
library(ncdf4.helpers)
library(abind)

nc <- nc_open("tas_day_CNRM-CM6-1-HR_piControl_r1i1p1f2_gr_merged.nc")
tas_pi <- ncvar_get(nc, "tas")
time_pi <- nc.get.time.series(nc)
nc_close(nc)

nc <- nc_open("tas_day_CNRM-CM6-1-HR_historical_r1i1p1f2_gr_merged.nc")
tas_hist <- ncvar_get(nc, "tas")
time_hist <- nc.get.time.series(nc)
nc_close(nc)

saveRDS(data.frame(time = as.character(time_pi), tas = tas_pi), file = "tas_pi.rds")
saveRDS(data.frame(time = as.character(time_hist), tas = tas_hist), file = "tas_hist.rds")




