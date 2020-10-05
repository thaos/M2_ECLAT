library(ncdf4)
library(ncdf4.helpers)
library(abind)

nc <- nc_open("tas_day_CNRM-CM6-1-HR_piControl_r1i1p1f2_gr_merged.nc")
tas <- ncvar_get(nc, "tas")
nc_close(nc)

nc <- nc_open("pr_day_CNRM-CM6-1-HR_piControl_r1i1p1f2_gr_merged.nc")
pr <- ncvar_get(nc, "pr")

# time <- ncvar_get(nc, "time")
time <- nc.get.time.series(nc)
yyyy <- format(time, "%Y")
mm <- format(time, "%m")
dd <- format(time, "%d")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
nc_close(nc)

dimnames(tas) <- dimnames(pr) <- list(lon = lon, lat = lat, time = format(time, "%Y%m%d"))


extract_region <- function(dat, ilonmin, ilonmax, ilatmin, ilatmax){
  dat[ilonmin:ilonmax, ilatmin:ilatmax, , drop = FALSE]
}

ilonlat_df <- data.frame(
  ilonmin = c(6, 3, 1),
  ilonmax = c(6, 9, 11),
  ilatmin = c(6, 4, 2),
  ilatmax = c(6, 8, 10)
)
rownames(ilonlat_df) <- c("1x1", "7x5", "11x9")

tas_list <- with(
  ilonlat_df,
  mapply(extract_region, ilonmin = ilonmin, ilonmax = ilonmax, ilatmin = ilatmin, ilatmax = ilatmax, MoreArgs = list(dat = tas)
  )
)

spatial_aggregator <- function(dat, fun, ...){
  apply(dat, 3, fun, ...)
}

tas_spmean <- as.data.frame(lapply(tas_list, function(dat) spatial_aggregator(dat, mean)))
tas_spmax <- as.data.frame(lapply(tas_list, function(dat) spatial_aggregator(dat, max)))
names(tas_spmean) <- names(tas_spmax) <- c("1x1", "7x5", "11x9")

matplot(tas_spmean)
matplot(tas_spmax)

pairs(tas_spmean)
pairs(tas_spmax)

summary(tas_spmean)
summary(tas_spmax)

apply(tas_spmean, 2, sd)
apply(tas_spmax, 2, sd)

breaks <- hist(unlist(tas_spmean))$breaks
par(mfrow = c(3, 1))
hist(tas_spmean[[1]], breaks = breaks)
hist(tas_spmean[[2]], breaks = breaks)
hist(tas_spmean[[3]], breaks = breaks)

breaks <- hist(unlist(tas_spmax))$breaks
par(mfrow = c(3, 1))
hist(tas_spmax[[1]], breaks = breaks)
hist(tas_spmax[[2]], breaks = breaks)
hist(tas_spmax[[3]], breaks = breaks)

l_subyear <- list(
  iday = (mm == "07" & dd == "15"),
  imonth = (mm == "07"),
  iyear = rep(TRUE, length(mm))
)

temporal_aggregator <- function(dat, factor, fun){
  apply(dat, 2, function(x) tapply(x, factor, fun))
}

tas_spmean_tmean <- lapply(
  l_subyear,
  function(i){
    temporal_aggregator(tas_spmean[i, ], yyyy[i], mean)
  }
)
tas_spmean_tmax <- lapply(
  l_subyear,
  function(i){
    temporal_aggregator(tas_spmean[i, ], yyyy[i], max)
  }
)

tas_spmax_tmean <- lapply(
  l_subyear,
  function(i){
    temporal_aggregator(tas_spmax[i, ], yyyy[i], mean)
  }
)
tas_spmax_tmax <- lapply(
  l_subyear,
  function(i){
    temporal_aggregator(tas_spmax[i, ], yyyy[i], max)
  }
)

prepare_dat <- function(dat, time, ilonlat_df, l_subyear, l_spagg, l_tagg){
  yyyy <- format(time, "%Y")
  mm <- format(time, "%m")
  dd <- format(time, "%d")
  dat_list <- with(
    ilonlat_df,
    mapply(extract_region, ilonmin = ilonmin, ilonmax = ilonmax, ilatmin = ilatmin, ilatmax = ilatmax, MoreArgs = list(dat = dat)
    )
  )
  dat_spagg <- lapply(
    l_spagg,
    function(spagg){
      ans <- as.data.frame(lapply(dat_list, function(dat) spatial_aggregator(dat, spagg)))
      names(ans) <- rownames(ilonlat_df)
      return(ans)
    }
  )
  names(dat_spagg) <- names(l_spagg)
  dat_tagg <- lapply(
    dat_spagg,
    function(dat){
      ans <- lapply(
        l_tagg,
        function(tagg){
          ans <- lapply(
            l_subyear,
            function(i){
              temporal_aggregator(dat[i, ], yyyy[i], tagg)
            }
          )
          ans <- do.call(abind, c(ans, along = 3))
          dimnames(ans)[[3]] <- names(l_subyear)
          return(ans)
        }
      )
      names(ans) <- names(l_tagg)
      return(ans)
    }
  )
  list(spagg = dat_spagg, stagg = dat_tagg)
}
l_spagg <- l_tagg <- c("mean" = mean, "max" = max)
tas_agg <- prepare_dat(tas, time, ilonlat_df, l_subyear, l_spagg, l_tagg)
pr_agg <- prepare_dat(pr, time, ilonlat_df, l_subyear, l_spagg, l_tagg)
tas_spagg <- tas_agg$spagg
tas_stagg <- tas_agg$stagg
pr_spagg <- pr_agg$spagg
pr_stagg <- pr_agg$stagg

breaks <- hist(c(unlist(tas_stagg[[1]][[1]][,3,])), breaks = 30)$breaks
par(mfrow = c(3, 1))
hist(tas_stagg[[1]][[1]][,3,1], breaks = breaks)
hist(tas_stagg[[1]][[1]][,3,2], breaks = breaks)
hist(tas_stagg[[1]][[1]][,3,3], breaks = breaks)

breaks <- hist(c(unlist(tas_spagg[[2]])), breaks = 30)$breaks
par(mfrow = c(3, 1))
hist(tas_spagg[[2]][,1], breaks = breaks)
hist(tas_spagg[[2]][,2], breaks = breaks)
hist(tas_spagg[[2]][,3], breaks = breaks)

breaks <- hist(c(unlist(pr_stagg[[2]][[2]][,3,])), breaks = 30)$breaks
par(mfrow = c(3, 1))
hist(pr_stagg[[2]][[2]][,3,1], breaks = breaks)
hist(pr_stagg[[2]][[2]][,3,2], breaks = breaks)
hist(pr_stagg[[2]][[2]][,3,3], breaks = breaks)

breaks <- hist(c(unlist(pr_spagg[[2]])), breaks = 30)$breaks
par(mfrow = c(3, 1))
hist(pr_spagg[[2]][,1], breaks = breaks)
hist(pr_spagg[[2]][,2], breaks = breaks)
hist(pr_spagg[[2]][,3], breaks = breaks)

saveRDS(tas_stagg, file =  "tas_stagg.rds")
saveRDS(tas, file =  "tas.rds")
saveRDS(pr_stagg, file =  "pr_stagg.rds")
saveRDS(pr, file =  "pr.rds")