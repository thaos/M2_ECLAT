library(ncdf4)

orly <- read.csv("FRM00007149.csv", stringsAsFactors = FALSE)
dates <- as.numeric(gsub("-", "", orly$DATE))
idates <- (dates >= 19790101) & (dates <= 20051231) & ((dates %% 10000) != 0229)
orly <- orly[idates, ]
dates <- dates[idates]
orly$TAVG <- orly$TAVG / 10

# lebourget <- read.csv("FR000007150.csv", stringsAsFactors = FALSE)
# dates <- as.numeric(gsub("-", "", lebourget$DATE))
# idates <- (dates >= 19790101) & (dates <= 20051231) & ((dates %% 10000) != 0229)
# lebourget <- lebourget[idates, ]
# lebourget$TAVG <- lebourget$TAVG / 10

listnc <- list.files(pattern = ".*\\.nc")
tasname <- c(rep("tas", 4), "t2m", rep("tas", 2), "air", rep("tg", 2))

tas_df <- mapply(
  function(file, name){
    nc <- nc_open(file)
    tas <- ncvar_get(nc, name)
    if(name != "tg") tas <- tas - 273.15
    nc_close(nc)
    return(tas)
  },
  file = listnc, name = tasname,
  SIMPLIFY = FALSE
)
tas_df <- do.call(cbind, tas_df)
tas_df <- cbind(tas_GHCND_orly = orly$TAVG, tas_df)
colnames(tas_df) <- c(
  "GHCND_orly",
  "CNRM-CM5_historical_r1i1p1",
  "CNRM-CM5_historical_r2i1p1",
  "IPSL-CM5A-LR_historical_r1i1p1",
  "IPSL-CM5A-LR_historical_r2i1p1",
  "ERA5",
  "CORDEX_EUR-44_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CNRM-ALADIN53_v1",
  "CORDEX_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CNRM-ALADIN53_v1",
  "NCEP",
  "EOBS_0.1deg",
  "EOBS_0.25deg"
)
tas_df <- tas_df[, sample.int(ncol(tas_df))]
rownames(tas_df) <- dates
modelnames <- colnames(tas_df)
saveRDS(tas_df, "Ex1_tas_df.rds")
saveRDS(modelnames, "Ex1_modelnames.rds")
colnames(tas_df) <- 1:ncol(tas_df)
saveRDS(tas_df, "Ex1_tas_df_wonames.rds")

