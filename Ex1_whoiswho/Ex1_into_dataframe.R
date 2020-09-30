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

yyyy <- substring(gsub("-", "", dates), 1, 4)
compute_yearmean <- function(tas, yyyy){
  tapply(tas, yyyy, mean, na.rm = TRUE)
}
yearmean <-
  apply(
    tas_df,
    2,
    compute_yearmean,
    yyyy = yyyy
  )

mmdd <- substring(gsub("-", "", dates), 5, 8)
compute_seasonality <- function(tas, mmdd){
  tapply(tas, mmdd, mean, na.rm = TRUE)
}
seasonality <-
  apply(
    tas_df,
    2,
    compute_seasonality,
    mmdd = mmdd
  )
anomalies <- matrix(
  as.vector(t(tas_df)) - as.vector(t(seasonality)),
  nrow = nrow(tas_df),
  ncol = ncol(tas_df),
  byrow = TRUE
)
dimnames(anomalies) <- dimnames(tas_df)

ylim <- range(tas_df, na.rm = TRUE)
par(mfrow = c(4, 3))
apply(tas_df, 2, plot, type = "l", ylim = ylim)
ylim <- range(anomalies, na.rm = TRUE)
par(mfrow = c(4, 3))
apply(anomalies, 2, plot, type = "l", ylim = ylim)
par(mfrow = c(4, 3))
apply(anomalies, 2, pacf, na.action = na.pass)
pacf_ano <- apply(anomalies, 2, function(x) pacf(x, na.action = na.pass, plot = FALSE)$acf)
par(mfrow = c(4, 3))
apply(anomalies, 2, acf, na.action = na.pass)
summary(anomalies)
sort(apply(anomalies, 2, sd, na.rm = TRUE))
sort(apply(anomalies, 2, mean, na.rm = TRUE))
trends <- apply(anomalies, 2, function(y){
  x <- cbind(1, seq_along(y))
  ina <- is.na(y)
  lm.fit(x = x[!ina,, drop = FALSE], y = y[!ina])$coefficients[2]
})

ylim <- range(seasonality, na.rm = TRUE)
par(mfrow = c(4, 3))
apply(seasonality, 2, plot, type = "l", ylim = ylim)
summary(seasonality)

ylim <- range(yearmean, na.rm = TRUE)
par(mfrow = c(4, 3))
apply(yearmean, 2, plot, type = "l", ylim = ylim)
summary(yearmean)

xlim <- range(anomalies, na.rm = TRUE)
par(mfrow = c(4, 3))
apply(anomalies, 2, hist, xlim = xlim)

d <- dist(t(anomalies)) # euclidean distances between the columns
d <- dist(t(tas_df)) # euclidean distances between the columns
d <- dist(t(pacf_ano)) # euclidean distances between the columns
d <- dist(t(seasonality)) # euclidean distances between the columns
d <- dist(trends) # euclidean distances between the columns

fit <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
fit # view results
# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
par(mfrow = c(1, 1))
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
  main="Metric MDS", type="n")
text(x, y, labels = colnames(anomalies), cex=.6)

