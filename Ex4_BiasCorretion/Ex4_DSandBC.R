tas_df <- readRDS("Ex1_tas_df.rds")

# Downscaling
X <- tas_df[, "tas_era5_paris_7905.nc"]
Y <- tas_df[, "tas_GHCND_orly"]
ina <- is.na(X) | is.na(Y)
X <- X[!ina]
Y <- Y[!ina]
dates <- as.numeric(rownames(tas_df))
dates <- dates[!ina]



iapp <- dates < 20000101
X_app <- X[iapp]
Y_app <- Y[iapp]
X_test <- X[!iapp]
Y_test <- Y[!iapp]

xylim <- range(X, Y, na.rm = TRUE)
plot(X_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_app - X_app)^2))
cor(X_app, Y_app)
plot(Y_app - X_app, type = "l")

plot(X_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_test - X_test)^2))
cor(X_test, Y_test)
plot(Y_test - X_test, type = "l")

qqplot(X_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
qqplot(X_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

base_app <- data.frame(X = X_app, Y = Y_app)
base_test <- data.frame(X = X_test, Y = Y_test)

# Perfect Prog
# Linear Model
lm_fit <- lm(Y ~ X, data = base_app)
#lm_fit <- lm(Y ~ X - 1, data = base_app)
summary(lm_fit)

lm_app <- predict(lm_fit)

plot(lm_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_app - lm_app)^2))
cor(lm_app, Y_app)
plot(Y_app - lm_app, type = "l")

qqplot(lm_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

lm_test<- predict(lm_fit, newdata = base_test)
plot(lm_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_test - lm_test)^2))
cor(lm_test, Y_test)
plot(Y_test - lm_test, type = "l")

qqplot(lm_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")


# analogues / nearest-neighbour
nn_app <- numeric(length(Y_app))
for( i in seq_along(nn_app)){
  dist <- (X_app - X_app[i])^2
  inn <- which.min(dist)
  nn_app[i] <- Y_app[inn]
}

nn_test <- numeric(length(Y_test))
for( i in seq_along(nn_test)){
  dist <- (X_app - X_test[i])^2
  inn <- which.min(dist)
  nn_test[i] <- Y_app[inn]
}

plot(nn_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_app - nn_app)^2))
cor(nn_app, Y_app)
plot(Y_app - nn_app, type = "l")

qqplot(nn_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

plot(nn_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_test - nn_test)^2))
cor(nn_test, Y_test)
plot(Y_test - nn_test, type = "l")

qqplot(nn_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

# Model Output Statistic
# simple mean correction
mc_app <- X_app + mean(Y_app) - mean(X_app)
mc_test <- X_test + mean(Y_app) - mean(X_app)
plot(mc_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_app - mc_app)^2))
cor(mc_app, Y_app)
plot(Y_app - mc_app, type = "l")

qqplot(mc_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

plot(mc_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_test - mc_test)^2))
cor(mc_test, Y_test)
plot(Y_test - mc_test, type = "l")

qqplot(mc_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

# quantile-quantile correction
qq_app <- quantile(Y_app, probs = ecdf(X_app)(X_app))
qq_test <- quantile(Y_app, probs = ecdf(X_app)(X_test))

plot(qq_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_app - qq_app)^2))
cor(qq_app, Y_app)
plot(Y_app - qq_app, type = "l")

qqplot(qq_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

plot(qq_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_test - qq_test)^2))
cor(qq_test, Y_test)
plot(Y_test - qq_test, type = "l")

qqplot(qq_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

# Summary plots
# Apprentissage
# Point by point
plot(X_app, Y_app, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5)
points(lm_app, Y_app, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 2)
points(nn_app, Y_app, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 3)
points(mc_app, Y_app, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 4)
points(qq_app, Y_app, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 5)
abline(b = 1, a = 0, col = "red")
legend(
  "topleft",
  legend = c("X", "lm", "nn", "mc", "qq"),
  col = 1:5,
  pch = 20
)

# distribution
qqplot_lm <- qqplot(lm_app, Y_app, plot.it = FALSE)
qqplot_nn <- qqplot(nn_app, Y_app, plot.it = FALSE)
qqplot_mc <- qqplot(mc_app, Y_app, plot.it = FALSE)
qqplot_qq <- qqplot(qq_app, Y_app, plot.it = FALSE)
qqplot(X_app, Y_app, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5)
points(qqplot_lm$x, qqplot_lm$y, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 2)
points(qqplot_nn$x, qqplot_nn$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 3)
points(qqplot_mc$x, qqplot_mc$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 4)
points(qqplot_qq$x, qqplot_qq$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 5)
abline(b = 1, a = 0, col = "red")
legend(
  "topleft",
  legend = c("X", "lm", "nn", "mc", "qq"),
  col = 1:5,
  pch = 20
)
ks.test(X_app, Y_app)
ks.test(lm_app, Y_app)
ks.test(nn_app, Y_app)
ks.test(mc_app, Y_app)
ks.test(qq_app, Y_app)

# Test
# Point by point
plot(X_test, Y_test, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5)
points(lm_test, Y_test, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 2)
points(nn_test, Y_test, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 3)
points(mc_test, Y_test, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 4)
points(qq_test, Y_test, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 5)
abline(b = 1, a = 0, col = "red")
legend(
  "topleft",
  legend = c("X", "lm", "nn", "mc", "qq"),
  col = 1:5,
  pch = 20
)

# distribution
qqplot_lm <- qqplot(lm_test, Y_test, plot.it = FALSE)
qqplot_nn <- qqplot(nn_test, Y_test, plot.it = FALSE)
qqplot_mc <- qqplot(mc_test, Y_test, plot.it = FALSE)
qqplot_qq <- qqplot(qq_test, Y_test, plot.it = FALSE)
qqplot(X_test, Y_test, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5)
points(qqplot_lm$x, qqplot_lm$y, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 2)
points(qqplot_nn$x, qqplot_nn$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 3)
points(qqplot_mc$x, qqplot_mc$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 4)
points(qqplot_qq$x, qqplot_qq$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 5)
abline(b = 1, a = 0, col = "red")
legend(
  "topleft",
  legend = c("X", "lm", "nn", "mc", "qq"),
  col = 1:5,
  pch = 20
)
ks.test(X_test, Y_test)
ks.test(lm_test, Y_test)
ks.test(nn_test, Y_test)
ks.test(mc_test, Y_test)
ks.test(qq_test, Y_test)

# Bias Corretion: only MOS
X <- tas_df[, "tas_day_IPSL-CM5A-LR_historical_r1i1p1_7905.nc"]
Y <- tas_df[, "tas_GHCND_orly"]
ina <- is.na(X) | is.na(Y)
X <- X[!ina]
Y <- Y[!ina]
dates <- as.numeric(rownames(tas_df))
dates <- dates[!ina]



iapp <- dates < 20000101
X_app <- X[iapp]
Y_app <- Y[iapp]
X_test <- X[!iapp]
Y_test <- Y[!iapp]

xylim <- range(X, Y, na.rm = TRUE)
plot(X_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_app - X_app)^2))
cor(X_app, Y_app)
plot(Y_app - X_app, type = "l")
plot(X_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
sqrt(mean((Y_test - X_test)^2))
cor(X_test, Y_test)
plot(Y_test - X_test, type = "l")

qqplot(X_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")
qqplot(X_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

# Model Output Statistic
# simple mean correction
mc_app <- X_app + mean(Y_app) - mean(X_app)
mc_test <- X_test + mean(Y_app) - mean(X_app)

qqplot(mc_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

qqplot(mc_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

# quantile-quantile correction
qq_app <- quantile(Y_app, probs = ecdf(X_app)(X_app))
qq_test <- quantile(Y_app, probs = ecdf(X_app)(X_test))

qqplot(qq_app, Y_app, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

qqplot(qq_test, Y_test, xlim = xylim, ylim = xylim)
abline(b = 1, a = 0, col = "red")

# Summary plots
# Apprentissage
# distribution
qqplot_mc <- qqplot(mc_app, Y_app, plot.it = FALSE)
qqplot_qq <- qqplot(qq_app, Y_app, plot.it = FALSE)
qqplot(X_app, Y_app, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5)
points(qqplot_mc$x, qqplot_mc$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 4)
points(qqplot_qq$x, qqplot_qq$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 5)
abline(b = 1, a = 0, col = "red")
legend(
  "topleft",
  legend = c("X", "mc", "qq"),
  col = c(1, 4, 5),
  pch = 20
)
ks.test(X_app, Y_app)
ks.test(mc_app, Y_app)
ks.test(qq_app, Y_app)

# Test
# distribution
qqplot_mc <- qqplot(mc_test, Y_test, plot.it = FALSE)
qqplot_qq <- qqplot(qq_test, Y_test, plot.it = FALSE)
qqplot(X_test, Y_test, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5)
points(qqplot_mc$x, qqplot_mc$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 4)
points(qqplot_qq$x, qqplot_qq$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 5)
abline(b = 1, a = 0, col = "red")
legend(
  "topleft",
  legend = c("X", "lm", "nn", "mc", "qq"),
  col = 1:5,
  pch = 20
)
ks.test(X_test, Y_test)
ks.test(mc_test, Y_test)
ks.test(qq_test, Y_test)

# Applying downscaling to climate model simulations
# Perfect Prognosis
base_gcm <- data.frame(X = X_test, Y = Y_test)
lm_gcm <- predict(lm_fit, newdata = base_gcm)
nn_gcm <- numeric(length(Y_test))
for( i in seq_along(nn_test)){
  dist <- (base_app$X - X_test[i])^2
  inn <- which.min(dist)
  nn_gcm[i] <- base_app$Y[inn]
}
# Test
# distribution
qqplot_lm <- qqplot(lm_gcm, Y_test, plot.it = FALSE)
qqplot_nn <- qqplot(nn_gcm, Y_test, plot.it = FALSE)
qqplot(X_test, Y_test, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5)
points(qqplot_mc$x, qqplot_mc$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 4)
points(qqplot_qq$x, qqplot_qq$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 5)
points(qqplot_lm$x, qqplot_lm$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 2)
points(qqplot_nn$x, qqplot_nn$y,, xlim = xylim, ylim = xylim, pch = 20, cex = 0.5, col = 3)
abline(b = 1, a = 0, col = "red")
legend(
  "topleft",
  legend = c("X", "lm", "nn", "mc", "qq"),
  col = 1:5,
  pch = 20
)
ks.test(X_test, Y_test)
ks.test(lm_test, Y_test)
ks.test(nn_test, Y_test)
