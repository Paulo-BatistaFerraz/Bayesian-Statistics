set.seed(2001)

t <- 1:200 # generating time serios points
y_white_noise <- rnorm(200, mean = 0, sd = 1) # corrected variable name from y_wwhite_noise

# Plot the simulated time series, the sample ACF, and the sample PACF
par(mfrow = c(1, 3), cex.lab = 1.3, cex.main = 1.3) # corrected 'mfrom' to 'mfrow'

# Plot the simulated time series
plot(t, y_white_noise, type = "l", xlab = "Time", ylab = "y_t", main = "Simulated white noise")

# Plot the sample ACF ( Autocorrelation function (p))
acf(y_white_noise, lag.max = 20, xlab = "Lag", ylab = "Sample ACF", ylim = c(-1, 1), main = "Sample ACF") # fixed syntax

# Plot the sample PACF ( Sample Autocorrelation function)
pacf(y_white_noise, lag.max = 20, xlab = "Lag", ylab = "Sample PACF", ylim = c(-1, 1), main = "Sample PACF") # fixed syntax



# Defining a time series object in R


par(mfrow = c(3, 1), cex.lab = 1.3, cex.main = 1.3) # corrected 'mfrom' to 'mfrow'
yt = ts(y_white_noise, start = c(1960,1), frequency = 12)
plot(yt)
yt = ts(y_white_noise, start = c(1960,1), frequency = 1)
plot(yt)
yt = ts(y_white_noise, start = c(1960,1), frequency = 4)
plot(yt)


#####################################################
######## Differencig and smoothing by moving average
###################################################

par(mfrom=c(1,1), cex.lab = 1.3, cex.main = 1.3) # corrected 'mfrom' to 'mfrow'
data(co2)

plot(co2)

## Plot ACF
acf(ts(co2), lag.max = 100)

# Plot PACF
pacf(ts(co2),lag.max = 100)

# Clear Colose comand cat("\014")


co2_ma = filter(co2, filter = c(1/24, rep(1/12,11),1/24), sides = 2)
plot(co2_ma)

plot(co2_ma, main = "", ylab = "Smooth Series")

#plot sample ACF
acf(co2_ma[!is.na(co2_ma)], lag.max = 100, main = "correlation",
    main = "Samle ACF of smoothed series")

#plot sample PACF
pacf(co2_ma[!is.na(co2_ma)], lag.max = 100, main = "correlation",
    main = "Samle PACF of smoothed series")

## doing 1st and 2 order differencing to remove effects of trend component

co2_ma_1stdiff = diff(co2_ma, differences = 1)
co2_ma_2stdiff = diff(co2_ma, differences = 2)

plot(co2_ma_1stdiff, main = "", ylab = "Smoothed & Detrended (First Diff")
plot(co2_ma_2stdiff, main = "", ylab = "Smoothed & Detrended (Second Diff")

par(mfrom = c(2,1), cex.lab = 1.3, cex.main = 1.3) # corrected 'mfrom' to 'mfrow'

#plot ACF
acf(co2_ma_1stdiff[!is.na(co2_ma_1stdiff)], lag.max = 100, main = "correlation",
    main = "Samle ACF of smoothed series")

#plot sample PACF
pacf(co2_ma_1stdiff[!is.na(co2_ma_1stdiff)], lag.max = 100, main = "correlation",
    main = "Samle PACF of smoothed series")


# R code differencing and filtering via moving averages


# Load the CO2 dataset in R
data(co2) 

# Take first differences to remove the trend 
co2_1stdiff=diff(co2,differences=1)

# Filter via moving averages to remove the seasonality 
co2_ma=filter(co2,filter=c(1/24,rep(1/12,11),1/24),sides=2)

par(mfrow=c(3,1), cex.lab=1.2,cex.main=1.2)
plot(co2) # plot the original data 
plot(co2_1stdiff) # plot the first differences (removes trend, highlights seasonality)
plot(co2_ma) # plot the filtered series via moving averages (removes the seasonality, highlights the trend)



# Load required library
library(forecast)

# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 100

# Function to simulate AR process
simulate_AR <- function(phi, n, sigma = 1) {
  # Simulate an AR process using arima.sim
  arima.sim(n = n, list(ar = phi), sd = sigma)
}

# Simulate the processes
ar0 <- rnorm(n)  # AR(0): White noise
ar1_03 <- simulate_AR(phi = 0.3, n = n)  # AR(1) with phi = 0.3
ar1_09 <- simulate_AR(phi = 0.9, n = n)  # AR(1) with phi = 0.9
ar2_03_03 <- simulate_AR(phi = c(0.3, 0.3), n = n)  # AR(2) with phi = 0.3, 0.3
ar2_09_neg08 <- simulate_AR(phi = c(0.9, -0.8), n = n)  # AR(2) with phi = 0.9, -0.8

# Plot the time series
par(mfrow = c(3, 2))  # Arrange plots in a grid

plot(ar0, type = "l", col = "blue", main = "AR(0): White Noise", xlab = "Time", ylab = "Value")
plot(ar1_03, type = "l", col = "red", main = "AR(1): Phi = 0.3", xlab = "Time", ylab = "Value")
plot(ar1_09, type = "l", col = "green", main = "AR(1): Phi = 0.9", xlab = "Time", ylab = "Value")
plot(ar2_03_03, type = "l", col = "purple", main = "AR(2): Phi = 0.3, 0.3", xlab = "Time", ylab = "Value")
plot(ar2_09_neg08, type = "l", col = "orange", main = "AR(2): Phi = 0.9, -0.8", xlab = "Time", ylab = "Value")

# Add a common title
mtext("Simulated AR Processes", outer = TRUE, cex = 1.5)

