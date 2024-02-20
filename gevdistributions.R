library(fExtremes)
library(extRemes)
library(xts)
library(ismev)
summary(model.gev)
#SIMULATING THE WEIBULL DISTRIBUTION
my_weibull <- gevSim(model = list(xi = -0.25, mu =0, beta =1), n = 1000, seed = NULL)
 # SIMULATING THE FRECHET DISTRIBUTION
my_frechet <- gevSim(model = list(xi = 0.25, mu = 0, beta =1), n = 1000, seed = NULL)
#SIMULATING THE GUMBELL DISTRIBUTION
 my_gumbel <- gumbelSim(list(mu=0, beta =1), n= 1000, seed = NULL)
gum<-gevSim(model = list(xi =0, mu =0, mu =0, beta =1), n =1000, seed = NULL) 

G <- density(my_gumbel)
W <- density(my_weibull)
F <- density(my_frechet)

plot(G, col = 'blue', main = 'gev LIMITING DISTRIBUTIONS')
lines(W, col = 'red')
lines(F, col = 'green')
legend(x = "topright", legend = c("Gumbell", "Weibull", "Fretchet"), 
       col = c("blue", "red", "green"))
library(quantmod)
library(perfomanceAnalytics)
getSymbols("IBM", from = "2010-01-04", to = "2023-12-31")
ibm <- IBM
ibm[1]
dim(IBM)
IBM_ret <- diff(log(IBM$IBM.Close))[-1]
head(IBM_ret)
plot(IBM_ret)

#Block Creation
df <- IBM_ret
T <- length(df)
n <- 21
blocks <- split(df, rep(1:ceiling(T/n), each = n, length.out = T))
length(blocks)


for ( i in 1:length(blocks)) {
  blocks[[i]] <- 
}
gevfitted1 <- fevd(x= max_values, type = "GEV", time.units = "months")


### Assignment
Assignmentdata <- c(7.4, 0, 0.6, 3.4, 0, 0, 0.7, 1.5, 2.2,9.2, 6.9, 0, 2.9, 3, 1.3, 9.3, 22.8, 11.5, 129.8, 47.0, 17.2, 12.8, 3.2)
years <- 1971:1993
assignmentdf <-data.frame(years, Assignmentdata)
length(Assignmentdata)
length(years)
assignmentdf
library(fitdistrplus)
initial_params <- list(
  location = 12.72609,
  scale = 27.60618,
  shape = -1e16  # You may need to provide a reasonable initial shape parameter value
)
fit_result <- fitdist(Assignmentdata, "gev", start = initial_params)

## fitdist package documentation in r
fit_result <- fitdist(Assignmentdata, "gev", method = "mle", start = initial_params, discrete = FALSE, fix.arg = NULL, keepdata = TRUE, keepdata.nb = 100)

fit_result <- fevd(Assignmentdata, type = "GEV", initial = initial_params)
fit_result <- fevd(Assignmentdata, type = "GEV", method = "MLE")

plot(fit_result, main = 'Plotting the Assignment Data')
fit_result$par["location"]
scale_param <- fit_result$par["scale"]

## plotting the Assignment data
plot(assignmentdf$Assignmentdata, main = 'Distribution')


## Fit GEV Distribution
fit_result <- fevd(Assignmentdata, type = "GEV", initial = initial_params)
plot(fit_result, main = "GEV DISTRIBUTION OF THE DATA")


## gev youtube
## ismev and fextRemes packages
summary(assignmentdf$Assignmentdata)
gev.fit(Assignmentdata)
