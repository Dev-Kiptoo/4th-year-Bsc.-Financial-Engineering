plot(assignmentdf$Assignmentdata, type = "l", col = "darkblue", 
     main = 'Plotting the Data', xlab = 'Years', ylab = "Annual loss")
axis(1, at = seq_along(assignmentdf$years), labels = assignmentdf$years, cex.axis = 0.75)

fit1 <- fevd(Assignmentdata, assignmentdf, units = "deg C")
distill_output <-distill(fit1)

distillate <- as.data.frame(distill_output[c("location", "scale", "shape")])

print(distillate)

write.csv(distillate, "c://Users//Bethuu//Downloads//output_file.csv", row.names = FALSE)

plot(fit1, "trace")
return.level(fit1)
return.level(fit1, do.ci = TRUE)
ci(fit1, return.period = c(2, 20, 100))

# Assuming 'fit1' is the result of the fevd function
qqplot(fit1, main = "Q-Q Plot for Fitted Distribution")


ci(fit1, type = "parameter")

ci(fit1, type = "parameter")
return.level(fit1, do.ci = TRUE)

alpha <- 0.05
lc <- distillate[1,1]
sc <- distillate[2,1]
sl <- distillate[3,1]
# Calculate VaR using the fitted distribution
var_estimate <- qgev(u = alpha, para =c(lc, sc, sl))


#Var calculation
calculate_var_gev <- function(alpha, location, scale, shape) {
  # Calculate VaR using the GEV distribution formula
  var_estimate <- location + (scale / shape) * ((-log(1 - alpha))^(-shape) - 1)
  return(var_estimate)
}

# Example usage:
alpha <- 0.05  # 95% confidence level
location <- 1.2334688  # Replace with the actual location parameter
scale <- 2.3751919  # Replace with the actual scale parameter
shape <- 1.6363086  # Replace with the actual shape parameter

var_estimate <- calculate_var_gev(alpha, location, scale, shape)
cat("VaR (", 100*(1 - alpha), "%):", var_estimate, "\n")
