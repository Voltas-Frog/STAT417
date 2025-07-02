# William McKeown
# St. Cloud State University
# Stat 417 - Project 1
# Dr. Shiju Zhang



                            # Simulation for service times


# Variables
set.seed(314)               #set random number seed for repeatability
N <- 10000                  #set n to number of generations
prob_fast_mechanic <- 0.8   #given probability of fast mechanic
mean_fast_mechanic <- 3     #given service time (min) for fast mechanic
mean_slow_mechanic <- 12    #given service time (min) for slow mechanic

# Conversions
rate_fast <- 1/mean_fast_mechanic    # rate = 1/mean
rate_slow <- 1/mean_slow_mechanic    # rate = 1/mean



#Generate service times
service_times = sapply(1:N, function(k) {
  U = runif(1)  # Generate random number between 0 and 1
  
  if(U <= prob_fast_mechanic) {
    # 80% chance: get fast mechanic
    rexp(1, rate = rate_fast)
  } else {
    # 20% chance: get slow mechanic  
    rexp(1, rate = rate_slow)
  }
})

# Calculate the estimated mean service time
estimated_mean <- mean(service_times)

# Calculate theoretical mean
theoretical_mean <- (prob_fast_mechanic * mean_fast_mechanic) + ((1 - prob_fast_mechanic) * mean_slow_mechanic)

# Calculate percent difference
percent_diff <- ((estimated_mean - theoretical_mean) / theoretical_mean) * 100

# Display results
cat("Simulation Results:\n\n")
cat("Sample size:", N, "\n")
cat("Estimated mean service time:", round(estimated_mean, 3), "minutes\n")
cat("Theoretical mean service time:", theoretical_mean, "minutes\n")
cat("Percent Difference:", round(percent_diff, 3), "%\n")

# Show first 50 values as required by assignment
cat("\nFirst 50 simulated service times (in minutes):\n")
cat(round(service_times[1:10], 3), "\n")
cat(round(service_times[11:20], 3), "\n")
cat(round(service_times[21:30], 3), "\n")
cat(round(service_times[31:40], 3), "\n")
cat(round(service_times[41:50], 3), "\n")

# Histogram
hist(service_times, probability = TRUE,            
     xlab = "Service Time (minutes)",              #X axis label
     main = "Histogram of Service Times",          #Title
     breaks = 12)                                  #How many bins

# Add vertical lines for simulated and theoretical means
abline(v = estimated_mean, col = 2, lty = 2, lwd = 2)
abline(v = theoretical_mean, col = 3, lty = 3, lwd = 2)

#Add Legend
legend(40,                                            #X Location
       .14,                                           #Y Location
       c("Simulated Mean","Theoretical Mean"),        #Labels
       col = 2:3,                                     #Color for lines
       text.col = 2:3,                                #Color for Text (should match lines)
       lty = 2:3,                                     #Line type (match abline)
       lwd = 2,                                       #Line width
       bty = "n")                                     #Box around legend "n" - no    "o" for box

#Curve equation
theoretical_curve = function(x) {
  prob_fast_mechanic * rate_fast * exp(-rate_fast * x) + 
  (1 - prob_fast_mechanic) * rate_slow * exp(-rate_slow * x)
}

#Curve line
curve(theoretical_curve, 
      add = TRUE, 
      col = "green", 
      lwd = 2)



