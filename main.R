# R code to implement the A-ApBM algorithm and generate toy data

# Set random seed for reproducibility
set.seed(123)

# Total number of sessions
T <- 20  # You can adjust this number as needed

# Initialize vectors to store variables
ITR <- numeric(T + 1)      # Intended Training Ratio, extra element for t+1
D <- numeric(T)            # Difficulty Index
P <- numeric(T)            # Performance Index
sigma_ITR <- numeric(T)    # Standard deviation of ITRs up to t-1

# Initial ITR values for warm-up sessions
ITR[1:3] <- 0.8  # 80% for the first three warm-up sessions

# Placeholder for alpha parameters (assumed known for toy data)
alpha0 <- 0
alpha1 <- -1
alpha2 <- 4
alpha3 <- 1

# Initialize beta estimates (will be updated dynamically)
beta0_est <- 1
beta1_est <- 0.5
beta2_est <- -0.2

# Generate target difficulty curve (inverted U-shape)
peak_session <- round(T / 2)
session_positions <- 1:T
max_D_value <- 1
min_D_value <- 0
target_D_vector <- -(session_positions - peak_session)^2 + (peak_session^2)
# Normalize target_D_vector to be between min_D_value and max_D_value
target_D_vector <- (target_D_vector - min(target_D_vector)) / (max(target_D_vector) - min(target_D_vector))

# Main loop for each session
for (t in 1:T) {
  if (t <= 3) {
    # Warm-up sessions
    # Simulate performance data
    correct_rate <- runif(1, 0.7, 0.9)
    median_log_rt <- rnorm(1, mean = log(0.5), sd = 0.1)
    P[t] <- correct_rate + median_log_rt  # Simplified Performance Index
  } else {
    # Calculate sigma_ITR up to t-1
    sigma_ITR[t] <- sd(ITR[1:(t - 1)])
    
    # Calculate Difficulty Index D_it
    D[t] <- alpha0 + alpha1 * ITR[t] + alpha2 * (ITR[t]^2) + alpha3 * sigma_ITR[t]
    
    # Simulate performance P_it
    epsilon <- rnorm(1, mean = 0, sd = 0.1)
    P[t] <- beta0_est + beta1_est * D[t] + beta2_est * (D[t]^2) + epsilon
    
    # Update beta estimates using data up to session t-1
    if (t >= 5) {
      data_fit <- data.frame(P = P[4:(t - 1)], D = D[4:(t - 1)])
      model <- lm(P ~ D + I(D^2), data = data_fit)
      beta_est <- coef(model)
      beta0_est <- beta_est[1]
      beta1_est <- beta_est[2]
      beta2_est <- beta_est[3]
    }
    
    # Adjust ITR for the next session to align with the inverted U-shaped difficulty curve
    target_D <- target_D_vector[t]
    
    # Define the function to compute D_it given ITR_it
    D_function <- function(ITR_it) {
      D_it <- alpha0 + alpha1 * ITR_it + alpha2 * (ITR_it^2) + alpha3 * sigma_ITR[t]
      return(D_it)
    }
    
    # Objective function to minimize the difference between computed D_it and target_D
    objective_function <- function(ITR_it) {
      D_it <- D_function(ITR_it)
      return((D_it - target_D)^2)
    }
    
    # Find ITR_it that minimizes the objective function (bounded between 0 and 1)
    res <- optimize(objective_function, interval = c(0, 1))
    ITR[t + 1] <- res$minimum
    
    # Ensure ITR[t + 1] is within bounds
    ITR[t + 1] <- max(0, min(1, ITR[t + 1]))
  }
}

# Compile the data into a data frame for analysis
data_output <- data.frame(
  Session = 1:T,
  ITR = ITR[1:T],
  Sigma_ITR = sigma_ITR[1:T],
  Difficulty = D,
  Performance = P
)

# Display the data
print(data_output)

# Plot the target difficulty curve and actual difficulty values
plot(1:T, target_D_vector[1:T], type = 'l', col = 'blue', ylim = c(0, 1),
     ylab = 'Difficulty', xlab = 'Session', main = 'Target and Actual Difficulty Levels')
lines(1:T, D, col = 'red')
legend('topright', legend = c('Target Difficulty', 'Actual Difficulty'), col = c('blue', 'red'), lty = 1)

# Plot the Intended Training Ratio (ITR) over sessions
plot(1:T, ITR[1:T], type = 'l', col = 'green', ylim = c(0, 1),
     ylab = 'Intended Training Ratio', xlab = 'Session', main = 'ITR over Sessions')

# Plot the Performance Index over sessions
plot(1:T, P, type = 'l', col = 'purple',
     ylab = 'Performance Index', xlab = 'Session', main = 'Performance Index over Sessions')
