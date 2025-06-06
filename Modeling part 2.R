install.packages("decisionSupport")

num_simulation <- 10000
apple_income_range <- c(30000,60000)
apple_cost_range <- c(15000,30000)

apple_income_range<- runif(n= 10000, min = apple_income_range[1], 
                           max = apple_income_range[2])
apple_income_range
apple_cost_range <- runif(n = 10000, min = apple_cost_range[1], 
                          max = apple_cost_range[2])


summary(apple_income_range)
summary(apple_cost_range)


apple_profits <- apple_income_range -apple_cost_range

hist(apple_profits)

abline(v=quantile(apple_profits, c(0.1,0.5,0.9), lwd=10))

sheep_income_range <- c(2000,5000)
sheep_cost_range <- c(1000,2000)

sheep_income_range<- runif(n= 10000, min = apple_income_range[1], 
                           max = apple_income_range[2])
sheep_income_range
sheep_cost_range <- runif(n = 10000, min = apple_cost_range[1], 
                          max = apple_cost_range[2])



library(decisionSupport)

input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost",
                                           "management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm",
                                               "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", 
                                        "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season", 
                                              "Management costs in a normal season"))


model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  
  overall_cost<- Labor_cost + management_cost
  
  # Estimate the final results from the model
  
  final_result <- income - overall_cost
  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 10000,
                                      functionSyntax = "plainNames")

example_mc_simulation

plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")


