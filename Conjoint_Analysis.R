# Load necessary libraries
library(dplyr)
library(stats)
library(car)

setwd('D:\\R Studio')
df = read.csv('pizza_data.csv')
names(df)

# Define the model
model <- 'ranking ~ C(brand, Sum) + C(price, Sum) + C(weight, Sum) + C(crust, Sum) + C(cheese, Sum) + C(size, Sum) + C(toppings, Sum) + C(spicy, Sum)'

# Fit the model using OLS
model_fit = lm(model, data = df)
summary(model_fit)

# Extract conjoint attributes
conjoint_attributes <- c('brand', 'price', 'weight', 'crust', 'cheese', 'size', 'toppings', 'spicy')


level_name <- list()
part_worth <- list()
part_worth_range <- c()
important_levels <- list()
end <- 1  # Initialize index for coefficient in params


for (item in conjoint_attributes) {
  nlevels <- length(unique(df[[item]]))
  level_name[[item]] <- unique(df[[item]])
  
  begin <- end
  end <- begin + nlevels - 1
  
  new_part_worth <- coef(model_fit)[begin:end]
  new_part_worth <- c(new_part_worth, -sum(new_part_worth))
  important_levels[[item]] <- which.max(new_part_worth) - 1
  part_worth[[item]] <- new_part_worth
  part_worth_range <- c(part_worth_range, max(new_part_worth) - min(new_part_worth))
}

print("-------------------------------------------------------------")
print("level name:")
print(level_name)
print("npw with sum element:")
print(new_part_worth)
print("imp level:")
print(important_levels)
print("part worth:")
print(part_worth)
print("part_worth_range:")
print(part_worth_range)

attribute_importance <- sapply(part_worth_range, function(i) round(100 * (i / sum(part_worth_range)), 2))
print(attribute_importance)

part_worth_dict <- list()
attrib_level <- list()
for (item in conjoint_attributes) {
  cat("Attribute :", item, "\n")
  cat("    Relative importance of attribute ", attribute_importance[which(conjoint_attributes == item)], "\n")
  cat("    Level wise part worths: \n")
  for (j in 1:length(level_name[[item]])) {
    cat("          ", level_name[[item]][j], ":", part_worth[[item]][j], "\n")
    part_worth_dict[[level_name[[item]][j]]] <- part_worth[[item]][j]
    attrib_level[[item]] <- level_name[[item]]
  }
}

print(part_worth_dict)

# Plot the relative importance of attributes
barplot(attribute_importance, names.arg = conjoint_attributes, main = 'Relative importance of attributes', xlab = 'Attributes', ylab = 'Importance')

# Compute the utility scores
utility <- sapply(1:nrow(df), function(i) {
  part_worth_dict[[as.character(df$brand[i])]] + 
    part_worth_dict[[as.character(df$price[i])]] + 
    part_worth_dict[[as.character(df$weight[i])]] + 
    part_worth_dict[[as.character(df$crust[i])]] + 
    part_worth_dict[[as.character(df$cheese[i])]] + 
    part_worth_dict[[as.character(df$size[i])]] + 
    part_worth_dict[[as.character(df$toppings[i])]] + 
    part_worth_dict[[as.character(df$spicy[i])]]
})

df$utility <- utility
utility

cat("The profile that has the highest utility score :\n")
print(df[which.max(utility), ])

for (i in conjoint_attributes) {
  cat("Preferred level in", i, "is ::", level_name[[i]][important_levels[[i]]], "\n")
}