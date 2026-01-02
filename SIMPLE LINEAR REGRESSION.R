# File name: Amazon Sale Report E Commerce Sales Dataset Kaggle Excel Clean

## SIMPLE LINEAR REGRESSION

# Analytics Question: What is the estimated sale amount for a single‑unit purchase of this product?  

installed.packages("tidyverse")
library("tidyverse")

# Load data file into r session 
data_file <- read.csv("Amazon Sale Report E Commerce Sales Dataset Kaggle Excel Clean.csv")


# Remove blank rows 
data_file_1 <- drop_na(data_file) # where null values are dropped. 


# Group by unique product, column SKU

data_file_2 <- data_file_1 %>%
  group_by(SKU) %>%
  summarise(
    unique_qty = n_distinct(Qty),
    total_rows = n()
  ) %>%
  arrange(desc(unique_qty), desc(total_rows))



# Identified top SKU
# Filter original data to contain only the identified SKU

data_file_3 <- data_file_1 %>%
  filter(SKU == "JNE3797-KR-M", Amount != 0, Qty != 0)


# Check
# unique(data_file_4$Qty) = 1,2,3
# unique(data_file_4$Amount) = 725, 1450, 771, 2175, 715, 1430, 761, 735, 724, 799, 1470



# Simple Linear Regression
SLR_model <- lm(Amount ~ Qty, data = data_file_3)
summary (SLR_model)
plot (SLR_model)




# Scatter plot with fitted linear regression line
ggplot(data_file_3, aes(x = Qty, y = Amount)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linewidth = 1.2) +
  labs(
    title = "Amount vs Qty for SKU JNE3797-KR-M",
    x = "Quantity",
    y = "Amount"
  )
 



# Residuals vs Fitted Values diagnostic plot
ggplot(data_file_3, aes(x = SLR_model$fitted.values, y = SLR_model$residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "darkgrey", linewidth = 1) +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Amount",
    y = "Residuals"
  )



# Results:
# •	Intercept (37.43): Baseline model prediction when Qty = 0; serves as a mathematical anchor rather than a real sales scenario.
# •	Slope (709.40): Each 1-unit increase in Qty increases Amount by approximately 709.40.
# •	p-value (< 2 × 10⁻¹⁶): Indicates an extremely strong and statistically significant relationship between Qty and Amount.
# •	Residuals: Tight residuals show predictions closely match observed values.
# •	R² (0.946): Qty explains 94.6% of the variation in Amount, indicating an excellent model fit.


# Analytics Answer: The model estimates a single‑unit sale at approximately 709.40


## END SIMPLE LINEAR REGRESSION - Amount vs Qty for SKU JNE3797-KR-M
