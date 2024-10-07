library(ca)

clients <- read.csv("/Users/jagritisuneja/Documents/advanced data analysis/project/clients.csv")
head(clients)

# contingency table for product_type and family_status
contingency_table <- table(clients$product_type, clients$family_status)
contingency_table
# Perform correspondence analysis
ca_result <- ca(contingency_table)

# summary of the correspondence analysis
summary(ca_result)

# correspondence analysis results
plot(ca_result, main = "Correspondence Analysis of Product Type and Family Status", mass = TRUE, arrows = c(FALSE, TRUE), ylim=y_lim_custom)

# Generate the contingency table
contingency_table <- table(clients$product_type, clients$family_status)

# Perform chi-square test
chi_square_test <- chisq.test(contingency_table)
chi_square_test

# Mosaic plot with shading based on standardized residuals
mosaicplot(contingency_table, shade = TRUE, las = 2, main = "Mosaic Plot of Product Type and Family Status")

# Add a legend for the standardized residuals
legend("topleft", legend = c("> 4", "2.4 - 4", "0 - 2.4", "-2.4 - 0", "< -2.4"),
       fill = c("red", "pink", "white", "lightblue", "blue"), bty = "n")

# Plot the correspondence analysis results with row and column categories
plot(ca_result, what = c("all", "none"), main = "Correspondence Analysis of Product Type and Family Status")

# Plot the correspondence analysis results
x_lim_custom <- c(-0.1,0.1)
y_lim_custom <- c(-0.1,0.1)

plot(ca_result, main = "Correspondence Analysis of Product Type and Family Status", mass = TRUE, xlim=c(-0.1,0.1))



# Recode product types
clients$product_type <- recode(clients$product_type,
                               "Boats" = "Leisure",
                               "Tourism" = "Leisure",
                               "Household appliances" = "Household Items",
                               "Windows & Doors" = "Household Items",
                               "Clothing" = "Household Items",
                               "Computers" = "Tech",
                               "Jewelry" = "Health & Beauty",
                               "Sporting goods" = "Health & Beauty",
                               "Furniture" = "Household Items",
                               "Training" = "Health & Beauty",
                               "Music" = "Leisure",
                               "Garden equipment" = "Household Items",
                               "Repair Services" = "Employment",
                               "Construction Materials" = "Employment",
                               "Medical services" = "Employment",
                               "Fishing and hunting supplies" = "Leisure",
                               "Auto" = "Household Items",
                               "Audio & Video" = "Leisure",
                               "Childen's goods" = "Household Items",
                               "Cell phones" = "Tech",
                               "Cosmetics and beauty services" = "Health & Beauty",
                               "Fitness" = "Health & Beauty"
)

contingency_table_grouped <- table(clients$product_type, clients$family_status)
print(contingency_table_grouped)

# Perform chi-square test
chi_square_test_grouped <- chisq.test(contingency_table_grouped)
print(chi_square_test_grouped)

# Mosaic plot with shading based on standardized residuals
mosaicplot(contingency_table_grouped, shade = TRUE, las = 2, main = "Mosaic Plot of Grouped Product Type and Family Status")
legend("topleft", legend = c("> 4", "2.4 - 4", "0 - 2.4", "-2.4 - 0", "< -2.4"),
       fill = c("red", "pink", "white", "lightblue", "blue"), bty = "n")

# Perform correspondence analysis
ca_result_grouped <- ca(contingency_table_grouped)
summary(ca_result_grouped)

# Plot the correspondence analysis results
plot(ca_result_grouped, main = "Correspondence Analysis of Grouped Product Type and Family Status", mass = TRUE, arrows = c(FALSE, TRUE))

