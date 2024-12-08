library(ggplot2)
data(diamonds)
str(diamonds)
head(diamonds)

summary(diamonds$price)
sd(diamonds$price)

hist(diamonds$price, main="Histogram of Diamond Prices", xlab="Price ($)")
boxplot(diamonds$price, main="Boxplot of Diamond Prices", ylab="Price ($)")


#barplot(table(diamonds$cut), main="Distribution of Diamond Cuts", xlab="Cut", ylab="Count")
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut)) +
  labs(title = "Distribution of Diamond Cuts", x = "Cut", y = "Count")

cor(diamonds$carat, diamonds$price)


ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship between Carat and Price",
       x = "Carat", y = "Price ($)") +
  theme_minimal()

model <- lm(price ~ carat + depth, data = diamonds)
summary(model)

par(mfrow=c(2,2))
plot(model)

# Select numerical variables
num_vars <- diamonds[, c("carat", "depth", "table", "price", "x", "y", "z")]

# Perform PCA
pca_result <- prcomp(num_vars, scale. = TRUE)

# Create scree plot
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

ggplot(data = data.frame(pc = 1:length(var_explained), 
                         var = var_explained), 
       aes(x = pc, y = var)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scree Plot",
       x = "Principal Component",
       y = "Explained Variance") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:length(var_explained))

biplot(pca_result, scale=0, cex=c(0,1))

# Print loadings
print(pca_result$rotation)
