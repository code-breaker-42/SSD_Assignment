# Loading MASS package
library(MASS)
library(ggplot2)
# Loading Boston dataset
data(Boston)
str(Boston)
head(Boston)

summary(Boston$medv)
sd(Boston$medv)

# Create histogram
ggplot(Boston, aes(x=medv)) +
  geom_histogram(bins=30, fill="steelblue", color="black") +
  labs(title="Distribution of Median Home Values in Boston",
       x="Median Home Value ($1000s)",
       y="Frequency") +
  theme_minimal()

# Create boxplot
ggplot(Boston, aes(y=medv)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Boxplot of Median Home Values",
       y="Median Home Value ($1000s)") +
  theme_minimal()

#categorical variable
ggplot(Boston, aes(x=factor(chas))) +
  geom_bar(fill="steelblue", alpha=0.7) +
  labs(title="Distribution of Properties by Charles River Location",
       x="Bounds Charles River",
       y="Count") +
  scale_x_discrete(labels=c("No", "Yes")) +
  theme_minimal()

#Pearson correlation
cor(Boston$medv, Boston$lstat)
#Scatter plot
ggplot(Boston, aes(x=lstat, y=medv)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="red") +
  labs(title="Home Values vs Lower Status Population",
       x="Lower Status Population (%)",
       y="Median Home Value ($1000s)") +
  theme_minimal()

#multiple linear regression
model <- lm(medv ~ lstat + rm, data=Boston)
summary(model)

par(mfrow=c(2,2))
plot(model)

#pca
boston_pca <- prcomp(Boston, scale = TRUE)

#variance explained
var_explained <- boston_pca$sdev^2 / sum(boston_pca$sdev^2)

# Creating scree plot
ggplot(data.frame(PC = 1:length(var_explained), 
                  VarExplained = var_explained), 
       aes(x = PC, y = VarExplained)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree Plot of Boston Housing PCA",
       x = "Principal Component",
       y = "Proportion of Variance Explained") +
  theme_minimal()

#pca intepret
biplot(boston_pca, scale=0, cex=c(0,1))

print(boston_pca$rotation)
