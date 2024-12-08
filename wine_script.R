install.packages("rattle")
library(ggplot2)
library(rattle)
library(corrplot)
data(wine)
str(wine)
head(wine)

summary(wine$Alcohol)
sd(wine$Alcohol)

#histogram
ggplot(wine, aes(x=Alcohol)) +
  geom_histogram(bins=20, fill="steelblue", alpha=0.7) +
  labs(title="Distribution of Alcohol Content in Wines",
       x="Alcohol (%)",
       y="Frequency") +
  theme_minimal()

# Create boxplot
ggplot(wine, aes(y=Alcohol)) +
  geom_boxplot(fill="red", alpha=0.7) +
  labs(title="Boxplot of Alcohol Content",
       y="Alcohol (%)") +
  theme_minimal()

#categorical variable
ggplot(wine, aes(x=factor(Type))) +
  geom_bar(fill="steelblue", alpha=0.7) +
  labs(title="Distribution of Wine Types",
       x="Wine Type",
       y="Count") +
  theme_minimal()

#correlation
cor(wine$Alcohol, wine$Color)

#scatterplot
ggplot(wine, aes(x=Alcohol, y=Color)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="red") +
  labs(title="Alcohol Content vs Color",
       x="Alcohol (%)",
       y="Color") +
  theme_minimal()

#linear regression
model <- lm(Alcohol ~ Color + Ash, data=wine)
summary(model)

par(mfrow=c(2,2))
plot(model)

wine_numeric <- wine[, -1]  # Remove Type column

# Perform PCA
pca_result <- prcomp(wine_numeric, scale = TRUE)

# Calculate variance explained
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Create scree plot
ggplot(data.frame(PC = 1:length(var_explained), 
                  VarExplained = var_explained), 
       aes(x = PC, y = VarExplained)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree Plot of Wine PCA",
       x = "Principal Component",
       y = "Proportion of Variance Explained") +
  theme_minimal()

biplot(pca_result, scale=0, cex=c(0,1))

print(pca_result$rotation)

corrplot(cor(wine_numeric), method="color")
