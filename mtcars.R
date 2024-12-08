library(ggplot2)
install.packages("corrplot")
library(corrplot)

data(mtcars)
str(mtcars)
head(mtcars)

summary(mtcars$mpg)
sd(mtcars$mpg)

ggplot(mtcars, aes(x=mpg)) +
  geom_histogram(binwidth=2, fill="steelblue", color="black") +
  labs(title="Distribution of Miles Per Gallon",
       x="Miles Per Gallon",
       y="Frequency") +
  theme_minimal()

ggplot(mtcars, aes(y=mpg)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Boxplot of Miles Per Gallon",
       y="Miles Per Gallon") +
  theme_minimal()

ggplot(mtcars, aes(x=factor(cyl))) +
  geom_bar(fill="steelblue") +
  labs(title="Distribution of Cylinders",
       x="Number of Cylinders",
       y="Count") +
  theme_minimal()

cor(mtcars$mpg, mtcars$hp)

ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) +
  labs(title="MPG vs Horsepower",
       x="Horsepower",
       y="Miles Per Gallon") +
  theme_minimal()

model <- lm(mpg ~ hp + wt, data=mtcars)
summary(model)

# 8. Model Diagnostics
par(mfrow=c(2,2))
plot(model)

num_vars <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]
pca_result <- prcomp(num_vars, scale=TRUE)

# Scree plot
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
ggplot(data.frame(PC=1:length(var_explained), 
                  VarExplained=var_explained),
       aes(x=PC, y=VarExplained)) +
  geom_line() +
  geom_point() +
  labs(title="Scree Plot",
       x="Principal Component",
       y="Proportion of Variance Explained") +
  theme_minimal()

biplot(pca_result, scale=0, cex=c(0,1))

print(pca_result$rotation)
# Correlation matrix visualization
corrplot(cor(num_vars), method="color")
