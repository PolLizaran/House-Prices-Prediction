
# We load the libraries that will be used.

library(caTools)
library(corrplot)
library(dplyr)
library(factoextra)
library(foreign)
library(GGally)
library(ggplot2)
library(leaflet)
library(MASS)
library(VIM)
library(viridis)

# We load our data set.

Houses <- read.arff("house_sales_reduced.arff")


# Preprocessing

## Visualization of the dataset

Houses[1:9,]


## Inspection of the dataset

# Atributes names.

names(Houses)

summary(Houses)

# The following command allow us to see the data types of all the variables.

str(Houses)

## Treatment of mixed datatypes

# Converting 'sqft_lot15' to numerical since it has too many levels.

Houses$sqft_lot15 <- as.numeric(Houses$sqft_lot15)

# We convert the square feet into square meters using the fact that 1 ft^2 = 0.09290304 m^2.

Houses[,"sqft_living"] <- Houses[,"sqft_living"]*0.09290304
Houses[,"sqft_lot"] <- Houses[,"sqft_lot"]*0.09290304
Houses[,"sqft_above"] <- Houses[,"sqft_above"]*0.09290304
Houses[,"sqft_basement"] <- Houses[,"sqft_basement"]*0.09290304
Houses[,"sqft_living15"] <- Houses[,"sqft_living15"]*0.09290304
Houses[,"sqft_lot15"] <- Houses[,"sqft_lot15"]*0.09290304

# We rename the columns to fit the new data.

names(Houses)[6] = "sqm_living"
names(Houses)[7] = "sqm_lot"
names(Houses)[13] = "sqm_above"
names(Houses)[14] = "sqm_basement"
names(Houses)[20] = "sqm_living15"
names(Houses)[21] = "sqm_lot15"
Houses[1:9,]

## PCA 

# Seleccionar només les variables que volguem

Houses_for_pca <- subset(Houses, select = - c(id, attribute_0))
(houses_pca <- prcomp(Houses_for_pca, scale = TRUE))
fviz_pca_biplot(houses_pca, repel = TRUE, axes = c(1,2), col.ind = "#CDC5BF", col.var = "#7A67EE")
fviz_eig(houses_pca, ylim = c(1, 100), addlabels = TRUE, barcolor = 1, barfill = "darkorange2", 
         main = "Scree Plot of nyc_PCA")

## Data visualization

# The houses are placed in a map in order to properly visualize the dataset. 

# We arbitrarily separate the houses in three distinct categories. 

high_calif <- Houses$grade >= 9 
med_calif <- (Houses$grade < 9 & Houses$grade > 4)
low_calif <- Houses$grade <= 4
Houses_HG <- subset(Houses, high_calif == TRUE) # High grade
Houses_MG <- subset(Houses, med_calif == TRUE) # Medium grade
Houses_LG <- subset(Houses, low_calif == FALSE) # Low grade

set_points <- data.frame( lat = c(min(Houses$lat) - 0.07, max(Houses$lat) + 0.07, max(Houses$lat) + 0.07,
                                  min(Houses$lat) - 0.07, min(Houses$lat) - 0.07),
                          long = c(min(Houses$long) - 0.07, min(Houses$long) - 0.07, max(Houses$long) + 0.07,
                                   max(Houses$long) + 0.07, min(Houses$long) - 0.07)) # Added extra lat and long in order to box observations better



m <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = Houses_LG, lng = Houses_LG$long, lat = Houses_LG$lat, popup = "Seattle",
                   radius = 1, color = "#FF1493", stroke = F, opacity = 0.5) %>%
  addCircleMarkers(data = Houses_MG, lng = Houses_MG$long, lat = Houses_MG$lat, popup = "Seattle",
                   radius = 0.5, color = "black", stroke = F, opacity = 2) %>%
  addCircleMarkers(data = Houses_HG, lng = Houses_HG$long, lat = Houses_HG$lat, popup = "Seattle",
                   radius = 0.5, color = "#00BFFF", stroke = F, opacity = 5) %>%
  addPolylines(data = set_points, lng = ~long, lat = ~lat, weight = 3,
               opacity = 6, col = "black")
addLegend(m, position = "topright", labels = c("High", "Medium","Low"), colors = c("#00BFFF", "black","#FF1493"),
          title = "Grade qualification")

## Categorization of variables

# Certain variables are transformed into factors to represent them correctly.

Houses$waterfront <- as.factor(Houses$waterfront)

Houses$view <- as.factor(Houses$view)

Houses$condition <- factor(Houses$condition)

Houses$floors <- as.factor(Houses$floors)

## Feature extraction

# A new variable is created using already existent ones.

Houses$age <- 2015 - apply(dplyr::select(Houses, yr_built, yr_renovated), FUN = max, MARGIN = 1) # This new variable represents the years passed with no modifications done to the house.

## Splitting of the data

# The data is separated into a training dataset and a testing dataset. 

set.seed(18) # This line is for reproducibility

sample <- sample.split(Houses, SplitRatio = 0.8)
Houses_tr <- subset(Houses, sample == TRUE)
Houses_te <- subset(Houses, sample == FALSE)

## Treatment of missing values (after splitting)

# We check if there is NA values.

sample3 <- is.na(Houses_tr)
table(sample3)

# We observe no NA values defined as such.

# We'll study if variables' values are coherent.

# Search of missing values on numerical variables

cat('\n', "Bedrooms")
table(Houses_tr[,"bedrooms"] == 0)
cat('\n', "Bathrooms")
table(Houses_tr[,"bathrooms"] == 0)
cat('\n', "Sqm_living")
table(Houses_tr[,"sqm_living"] == 0)
cat('\n', "Sqm_lot")
table(Houses_tr[,"sqm_lot"] == 0)
cat('\n', "Floors")
table(Houses_tr[,"floors"] == 0)
cat('\n', "Sqm_above")
table(Houses_tr[,"sqm_above"] == 0)
cat('\n', "Sqm_basement")
table(Houses_tr[,"sqm_basement"] == 0)
cat('\n', "Yr_built")
table(Houses_tr[,"yr_built"] == 0)
cat('\n', "Yr_renovated")
table(Houses_tr[,"yr_renovated"] == 0)
cat('\n', "Zipcode")
table(Houses_tr[,"zipcode"] == 0)
cat('\n', "Lat")
table(Houses_tr[,"lat"] == 0)
cat('\n', "Long")
table(Houses_tr[,"long"] == 0)
cat('\n', "Sqm_living15")
table(Houses_tr[,"sqm_living15"] == 0)
cat('\n', "Sqm_lot15")
table(Houses_tr[,"sqm_lot15"] == 0)

# We observe houses containing $0$ bedrooms and/or bathrooms, which initially does no make sense.

# We check the houses with a incoherent value for bathrooms and/or bedrooms.

sample1 <- Houses_tr$bedrooms == 0
(House_no_bed <- subset(Houses_tr, sample1 == TRUE))
sample2 <- Houses_tr$bathrooms == 0
(House_no_bath <- subset(Houses_tr, sample2 == TRUE))
```

# As no NANs are found, we transform the zero values of _bedrooms_ and _bathrooms_ into NANs. 
# By this, we'll be able to impute them a value based on their neighbours

Houses_tr$bedrooms[Houses_tr$bedrooms == 0] <- NA
Houses_tr$bathrooms[Houses_tr$bathrooms == 0] <- NA

summary(Houses_tr)

Bath <- data.frame(
  atr <- which(is.na(Houses_tr$bathrooms)),
  s <- Houses_tr$sqm_living[atr],
  Floors <- Houses_tr$floors[atr]
)  

ggplot(Bath, aes(x = atr, y = s, color = Floors)) + 
  geom_point(show.legend = TRUE, shape=18, size=3) +
  xlab('attribute_0') +
  ylab('sqm_living')  +
  labs(title = "MISSING VALUES OF BATHROOMS") + 
  scale_color_manual(values=c('#030303','#FF4500','#4EEE94', '#FFE7BA')) +
theme_minimal()+theme(panel.background = element_rect(fill = "gray"),
                      legend.position="bottom")

Bed <- data.frame(
  atr <- which(is.na(Houses_tr$bedrooms)),
  s <- Houses_tr$sqm_living[atr],
  Floors <- Houses_tr$floors[atr]
)  

ggplot(Bed, aes(x = atr, y = s, color = Floors)) + 
  geom_point(show.legend = TRUE, shape=18, size=3) +
  xlab('attribute_0') +
  ylab('sqm_living')  +
  labs(title = "MISSING VALUES OF BEDROOMS") + 
  scale_color_manual(values=c('#030303','#FF4500','#4EEE94', '#FFE7BA')) +
  theme_minimal()+theme(panel.background = element_rect(fill = "gray"),
                        legend.position="bottom")

## Imputation of new values to the missing values

# New values wil be assign to the missing values through the K-nn algorithm

names_columns <- colnames(Houses_tr)
var_to_use_knn <- names_columns[names_columns != c("attribute_0", "id")] # We don't want certain variables to be used for knn method

houses_knn <- kNN(Houses_tr, variable = c("bedrooms", "bathrooms"), dist_var = var_to_use_knn, k = 129, imp_var = FALSE) #imp_var avoids the creation of variables showing imputation status

Houses_tr <- houses_knn
summary(Houses_tr)

# We choose k = 129 as it is sqrt(SIZE) of our training data. 

## Identification and treatment of outliers

# We plot the values for each variable in order to visually find outliers.

# Price
ggplot(Houses_tr, aes(attribute_0, price)) + 
  geom_point(color = "#AB82FF") +
  ylab("price ($)") + 
  geom_point(data = Houses_tr[which(Houses_tr$bedrooms == 33),], colour = "black") +
  geom_point(data = Houses_tr[which(Houses_tr$sqm_living > 1000),], colour = "red")
head(Houses_tr[order(Houses_tr$price, decreasing = TRUE),])

# Bedrooms
ggplot(Houses_tr, aes(attribute_0, bedrooms)) + 
  geom_point(color = "#AB82FF")  +
  geom_point(data = Houses_tr[which(Houses_tr$bedrooms == 33),], colour = "black") +
  geom_point(data = Houses_tr[which(Houses_tr$sqm_living > 1000),], colour = "red")
head(Houses_tr[order(Houses_tr$bedrooms, decreasing = TRUE),])

# Bathrooms
ggplot(Houses_tr, aes(attribute_0, bathrooms)) + 
  geom_point(color = "#AB82FF") +
  geom_point(data = Houses_tr[which(Houses_tr$bedrooms == 33),], colour = "black")  +
  geom_point(data = Houses_tr[which(Houses_tr$sqm_living > 1000),], colour = "red")
head(Houses_tr[order(Houses_tr$bathrooms, decreasing = TRUE),])

# Sqm_living
ggplot(Houses_tr, aes(attribute_0, sqm_living)) + 
  geom_point(color = "#AB82FF") +
  geom_point(data = Houses_tr[which(Houses_tr$bedrooms == 33),], colour = "black") +
  geom_point(data = Houses_tr[which(Houses_tr$sqm_living > 1000),], colour = "red")
head(Houses_tr[order(Houses_tr$sqm_living, decreasing = TRUE),])

# Sqm_lot
ggplot(Houses_tr, aes(attribute_0, sqm_lot)) + 
  geom_point(color = "#AB82FF") +
  geom_point(data = Houses_tr[which(Houses_tr$bedrooms == 33),], colour = "black") +
  geom_point(data = Houses_tr[which(Houses_tr$sqm_living > 1000),], colour = "red")
head(Houses_tr[order(Houses_tr$sqm_lot, decreasing = TRUE),])

# Sqm_above
ggplot(Houses_tr, aes(attribute_0, sqm_above)) + 
  geom_point(color = "#AB82FF") +
  geom_point(data = Houses_tr[which(Houses_tr$bedrooms == 33),], colour = "black") +
  geom_point(data = Houses_tr[which(Houses_tr$sqm_living > 1000),], colour = "red")
head(Houses_tr[order(Houses_tr$sqm_above, decreasing = TRUE),])

# Sqm_basement
ggplot(Houses_tr, aes(attribute_0, sqm_basement)) + 
  geom_point(color = "#AB82FF") +
  geom_point(data = Houses_tr[which(Houses_tr$bedrooms == 33),], colour = "black") +
  geom_point(data = Houses_tr[which(Houses_tr$sqm_living > 1000),], colour = "red")
head(Houses_tr[order(Houses_tr$sqm_basement, decreasing = TRUE),])

# Yr_built
ggplot(Houses_tr, aes(attribute_0, age)) + 
  geom_point(color = "#AB82FF") +
  geom_point(data = Houses_tr[which(Houses_tr$bedrooms == 33),], colour = "black") +
  geom_point(data = Houses_tr[which(Houses_tr$sqm_living > 1000),], colour = "red")
head(Houses_tr[order(Houses_tr$yr_built, decreasing = TRUE),])

# House $'1225069038'$ has a very large dimension on *sqm_living*, was built in *1999* but is considerably cheaper than other houses. 
# We will delete this individual.

# Elimination of outlier. 

Houses_tr <- Houses_tr[!Houses_tr$id == 1225069038,]
if(! any(Houses_tr$id == 1225069038)){
  print("Deleted successfully")
}else{ 
  print("Error!")
}

# Manual imputation of bedrooms due to human error.
Houses_tr$bedrooms[Houses_tr$id == 2402100895] <- 3
(Houses_tr[Houses_tr$id == 2402100895,])

# We search for outliers using z score.

# We create a separate dataset to work on.
Houses_z <- Houses_tr

Houses_z$price_z_score <- abs(scale(Houses_z$price, center = TRUE, scale = TRUE))
Houses_z$bedrooms_z_score <- abs(scale(Houses_z$bedrooms, center = TRUE, scale = TRUE))
Houses_z$bathrooms_z_score <-  abs(scale(Houses_z$bathrooms, center = TRUE, scale = TRUE))
Houses_z$living_z_score <- abs(scale(Houses_z$sqm_living, center = TRUE, scale = TRUE))
Houses_z$lot_z_score <-  abs(scale(Houses_z$sqm_lot, center = TRUE, scale = TRUE))
Houses_z$above_z_score <-  abs(scale(Houses_z$sqm_above, center = TRUE, scale = TRUE))
Houses_z$age_z_score <-  abs(scale(Houses_z$age, center = TRUE, scale = TRUE))

collist <- c(23, 24, 25, 26, 27, 28, 29)

num <- vector()

for (row in 1:length(Houses_z[,1])) 
{ 
  sum <- 0
  for (element in collist) 
  {
    if (Houses_z[row,element] > 3){ # If the z_score value is higher than 3, we will count that as an outlier. 
      sum <- sum + 1
    }
  }
  num <- c(num, sum)
}

Houses_z$num <- num

# We will consider outliers those houses with a number higher of 3.
outlier_id <- Houses_z$id[Houses_z$num > 3]
Houses_tr <- Houses_tr[!(Houses_tr$id %in% outlier_id),]

## Gaussianity

# We transform the data for price in order to obtain gaussianity. 

h <- hist(Houses_tr$price, xlab = "price ($)", main = "Histogram of price", 
          breaks = 25, ylim = c(0, 6000))
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
h <- hist(Houses_tr$price, col = "#87CEFA", breaks = 25, ylim = c(0, 6000),  add = TRUE)
text(h$mids, h$counts, labels = h$counts, adj=c(0.5, -0.5), cex=0.55)

h <- hist(log(Houses_tr$price), xlab = "log(price)" , main = "Histogram of log(price)", 
          ylim = c(0,1), breaks = 15, prob = TRUE, col = "#87CEFA")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
h <- hist(Houses_tr$price, ylim = c(0,1), col = "#87CEFA",  add = TRUE,  breaks = 15, prob = TRUE)
curve(dnorm(x, mean(log(Houses_tr$price)), sd(log(Houses_tr$price))), col = "#EE6AA7", add = TRUE, lwd = 1.75)

Houses_tr$price = (log(Houses_tr$price)) # Apply it for the training set

Houses_te$price = (log(Houses_te$price)) # Apply it for the test set

# We transform the data for sqm_living in order to obtain gaussianity.

h <- hist(Houses_tr$sqm_living, xlab = "Squared metres of living", main = "Histogram of sqm_living", 
          breaks = 25, ylim = c(0, 6000))
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
h <- hist(Houses_tr$sqm_living, col = "#87CEFA", breaks = 25, ylim = c(0, 6000),  add = TRUE)
text(h$mids, h$counts, labels = h$counts, adj=c(0.5, -0.5), cex=0.55)

h <- hist(log(Houses_tr$sqm_living), xlab = "log(Squared metres of living)", main = "Histogram of log(sqm_living)", 
          xlim = c(3,8), ylim = c(0,1), breaks = 15, prob = TRUE)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
h <- hist(log(Houses_tr$sqm_living), ylim = c(0,1), col = "#87CEFA",  add = TRUE,  breaks = 15, prob = TRUE)
curve(dnorm(x, mean(log(Houses_tr$sqm_living)), sd(log(Houses_tr$sqm_living))), col = "#EE6AA7", add = TRUE, lwd = 1.75)

Houses_tr$sqm_living = (log(Houses_tr$sqm_living)) # Apply it for the training set.

Houses_te$sqm_living = (log(Houses_te$sqm_living)) # Apply it for the test set.

# We transform the data for sqm_lot in order to obtain gaussianity. 

hist(Houses_tr$sqm_lot, main="Squared metres of lot")

h <- hist(log(Houses_tr$sqm_lot), xlab = "sqm_lot_transf", main = "Transformed squared metres of lot", 
          ylim = c(0,5), breaks = 30, prob = TRUE)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
h <- hist(log(Houses_tr$sqm_lot), ylim = c(0,5), col = "#87CEFA",  add = TRUE,  breaks = 30, prob = TRUE, xlab = "sqm_lot")
curve(dnorm(x, mean(log(Houses_tr$sqm_lot)), sd(log(Houses_tr$sqm_lot))), col = "#EE6AA7", add = TRUE, lwd = 1.75)

Houses_tr$sqm_lot = (log(Houses_tr$sqm_lot)) # Apply it for the training set.

Houses_te$sqm_lot = (log(Houses_te$sqm_lot)) # Apply it for the test set.

# We transform the data for sqm_above in order to obtain gaussianity. 

hist(Houses_tr$sqm_above, main="Squared metres above")

bx = boxcox(I(sqm_above) ~ . - id - attribute_0, data = Houses_tr,
            lambda = seq(-0.5, 0.5, length = 10))

lambda = bx$x[which.max(bx$y)]

sprintf("The value of lambda used is: %f", lambda)

sqm_above_transf = (Houses_tr$sqm_above^lambda - 1)/lambda

h <- hist(sqm_above_transf, xlab = "sqm_above", main = "Squared metres above", 
          ylim = c(0,1), breaks = 30, prob = TRUE)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
h <- hist((sqm_above_transf), ylim = c(0,1), col = "#87CEFA",  add = TRUE,  breaks = 30, prob = TRUE)
curve(dnorm(x, mean(sqm_above_transf), sd(sqm_above_transf)), col = "#EE6AA7", add = TRUE, lwd = 1.75)

# As it doesn't correct the skewness totally, we decide not to apply it.

# We transform the data for sqm_above in order to obtain gaussianity. 

hist(Houses_tr$sqm_basement, main="Squared metres basement", breaks = 30, col = "#87CEFA", xlab = "sqm_basement")

# It is already Gaussian.

# We transform the data for sqm_living15 in order to obtain gaussianity.

hist(Houses_tr$sqm_living15, main="Squared metres15", breaks = 30)

bx = boxcox(I(Houses_tr$sqm_living15) + 1 ~ . - id - attribute_0, data = Houses_tr,
             lambda = seq(-0.25, 0.25, length = 10)) # if we don't add 1, we would compute negative logarithms

lambda = bx$x[which.max(bx$y)]

sprintf("The value of lambda used is: %f", lambda)

sqm_living15_transf = (Houses_tr$sqm_living15^lambda - 1)/lambda


h <- hist(sqm_living15_transf, xlab = "sqm_living15_transf", main = "Squared metres of living15", 
          breaks = 30)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
h <- hist((sqm_living15_transf),col = "#87CEFA",  add = TRUE,  breaks = 30)

# We don't apply the Box Cox Transformation.

# We transform the data for sqm_lot15 in order to obtain gaussianity.

hist(Houses_tr$sqm_lot15, main="Squared metres15", breaks = 30)

bx = boxcox(I(Houses_tr$sqm_lot15) + 1 ~ . - id - attribute_0, data = Houses_tr,
             lambda = seq(-1, 1, length = 10)) # if we don't add 1, we would compute negative logarithms

lambda = bx$x[which.max(bx$y)]

sprintf("The value of lambda used is: %f", lambda)

sqm_lot15_transf = (Houses_tr$sqm_lot15^lambda - 1)/lambda

h <- hist(sqm_lot15_transf + 1, xlab = "sqm_lot15_transf", main = "Squared metres of lot15", 
          breaks = 30)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
h <- hist((sqm_lot15_transf + 1),col = "#87CEFA",  add = TRUE,  breaks = 30)

# We do not apply any transformation.

# We download the processed data to work in python

write.csv(Houses_tr,"Processed_tr.csv", row.names = FALSE)
write.csv(Houses_te,"Processed_te.csv", row.names = FALSE)

## Correlation between variables

# Correlation plot of numerical variables

Houses_smth <- subset(Houses_tr, select = - c(id, attribute_0))
ggcorr(Houses_smth, hjust = 0.85,  size = 3, color = "black", type = "upper", layout.exp = 2, 
       label = TRUE, label_size = 2.5, low = "yellow", mid = "orange", high = "red")+
  labs(title = "Correlation Heat-Map between variables") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) 

