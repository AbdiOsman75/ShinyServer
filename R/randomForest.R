library(randomForest)
library(ranger)
library(dplyr)
library(corrplot)

#Import the dataset from csv, importing Strings as characters to change NA into None
Iowa_train <- read.csv("Iowa_train.csv", stringsAsFactors = FALSE)

#Remove the the Id column as it is not needed
I2 <- select(Iowa_train,-Id)

#Dealt with missing data and factors-----
#Replace NA with None
I2[, c(2,5:16,21:25,27:33,35,39:42,53,55,57:58,60,63:65,72:74,78:79)][is.na(I2[, c(2,5:16,21:25,27:33,35,39:42,53,55,57:58,60,63:65,72:74,78:79)])] <- "None"

#Round years into decades as the random forest can't handle more 53 categories. Also because of the range
# of years, it is acceptable to have less precision.

#' Rounding years into decade
#'
#' @param value The year you want to round
#'
#' @return The rounded decade
#' @export

floor_decade <- function(value){ return(value - value %% 10) }

#Apply floor_decade to year built and year remolded
YearBuilt <-lapply(I2$YearBuilt,floor_decade)
I2$YearBuilt <- do.call(rbind, YearBuilt)

YearRemodAdd <-lapply(I2$YearRemodAdd,floor_decade)
I2$YearRemodAdd <- do.call(rbind, YearRemodAdd)

#Convert MSSubclass to factor
I2$MSSubClass <- as.factor(I2$MSSubClass)

#Convert all characters to factors
I2 <- I2 %>%
  mutate_if(sapply(I2,is.character), as.factor)
#Create third copy of dataframe
I3 <- I2

#Convert factors into Ordinal using the info.txt as reference----
#Create ordinal factors and used factor function in console to check if the values were changed
I3$LotShape <- factor(I3$LotShape,levels= c("IR3", "IR2", "IR1", "Reg"),ordered = TRUE)

I3$LandContour <- factor(I3$LandContour,levels= c("Low", "Lvl", "Bnk" ,"HLS"),ordered = TRUE)

I3$Utilities <- factor(I3$Utilities,levels= c("NoSeWa","AllPub"),ordered = TRUE)

I3$LandSlope <- factor(I3$LandSlope,levels= c("Gtl", "Mod", "Sev"),ordered = TRUE)

I3$ExterQual <- factor(I3$ExterQual,levels= c("Fa","TA","Gd","Ex"),ordered = TRUE)

I3$ExterCond <- factor(I3$ExterCond,levels= c("Po", "Fa","TA","Gd","Ex"),ordered = TRUE)

I3$BsmtQual <- factor(I3$BsmtQual,levels= c("None","Po", "Fa","TA","Gd"),ordered = TRUE)

I3$BsmtCond <- factor(I3$BsmtCond,levels= c("None","Po", "Fa","TA","Gd"),ordered = TRUE)

I3$BsmtExposure <- factor(I3$BsmtExposure,levels= c("None","No", "Mn","Av","Gd"),ordered = TRUE)

I3$BsmtFinType1 <- factor(I3$BsmtFinType1,levels= c("None","Unf", "LwQ","Rec","BLQ","ALQ","GLQ"),ordered = TRUE)

I3$BsmtFinType2 <- factor(I3$BsmtFinType2,levels= c("None","Unf", "LwQ","Rec","BLQ","ALQ","GLQ"),ordered = TRUE)

I3$HeatingQC <- factor(I3$HeatingQC,levels= c("Po","Fa", "TA","Gd","Ex"),ordered = TRUE)

I3$KitchenQual <- factor(I3$KitchenQual,levels= c("Fa", "TA","Gd","Ex"),ordered = TRUE)

I3$FireplaceQu <- factor(I3$FireplaceQu,levels= c("None","Po","Fa", "TA","Gd","Ex"),ordered = TRUE)

I3$GarageCond <- factor(I3$GarageCond,levels= c("None","Po","Fa", "TA","Gd","Ex"),ordered = TRUE)

I3$PoolQC <- factor(I3$PoolQC,levels= c("None","Fa","Gd","Ex"),ordered = TRUE)

#Convert some numeric factors that are factors, for example MSSubClass
I2$MSSubClass <- as.factor(I2$MSSubClass)
I3$OverallCond <- factor(I3$OverallCond,ordered = TRUE)

I3$OverallQual <- factor(I3$OverallQual,ordered = TRUE)

I3$MoSold <- factor(I3$MoSold)

I3$YrSold <- factor(I3$YrSold)

I3$YearBuilt <- factor(I3$YearBuilt)

I3$YearRemodAdd <- factor(I3$YearRemodAdd)
#Remove rows with missing data

I4 <- I3[!(rowSums(is.na(I3)) > 0),]



#Create small data frame that shows the type for each variable, as it is difficult to keep track of them.
a <- data.frame(lapply(I4,class))
#Create correlation matrix to see which variables have weak or no correlation to Sale Price and plot the matrix
correlationMatrix <- cor(I4[,c(3:4,26,34,36:38,43:52,54,56,59,61:62,66:71,75,80)])
corrplot(correlationMatrix)
#Remove weak correlation attributes
I5 <- I4[,-c(3:4,26,34,36:37,44:45,47:48,50:52,66:71,75)]

#Double check the remaining varaibles.
b <- data.frame(lapply(I5,class))
correlationMatrix2 <-cor(I5[,c(32,37:39,41,43,46,48:49,60)])
corrplot(correlationMatrix2)

#Create new data frame with strong correlating numeric varaibles,
#and factors that does not have a 75% majority of one factor
New_data <- select(I5,MSSubClass,LotShape,Neighborhood,LotConfig,OverallCond,OverallQual,
                   Exterior1st,Exterior2nd,MasVnrType,ExterQual,Foundation,BsmtQual,
                   BsmtExposure,BsmtFinType1,HeatingQC,KitchenQual,FireplaceQu,GarageType,
                   GarageFinish,YearBuilt,YearRemodAdd)
New_data <- cbind(New_data, I5[,c(32,37:39,41,43,46,48:49,60)])


I6 <- New_data


#Random Forest

#set random seed
set.seed(123456)
# create a training data with 75% of the sample----
sample_size <- floor(0.75 * nrow(I6))
train_ind <- sample(seq_len(nrow(I6)), size = sample_size)
pred_train <- I6[train_ind, ]
pred_test <- I6[-train_ind, ]

#Apply random forest model using training set
model <- randomForest(SalePrice~.,data=pred_train)
plot(model)
predtest <- predict(model, pred_test, type = "class")

all.equal(predtest, pred_test$SalePrice)

plot(pred_test$SalePrice, predtest, xlab="y", ylab=expression(hat(y)))





