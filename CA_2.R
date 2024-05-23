##Loading and reading the dataset into dataframe.

data_frame <- read.csv("Dataset_2024.csv")

##Changing all columns to proper names.
colnames(data_frame)[colnames(data_frame) == "Age (years)"] <- "Age"
colnames(data_frame)[colnames(data_frame) == "Body fat (%)"] <- "Body_fat"
colnames(data_frame)[colnames(data_frame) == "Chest circumference (cm)"] <- "Chest_circumference"
colnames(data_frame)[colnames(data_frame) == "Density (g/cm)"] <- "Density"
colnames(data_frame)[colnames(data_frame) == "Knee circumference (cm)"] <- "Knee_circumference"
colnames(data_frame)[colnames(data_frame) == "Weight (lbs)"] <- "Weight"

View(data_frame)

#Viewing the structure of dataset.
str(data_frame)

##Calculating min, median, mean and 3rd quartile.
summary(data_frame$`Age`)
summary(data_frame$`Body_fat`)
summary(data_frame$`Chest_circumference`)
summary(data_frame$`Density`)
summary(data_frame$`Knee_circumference`)
summary(data_frame$`Weight`)


## checking for the linearity of data
library(psych)
windows(20,10)

pairs.panels(data_frame,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


# Examine linearity in more detail using scatter plots

windows(20,12)
par(mfrow= c(4,2))

scatter.smooth(x = data_frame$Body_fat,
               y = data_frame$Age,
               xlab = "Body_fat",
               ylab = "Age", main = "Correlation of Age ~ Body_fat")

scatter.smooth(x = data_frame$Chest_circumference,
               y = data_frame$Age,
               xlab = "Chest_circumference",
               ylab = "Age", main = "Correlation of Age ~ Chest_circumference")


scatter.smooth(x = data_frame$Density,
               y = data_frame$Age,
               main = "Correlation of Age ~ Density",
               xlab = "Density",
               ylab = "Age")

scatter.smooth(x = data_frame$Knee_circumference,
               y = data_frame$Age,
               main = "Correlation of Age ~ Knee_circumference",
               xlab = "Knee_circumference",
               ylab = "Age")

scatter.smooth(x = data_frame$Weight,
               y = data_frame$Age,
               main = "Correlation of Age ~ Weight",
               xlab = "Weight",
               ylab = "Age")


##plotting the box plot

windows(20,16)
par(mfrow = c(3, 2)) # divide graph area in 3 rows by 2 columns
attach(data_frame)

boxplot(Age,
        main = "Age") # box plot for 'Age'

boxplot(Body_fat,
        main = "Body_fat") # box plot for 'Body_fat'

boxplot(Chest_circumference,
        main = "Chest_circumference") # box plot for 'Chest_circumference'

boxplot(Density,
        main = "Density") # box plot for 'Density'

boxplot(Knee_circumference,
        main = "Knee_circumference") # box plot for 'Knee_circumference'

boxplot(Weight,
        main = "Weight") # box plot for 'Weight'


# Examining correlation between Age and Independent variables

windows(20,16)
cor.plot(data_frame)
attach(data_frame)

# Examining the other variables
paste("Correlation for Age and Body_fat: ", round(cor(Age, Body_fat),2))
paste("Correlation for Age and Chest_circumference: ", round(cor(Age, Chest_circumference),2))
paste("Correlation for Age and Density: ", round(cor(Age, Density),2))
paste("Correlation for Age and Knee_circumference: ", round(cor(Age, Knee_circumference),2))
paste("Correlation for Age and Weight: ", round(cor(Age, Weight),2))

install.packages("corrplot")
library(corrplot)

windows(20,18)
cor_matrix <- cor(data_frame[, c("Age", "Body_fat", "Chest_circumference", "Density","Knee_circumference", "Weight")], use = "complete.obs")

Check for normality






# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
windows(30,20)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical


plot(density(data_frame$Age),
     main = "Density plot : Age",
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness : ", round(e1071::skewness(data_frame$Age), 2)))
polygon(density(data_frame$Age), col = "red")


plot(density(data_frame$Body_fat),
     main = "Density plot : Body_fat",
     ylab = "Frequency", xlab = "Body_fat",
     sub = paste("Skewness : ", round(e1071::skewness(data_frame$Body_fat), 2)))
polygon(density(data_frame$Body_fat), col = "red")

plot(density(data_frame$Chest_circumference),
     main = "Density plot : Chest_circumference",
     ylab = "Frequency", xlab = "Chest_circumference",
     sub = paste("Skewness : ", round(e1071::skewness(data_frame$Chest_circumference), 2)))
polygon(density(data_frame$Chest_circumference), col = "red")

plot(density(data_frame$Density),
     main = "Density plot : Density",
     ylab = "Frequency", xlab = "Density",
     sub = paste("Skewness : ", round(e1071::skewness(data_frame$Density), 2)))
polygon(density(data_frame$Density), col = "red")

plot(density(data_frame$Knee_circumference),
     main = "Density plot : Knee_circumference",
     ylab = "Frequency", xlab = "Knee_circumference",
     sub = paste("Skewness : ", round(e1071::skewness(data_frame$Knee_circumference), 2)))
polygon(density(data_frame$Knee_circumference), col = "red")


plot(density(data_frame$Weight),
     main = "Density plot : Weight",
     ylab = "Frequency", xlab = "Weight",
     sub = paste("Skewness : ", round(e1071::skewness(data_frame$Weight), 2)))
polygon(density(data_frame$Weight), col = "red")



# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -.05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0.5 = approx symetric.

paste("Skewness for Age : ", round(e1071::skewness(data_frame$Age), 2))
paste("Skewness for Body_fat : ", round(e1071::skewness(data_frame$Body_fat), 2))
paste("Skewness for Chest_circumference : ", round(e1071::skewness(data_frame$Chest_circumference), 2))
paste("Skewness for Density : ", round(e1071::skewness(data_frame$Density), 2))
paste("Skewness for Knee_circumference : ", round(e1071::skewness(data_frame$Knee_circumference), 2))
paste("Skewness for Weight: ", round(e1071::skewness(data_frame$Weight), 2))


# Check normality of all variables using normality test
shapiro.test(data_frame$Age)
shapiro.test(data_frame$Body_fat) 
shapiro.test(data_frame$Chest_circumference)
shapiro.test(data_frame$Density)
shapiro.test(data_frame$Knee_circumference) 
shapiro.test(data_frame$Weight)


# If p-value < 0.05 then variable is not normally distributed
# Age is normally distributed (p-value = 0.001043)
# Body_fat is not normally distributed (p-value = 0.141)
# Chest_circumference is normally distributed (p-value = 0.0001175)
# Density is not normally distributed (p-value = 0.6571)
# Knee_circumference is normally distributed (p-value =  0.003304)
# Weight is not normally distributed (p-value = 1.709e-08)


##Need to transform which are not normally distributed.
# Need to transform Body_fat, Density and Weight

install.packages("MASS")
library(MASS)
View(data_frame)

attach(data_frame)
windows(20,10)
par(mfrow = c(4,2))
box_cox_transform <- boxcox(Age~Body_fat)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Body_fat <-(Age^lamda-1)/lamda
normalised_Body_fat
hist(normalised_Body_fat)
shapiro.test(normalised_Body_fat)

##Modify the variable 
data_frame$Body_fat_new <- normalised_Body_fat
shapiro.test(data_frame$Body_fat_new)

##For density 
box_cox_transform <- boxcox(Age~Density)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Density <-(Age^lamda-1)/lamda
normalised_Density
hist(normalised_Density)
shapiro.test(normalised_Density)

##Modify the variable 
data_frame$Density_new <- normalised_Density
shapiro.test(data_frame$Density_new)


##For Weight
box_cox_transform <- boxcox(Age~Weight)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Weight <-(Age^lamda-1)/lamda
normalised_Weight
hist(normalised_Weight)
shapiro.test(normalised_Weight)

##Modify the variable 
data_frame$Weight_new <- normalised_Weight
shapiro.test(data_frame$Weight_new)




# check multicollinearity
cor(data_frame)


##Regression model using transformed variables
str(data_frame)
attach(data_frame)
model_1 <- lm( Age ~ 
                 Body_fat + 
                 Chest_circumference +
                 Density + 
                 Knee_circumference + 
                 Weight)

model_1
summary(model_1)


Age ~ -389.99 + 1.16*Body_fat +1.12*Chest_circumference + 315.73*Density + 1.27*Knee_circumference -0.46*Weight

AIC(model_1)

BIC(model_1)

##residuals normally distributed
shapiro.test(residuals(model_1))
##residuals diffrent from zero
t.test(residuals(model_1),mu=0) 


# randomness of residuals (autocorrelation) -- Durbin-Watson test
install.packages("lmtest")
library(lmtest)
dwtest(model_1)

#Check for multi_collinearity
library(faraway)
v1 <-vif(model_1)
v1


##For model 2
##Regression model using transformed variables
str(data_frame)
attach(data_frame)
model_2 <- lm( Age ~ 
                 Body_fat_new + 
                 Chest_circumference +
                 Density_new + 
                 Knee_circumference + 
                 Weight_new)

model_2
summary(model_2)

Age ~ -2.962e+01 + 6.496e+00*Body_fat -7.701e-04*Chest_circumference + 1.275e-02*Knee_circumference

AIC(model_2)

BIC(model_2)


##residuals normally distributed for model_2
shapiro.test(residuals(model_2))
##residuals diffrent from zero
t.test(residuals(model_2),mu=0) 

dwtest(model_2)

#Check for multi_collinearity
v2 <-vif(model_2)
v2


##FInal output for model_1 and model_2
AIC(model_1)
BIC(model_1)

AIC(model_2)
BIC(model_2)

##Among these two models model_2 is the best one and accurate.

##Saving the data frame in form of RDS.
saveRDS(data_frame, "data_frame.rds")
