set.seed(278) # random number will generate from 278
RandomNumbers <- sort(sample.int(100, 70))
RandomNumbers

install.packages("car")
library(car)
data<-read.csv("D:/kelaniya/STAT/Year3/STAT31513/assignment/Customers Satisfaction.csv")
summary(data)
#head of data set
head(data)


#number of rows of data set
nrow(data)

#check for missing values in the data set
num_missing_values <- sum(is.na(data))

#print the number of missing values

print(num_missing_values)


#duplicate data set
  data1 <- data 
  
  
##############################replacing missing values using mean of each column##################################
              data1$ProdQual[is.na(data1$ProdQual)] <- mean(data1$ProdQual, na.rm = TRUE)
              data1$Ecom[is.na(data1$Ecom)] <- mean(data1$Ecom, na.rm = TRUE) 
              data1$TechSup[is.na(data1$TechSup)] <- mean(data1$TechSup, na.rm = TRUE) 
              
              data1$Advertising[is.na(data1$Advertising)] <- mean(data1$Advertising, na.rm = TRUE)
              
              data1$ProdLine[is.na(data1$ProdLine)] <- mean(data1$ProdLine, na.rm = TRUE)
              
              data1$CompRes[is.na(data1$CompRes)] <- mean(data1$CompRes, na.rm = TRUE)
              
              data1$SalesFImage[is.na(data1$SalesFImage)] <- mean(data1$SalesFImage, na.rm = TRUE)
              
              data1$ComPricing[is.na(data1$ComPricing)] <- mean(data1$ComPricing, na.rm = TRUE)
              
              data1$WartyClaim[is.na(data1$WartyClaim)] <- mean(data1$WartyClaim, na.rm = TRUE)
              
              data1$OrdBilling[is.na(data1$OrdBilling)] <- mean(data1$OrdBilling, na.rm = TRUE)
              
              data1$DelSpeed[is.na( data1$DelSpeed)] <- mean( data1$DelSpeed, na.rm = TRUE)
              
              data1$Satisfaction[is.na( data1$Satisfaction)] <- mean( data1$Satisfaction, na.rm = TRUE)
              
              num_missing_values <- sum(is.na(data1))
              print(num_missing_values)
 
  ###################################identify outliers drawing box plots#########################################
          boxplot(  data1$ProdQual, main = "ProdQual", sub = paste("Outlier :", boxplot.stats(  data1$ProdQual)$out))
  
          boxplot(  data1$Ecom, main = "Ecom", sub = paste("Outlier :", boxplot.stats(  data1$Ecom)$out))  
  
          boxplot(  data1$Advertising, main = "Advertising", sub = paste("Outlier :", boxplot.stats(  data1$Advertising)$out))
              
          boxplot(  data1$ProdLine, main = "ProdL", sub = paste("Outlier :", boxplot.stats(  data1$ProdLine)$out))
          
          boxplot(  data1$CompRes, main = "CompRes", sub = paste("Outlier :", boxplot.stats(  data1$CompRes)$out))
          
          boxplot(  data1$SalesFImage, main = "SalesFImage", sub = paste("Outlier :", boxplot.stats(  data1$SalesFImage)$out))
          
          boxplot(  data1$ComPricing, main = "ComPricing", sub = paste("Outlier :", boxplot.stats(  data1$ComPricing)$out))
          
          boxplot(  data1$WartyClaim, main = "WartyClaim", sub = paste("Outlier :", boxplot.stats(  data1$WartyClaim)$out))
          
          boxplot(  data1$OrdBilling, main = "OrdBilling", sub = paste("Outlier :", boxplot.stats(  data1$OrdBilling)$out))
          
          boxplot(  data1$DelSpeed, main = "DelSpeed", sub = paste("Outlier :", boxplot.stats(  data1$DelSpeed)$out))
          

 
          boxplot(data1[,-1])

######################################################calculate inter quartiles ranges and upper and lower outliers####################################

          summary(data1$ProdQual)
          IQR_ProdQual=9.175-6.700
          Upfen_ProdQual=9.175+1.5*IQR_ProdQual
          Upfen_ProdQual
          lowfen_ProdQual=6.700-1.5*IQR_ProdQual
          lowfen_ProdQual
          
          summary(data1$Ecom)
          IQR_Ecom=3.900-3.200
          Upfen_Ecom=3.900+1.5*IQR_Ecom
          Upfen_Ecom
          lowfen_Ecom=3.200-1.5*IQR_Ecom
          lowfen_Ecom
          
          summary(data1$Advertising)
          IQR_Advertising=4.875-3.050
          Upfen_Advertising=4.875+1.5*IQR_Advertising
          Upfen_Advertising
          lowfen_Advertising=3.050-1.5*IQR_Advertising
          lowfen_Advertising
          
          
          summary(data1$SalesFImage)
          IQR_SalesFImage=5.650-4.500
          Upfen_SalesFImage=5.650+1.5*IQR_SalesFImage
          Upfen_SalesFImage
          lowfen_SalesFImage=4.500-1.5*IQR_SalesFImage
          lowfen_SalesFImage
          
          summary(data1$WartyClaim)
          IQR_WartyClaim=6.700-5.425
          Upfen_WartyClaim=6.700+1.5*IQR_WartyClaim
          Upfen_WartyClaim
          lowfen_WartyClaim=5.425-1.5*IQR_WartyClaim
          lowfen_WartyClaim
          
          summary(data1$OrdBilling)
          IQR_OrdBilling=4.875-3.825
          Upfen_OrdBilling=4.875+1.5*IQR_OrdBilling
          Upfen_OrdBilling
          lowfen_OderBilling=3.825-1.5*IQR_OrdBilling
          lowfen_OderBilling
          
          summary(data1$DelSpeed)
          IQR_DelSpeed=4.50-3.5
          Upfen_DelSpeed=4.5+1.5*IQR_DelSpeed
          Upfen_DelSpeed
          lowfen_DelSpeed=3.5-1.5*IQR_DelSpeed
          lowfen_DelSpeed
          
          summary(data1$Satisfaction)
          IQR_Satisfaction=7.60-6.15
          Upfen_Satisfaction=7.60+1.5*IQR_Satisfaction
          Upfen_Satisfaction
          lowfen_Satisfaction=6.15-1.5*IQR_Satisfaction
          lowfen_Satisfaction
  
#remove outliers from given data set
  data_cleaned = data1[data1$ProdQual<12.8875 & data1$Ecom<4.95 & data1$Advertising<7.6125 & data1$SalesFImage<7 & data1$WartyClaim<8.6125 & data1$OrdBilling<6.45 & data1$DelSpeed<6 & data1$Satisfaction<9.775,]
  data_cleaned1 =  data_cleaned[ data_cleaned$ProdQual>2.9875 &  data_cleaned$Ecom>2.15 &  data_cleaned$Advertising>0.3125 &  data_cleaned$SalesFImage>3 &  data_cleaned$WartyClaim>3.5125 &  data_cleaned$OrdBilling>3 &  data_cleaned$DelSpeed>2 & data_cleaned$Satisfaction>3.975,]
  boxplot(data_cleaned1[,-1])
  
 data_cleaned1
  
 
 library(leaps)
 regfit_full <- regsubsets(Satisfaction ~.,data = (data_cleaned1[,-1]),nvmax = 11)
 reg_summary <- summary(regfit_full)  
 reg_summary
 
 names(reg_summary)
 reg_summary$rsq 
 reg_summary$adjr2
 
 
 
 # Set up a 2x2 grid so we can look at 4 plots at once
 par(mfrow = c(2,2))
 plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
 plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
 
 # We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
 # The which.max() function can be used to identify the location of the maximum point of a vector
 adj_r2_max = which.max(reg_summary$adjr2)
 
 # The points() command works like the plot() command, except that it puts points 
 # on a plot that has already been created instead of creating a new plot
 points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
 
 # We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
 plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
 cp_min = which.min(reg_summary$cp) # 10
 points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
 
 plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
 bic_min = which.min(reg_summary$bic) # 6
 points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)
 
 data_cleaned1_frame(Adj_R2=(reg_summary$adjr2),
                     CP=(reg_summary$cp),BIC=(reg_summary$bic),RSS=reg_summary$rss)
 
 coef(regfit_full,bic_min)
 coef(regfit_full,cp_min)
 coef(regfit_full,adj_r2_max) 
 
 get_model_formula <- function(id, object, outcome)
   
   get_model_formula(13, regfit_full, "Satisfaction")
 bestbic<-lm(Satisfaction~ProdQual+Ecom+SalesFImage+OrdBilling+DelSpeed,data=data_cleaned1)
 summary(bestbic)

 plot(bestbic) 
 
 
 library(MASS)

 # Fit the full model 
 fullmodel <- lm(Satisfaction ~ ., data = (data_cleaned1[,-1]))
 # Stepwise regression model
 stepmodel <- stepAIC(fullmodel, direction = "both", 
                       trace = FALSE)
 summary(stepmodel)
 plot(stepmodel)
 
 vif(stepmodel)
 
 cor(data_cleaned1[,-1])
 (bestbic)
 anova(bestbic)
 
 

 
 