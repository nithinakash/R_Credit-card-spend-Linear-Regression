
setwd('C:/Users/Nithin/Downloads/R - Linear Regression case study')

#**************************************************************************************************************#

#########################
#-->Required Packages<--#
#########################
require(dplyr)
require(stringr)
require(fastDummies)
require(ggplot2)
require(caret)
require(car)
require(Metrics)
require(MLmetrics)
require(sqldf)
require(lubridate)

#**************************************************************************************************************#

################
#-->Datasets<--#
################

cust <- readxl::read_xlsx('Linear Regression Case.xlsx',sheet = 'customer_dbase')

#**************************************************************************************************************#

#################
#-->Data Prep<--#
#################

str(cust)

cust$custid <- NULL
cust$birthmonth <- NULL

#**************************************************************************************************************#

#############
#--> UDF <--#
#############

#cont_var_summary
cont_var_summary <- function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, var=var, range=range, pctl=pctl))
}

#outlier_treatment
outlier_treatment <- function(x){
  UC = quantile(x, p=0.99, na.rm=T)
  LC = quantile(x, p=0.01, na.rm=T)
  x = ifelse(x>UC, UC, x)
  x = ifelse(x<LC, LC, x)
  return(x)
}

#missing_value_treatment continuous
missing_value_treatment = function(x){
  x[is.na(x)] = mean(x, na.rm=T)
  return(x)
}

#mode for categorical
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#missing_value_treatment categorical
missing_value_treatment_categorical <- function(x){
  x[is.na(x)] <- Mode(na.omit(x))
  return(x)
}

#*#**************************************************************************************************************#

#######################
#-->Data Treatment <--#
#######################

cont_col <- c(colnames(select_if(cust,is.numeric)))
cat_col <- colnames(select_if(cust,is.character))

cust_cont <- cust[,cont_col]
cust_cat <- cust[,cat_col]

names(cust_cat)

#Outlier Treatment & Missing Value treatment for continuous variables

num_sum <- data.frame(t(round(apply(cust_cont,2,cont_var_summary),2)))

cust_cont <- data.frame(apply(cust_cont,2,outlier_treatment))
cust_cont <- data.frame(apply(cust_cont,2,missing_value_treatment))

#Mode Treatment for categorical variables

#cust_cat <- data.frame(apply(cust_cat,2,missing_value_treatment_categorical))

#*#**************************************************************************************************************#

##########################
#--> Dummies Creation <--#
##########################

#cust_cat <- fastDummies::dummy_cols(cust_cat,remove_first_dummy = TRUE)

#cust_cat <- select(cust_cat,-cat_col)

cust_clean <- cust_cont

cust_clean['Total_spend'] <- cust$cardspent + cust$card2spent

cust_clean$cardspent <- NULL
cust_clean$card2spent <- NULL

#*#**************************************************************************************************************#

#####################
#--> ASSUMPTIONS <--#
#####################

#target should be ND

ggplot(cust_clean) + aes(Total_spend) + geom_histogram(bins = 10,fill = 'blue',color = 'white')

#To normalise we take Log

cust_clean['ln_Total_spend'] <- log(cust_clean$Total_spend)

ggplot(cust_clean) + aes(ln_Total_spend) + geom_histogram(bins = 10,fill = 'blue',color = 'white')

#Corelation Between x & y ,x & x variables

corel_matrix <- data.frame(round(cor(cust_clean),2))

#lot of vcariables has high corelation so to be dropped

#*#**************************************************************************************************************#


###########################
#--> Feature Reduction <--#
###########################

feat <- data.matrix(select(cust_clean,-Total_spend))
target <- data.matrix(select(cust_clean,Total_spend))

set.seed(12345)

#--> Stepwise <--#

#Full & Empty model
m_full <- lm(Total_spend~.,data = cust_clean)
m_null <- lm(Total_spend~1,data = cust_clean)

stepwise_feat <- step(m_null,scope = list(upper = m_full),data = cust_clean, direction = 'both')

# ln_Total_spend + income + card + lninc + card2items + 
# carditems + cardtype + inccat + card2fee + pets_dogs + voice + 
# addresscat + ownpc + owngame + owncd + hometype + vote + 
# union + pets_saltfish + tollfree + creddebt + age + cardtenure + 
# edcat

#--> RFE <--#

rfe_model <- caret::rfe(feat, target, size = c(1:129), rfeControl=rfeControl(functions = lmFuncs))
#size - No. of columns in features
#Warning in RFE is normal

rfe_feat <- update(rfe_model,feat,target,size = 25)
rfe_feat[["bestVar"]]

#"ln_Total_spend"+"lnequipmon"+"equip"+"wireless"+"lninc"
#"lnwiremon"+"owntv"+"tollfree"+"lntollmon"+"marital"
#"lncardmon"+"lnlongmon"+"callcard"+"owncd"+"commutecat"    
#"voice"+"pets_saltfish"+"card2fee"+"response_03"+"owngame"
#"pets_dogs"+"spousedcat"+"retire"+"union"+"card"  

feat_selected <- c(
  'ln_Total_spend' , 'income' , 'card' , 'lninc' , 'card2items' , 
  'carditems' , 'cardtype' , 'inccat' , 'card2fee' , 'pets_dogs' , 'voice' , 
  'addresscat' , 'ownpc' , 'owngame' , 'owncd' , 'hometype' , 'vote' , 
  'union' , 'pets_saltfish' , 'tollfree' , 'creddebt' , 'age' , 'cardtenure' , 
  'edcat',
  
  "ln_Total_spend","lnequipmon","equip","wireless","lninc",
  "lnwiremon","owntv","tollfree","lntollmon","marital",
  "lncardmon","lnlongmon","callcard","owncd","commutecat",    
  "voice","pets_saltfish","card2fee","response_03","owngame",
  "pets_dogs","spousedcat","retire","union","card" ,

  'Total_spend'
)

feat_selected <- feat_selected[!duplicated(feat_selected)]

cust_clean_selected <- cust_clean[,feat_selected]

#--> LASSO <--#
lasso = train(Total_spend~.,
              data=cust_clean_selected,method='glmnet',
              trControl = trainControl(method="none"),
              tuneGrid=expand.grid(alpha=1,lambda=0.05))

coef(lasso$finalModel, s = lasso$bestTune$lambda)

lasso_rej <- c('wireless','lntollmon','spousedcat')

cust_clean_selected <- dplyr::select(cust_clean_selected,-lasso_rej)

#--> VIF <--#
m_full <- lm(Total_spend~.,data = cust_clean_selected)
vif(m_full)

#*#**************************************************************************************************************#

########################
#--> Data Splitting <--#
########################

samp <- sample(1:nrow(cust_clean_selected), floor(nrow(cust_clean_selected)*0.7))

dev <-cust_clean_selected[samp,]
val <-cust_clean_selected[-samp,]

#*#**************************************************************************************************************#

########################
#--> Model Building <--#
########################

M0 <- lm(Total_spend~ln_Total_spend+
           income+
           card+
           lninc+
           card2items+
           carditems+
           cardtype+
           inccat+
           card2fee+
           pets_dogs+
           addresscat+
           owngame+
           owncd+
           age+
           cardtenure
           ,data = dev)

summary(M0)

#--> Columns Removed <--#
#commutecat
#edcat
#callcard
#retire
#creddebt
#lnequipmon
#lnwiremon
#marital
#lncardmon
#lnlongmon
#owntv
#hometype
#vote
#equip
#union
#tollfree
#response_03
#voice
#pets_saltfish
#ownpc

dev <- data.frame(cbind(dev,pred = predict(M0)))
val <- data.frame(cbind(val,pred = predict(M0, newdata = val)))

#*#**************************************************************************************************************#

#*#####################
#--> Model Scoring <--#
#######################

#--> MAPE <--#
mape(dev$Total_spend,dev$pred)
mape(val$Total_spend,val$pred)

#--> RMSE <--#
rmse(dev$Total_spend,dev$pred)
rmse(val$Total_spend,val$pred)

#--> R^2 <--#
MLmetrics::R2_Score(dev$pred,dev$Total_spend)
MLmetrics::R2_Score(val$pred,val$Total_spend)

#*#*#**************************************************************************************************************#

#*#######################
#--> Cook's Distance <--#
#########################

#To Reduce Error

cd <- cooks.distance(M0)

plot(cd,pch = '*',cex = 2,main = 'Influencers')
abline(h = 4/nrow(dev),col = 'red')

#Remove Influential outliers
influerncers <- as.numeric(names(cd)[cd>(4/nrow(dev))])

dev2 <- dev[-influerncers,]
dev2$pred <- NULL
val$pred <- NULL

M1 <- lm(Total_spend~ln_Total_spend+
           income+
           card+
           lninc+
           card2items+
           carditems+
           cardtype+
           inccat+
           card2fee+
           pets_dogs+
           addresscat+
           owngame+
           owncd+
           age+
           cardtenure
         ,data = dev2)

summary(M1)

dev2 <- data.frame(cbind(dev2,pred = predict(M1)))
val2 <- data.frame(cbind(val,pred = predict(M1, newdata = val)))

#*#**************************************************************************************************************#

#*#####################
#--> Model Scoring <--#
#######################

#--> MAPE <--#
mape(dev2$Total_spend,dev2$pred)
mape(val2$Total_spend,val2$pred)

#--> RMSE <--#
rmse(dev2$Total_spend,dev2$pred)
rmse(val2$Total_spend,val2$pred)

#--> R^2 <--#
MLmetrics::R2_Score(dev2$pred,dev2$Total_spend)
MLmetrics::R2_Score(val2$pred,val2$Total_spend)

#*#*#**************************************************************************************************************#

#*#######################
#--> Decile Analysis <--#
#########################

dev2.1 <- dev2

#Deciles
dec <- quantile(dev2.1$pred,probs = seq(0.1,0.9,by=0.1))

#intervals
dev2.1$decile <- findInterval(dev2.1$pred,c(-Inf,dec,Inf))

#to check deciles
xtabs(~decile,dev2.1)

dev2_1 <- dev2.1[,c("decile","Total_spend","pred")]
colnames(dev2_1) <- c('decile','Total_spend','pred')

dev_dec <- sqldf::sqldf(
  " select decile,
                         count(decile) cnt,
                         avg(pred) as avg_pred_Y,
                         avg(Total_spend) avg
                         from dev2_1
                         group by decile 
                         order by decile")

writexl::write_xlsx(dev_dec,'DA_dev.xlsx')

val2.1 <- val2

#Deciles
dec <- quantile(val2.1$pred,probs = seq(0.1,0.9,by=0.1))

#intervals
val2.1$decile <- findInterval(val2.1$pred,c(-Inf,dec,Inf))

#to check deciles
xtabs(~decile,val2.1)

val2_1 <- val2.1[,c("decile","Total_spend","pred")]
colnames(val2_1) <- c('decile','Total_spend','pred')

val_dec <- sqldf::sqldf(
  " select decile,
                         count(decile) cnt,
                         avg(pred) as avg_pred_Y,
                         avg(Total_spend) avg
                         from val2_1
                         group by decile 
                         order by decile")                      

writexl::write_xlsx(val_dec,'DA_val.xlsx')

#*#*#**************************************************************************************************************#

#*#########################
#--> Model Diagnostics <--#
###########################

coefficients(M1) # model coefficients
confint(M1, level=0.95) # CIs for model parameters 
fitted(M1) # predicted values
residuals(M1) # residuals
anova(M1) # anova table 
vcov(M1) # covariance matrix for model parameters 
influence(M1) # regression diagnostics

#*#*#**************************************************************************************************************#