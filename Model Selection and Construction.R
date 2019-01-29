### Model Selection and Construction by Allison Emono
##This folder is a model selection and construction project u
## It will encompass data analysis techniques to decide which model is better than the other
##It will also entail a variable selection portion for dimension reduction 
## I do not cover multicolinearity filteration here but will breifly discuss ways to do so 

### I wll cover different ways to handle string/factor variables 
## one way is binarisation (which can become tideous)
## the other is factor dummying which is more straight forward and better datasets with more variables

## Want to model total income 
### They are multiple ways to tackle this problem. 
## We can split the problem to regression of income on only factor and income against numerics and then piece them back together
## OR
## we can do it all together

##Prior to the data, we will need a couple of functions 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

unnormalise <- function(x,prior_set){
  return(max(prior_set)*x+min(prior_set)) 
}

standardise<- function(x){
  return((x - mean(x))/sqrt(var(x)))
}

un_standardise <- function(x, prior_set){
  return((sqrt(var(prior_set))*x+mean(prior_set)))
}

save_prior_min <- function(x){
  z<- min(x)
  y<- max(x)
  q<- rbind(z,y)
  return(q)
}

## String detection function (to binarise certian strings enable compatibility with the computer)
library(stringr)

String_Binarisation  <- function(x,w){
  #x <- is.factor(x)
  N <- length(x)
  #w <- is.factor(w)
  acc <- matrix(nrow = N, ncol = 1)
  for (i in 1:N) {
    
    logic <- str_detect(x[i], w)
    if(logic == TRUE) {acc[i,] = 1}
    else{acc[i,] = 0}
  }
  
  return(acc)
}

## bootstrap algo 

my_Bootstrap <- function(data, N_n){
  N <- nrow(data) ## to make sure you encompass all the data
  boot_index <- round(runif(n = N_n, min = 1, max = N))
  x <- data[boot_index,]
  return(x)
}

##Get data and sneak peek data
data <- dat_acs2015
data <- data[which(data$INCTOT > 0),]
head(data)


data_sample <- my_Bootstrap(data, 25000)
write.csv(data_sample, file = "analyse_income.csv")
write.csv(data, file = "full_data.csv")
## See type of each column so as to gain intuition on faulty variables that can be removed
## consider this a quality check of sorts 
str(data)

##traditional approach first 
## from the look of things, some variables look suspicious, looks like they are being confused as numbers
# when in fact they are factors, some of which have only one level and hence can be down without
## First the suspicious ones
nlevels(as.factor(data$DATANUM)) ## can be removed

library("dplyr")

data_fact <- select_if(data, is.factor)
data_numeric <- select_if(data, is.numeric)

level_collect<- matrix(nrow = length(data_fact), ncol = 1)
index_collect <- matrix(nrow = length(data_fact), ncol = 1)
for (i in 1:length(data_fact)) {
  level_collect[i,] <- nlevels(data_fact[,i])
}
ind_coll <- which(level_collect>1, arr.ind = TRUE)
data_fact<- data_fact[,ind_coll[,1]]
head(data_fact)
## We can see no factor variables have less than one factor, so we are alright for factors

str(data_fact)
##factors with too many levels don't offer much predictive information/power 
## using threshold of 40 levels
ind_coll <- which(level_collect<40, arr.ind = TRUE)
#level_collect<40
data_fact<- data_fact[,ind_coll[,1]]

###############################
##string Binarisation
## this involves a more intimate look over your string variables and picking certain titles
# Based on graphical analysis, variables or certain strings with most significance is selected
sex_bin <- String_Binarisation(data_fact$SEX, "Male")
insurance_cov<- String_Binarisation(data_fact$HCOVANY, "No health insurance coverage")
labour_ones<- String_Binarisation(data_fact$LABFORCE, "Yes in LF")
ones_white <- String_Binarisation(data_fact$RACE, "White")
mixed_ones <- String_Binarisation(data_fact$RACE, "Three or more major races")
black_ones <- String_Binarisation(data_fact$RACE, "Black")
married_or_nah <- String_Binarisation(data$MARST, "Never married/single")
self_ones <- String_Binarisation(data$CLASSWKR, "Self Employed")
work_force_ones <- String_Binarisation(data$CLASSWKR, "Works for Wages")
citizen_or_nah<- String_Binarisation(data_fact$CITIZEN,"Not a citizen")
naturalised_citizen_ones <- String_Binarisation(data_fact$CITIZEN, "Naturalized citizen")
engineers_bin <- String_Binarisation(data_fact$DEGFIELD2,"Engineering")
bio_ones <- String_Binarisation(data_fact$DEGFIELD2, "Biology and Life Sciences")
math_ones <- String_Binarisation(data_fact$DEGFIELD2, "Mathematics and Statistics")
comp_scie_one <- String_Binarisation(data_fact$DEGFIELD2,"Computer and Information Sciences")
nuclear_bio_ones <- String_Binarisation(data_fact$DEGFIELD2,"Nuclear, Industrial Radiology, and Biological Technologies")
liberal_ones <- String_Binarisation(data_fact$DEGFIELD2, "Liberal Arts and Humanities")

#as we can see, this approach can become very tideous very quickly, which is why we can hope the second approach works just fine
data_fat_bin <- data.frame(GenderBin = as.factor(sex_bin),
                      CitizenBin = as.factor(citizen_or_nah), 
                      WorkForceBin = as.factor(work_force_ones), 
                      SelfEmployedBin = as.factor(self_ones),
                      StillMarriedBin = as.factor(married_or_nah), 
                      InsuranceBin = as.factor(insurance_cov), 
                      EngBin = as.factor(engineers_bin), 
                      BioBin = as.factor(bio_ones), 
                      MathBin = as.factor(math_ones), 
                      CompBin = as.factor(comp_scie_one), 
                      NucBin = as.factor(nuclear_bio_ones), 
                      LabourBin = as.factor(labour_ones),
                      LiberalBin = as.factor(liberal_ones), 
                      NaturalisedBin = as.factor(naturalised_citizen_ones), 
                      MixedBin = as.factor(mixed_ones), 
                      BlackBin = as.factor(black_ones), 
                      WhiteBin = as.factor(ones_white)
                      )
head(data_fat_bin)
y_nomred <- standardise(data$INCTOT)
#y_nom_consts<- un_standardise(y_nomred,data$INCTOT) ## check to ensure it is unormalised properly

mod_fact<- lm(y_nomred~., data = cbind(y_nomred,data_fat_bin))
summary(mod_fact)

### THis below is to see the frequency of each backstep regression on bootstrapped data
## due to the large size, regressing the entire data is infeasible
#iter<- 5
#somemat <- matrix(nrow = iter, ncol = 1)
#for(num_samps in 1:iter){
#  data_samp_fact_boot <- my_Bootstrap(cbind(y_nomred,data_fat_bin),25000)
#  reg_in <- lm(data_samp_fact_boot$y_nomred~., data = data_samp_fact_boot)
 # temp_form <- (step(reg_in, trace = 0))
#  somewhat <- formula(temp_form)
  #somemat[num_samps,] <- as.character(somewhat[3])
#}
### but due to the fact the variables are binary, some ouptut could have only one value in samples
##hence this is not recommeded for binarised data unless the fluntuations between values is high

## from summary of first reg model
data_fat_bin<- subset(data_fat_bin, select = -c(CompBin,NucBin))
mod_fact<- lm(y_nomred~ ., data = cbind(y_nomred,data_fat_bin))
summary(mod_fact)

data_fat_bin<- subset(data_fat_bin, select = -c(LiberalBin,EngBin))
mod_fact<- lm(y_nomred~ ., data = cbind(y_nomred,data_fat_bin))
summary(mod_fact)

iter<- 5
somemat <- matrix(nrow = iter, ncol = 1)
for(num_samps in 1:iter){
  data_samp_fact_boot <- my_Bootstrap(cbind(y_nomred,data_fat_bin),5000)
  reg_in <- lm(data_samp_fact_boot$y_nomred~., data = data_samp_fact_boot)
 temp_form <- (step(reg_in, trace = 0))
 somewhat<- formula(temp_form)
somemat[num_samps,] <- as.character(somewhat[3])
}
somemat

##NOW FOR NUMERICAL PORTION
## now collect the columns index which satisfy our column interest
# Using the level_collect to save a bit of memory
level_collect<- matrix(nrow = length(data_numeric), ncol = 1)
for (i in 1:length(data_numeric)) {
  level_collect[i,] <- var(data_numeric[,i])
}

ind_coll_2 <- which(level_collect>0, arr.ind = TRUE)
data_num_dum <- data_numeric[,ind_coll_2[,1]]


## two columns removed, now piece them back 
#but first remove obvious multicolinearity
cor(data_num_dum$INCTOT, data_num_dum$ANCESTR2D) < cor(data_num_dum$INCTOT, data_num_dum$ANCESTR1D)
#more infor contained in ANCESTR1D, hence remove ANCESTR2D
data_num_dum <- subset(data_num_dum, select = -c(ANCESTR2D,INCWAGE,HHINCOME))
head(data_num_dum)

#Normalise all numerical variables 
data_num_lappy_norm <- data.frame(lapply(data_num_dum, FUN = normalize))
head(data_num_lappy_norm)

## now piece back together
data_to_analyse <- cbind(data_num_lappy_norm, data_fat_bin)
head(data_to_analyse)

mod <- lm(data_to_analyse$INCTOT ~., data = data_to_analyse)
summary(mod)

## Now to select variables based on significance
#we shall use stepwise regression for this but dues to the large dataset, this is when bootstrapping might come in handy 
iter<- 5
#step_form_coll <- matrix(nrow = iter, ncol = 1)
for(num_samps in 1:iter){
  data_anal_boot <- my_Bootstrap(data_to_analyse,5000)
  reg_in <- lm(data_anal_boot$INCTOT~., data = data_anal_boot)
  temp_form <- (step(reg_in, trace = 0))
  somewhat<- formula(temp_form)
  somemat[num_samps,] <- (somewhat[3])
}
somemat
# now, we will use an average of the 5 models as our model
data_anal_boot <- my_Bootstrap(data_to_analyse,7000)
reg1<- lm(data_anal_boot$INCTOT~HHWT + GQ + OWNERSHP + OWNCOST + PERNUM + PERWT + FAMSIZE + NCHILD + RELATED + AGE + ANCESTR1D + ANCESTR2 + YRIMMIG + MIGPUMA1 + GenderBin + WorkForceBin + SelfEmployedBin + InsuranceBin + LabourBin + NaturalisedBin, data = data_anal_boot)
reg2<- lm(data_anal_boot$INCTOT~HHWT + GQ + OWNERSHP + RENT + COSTFUEL + ROOMS + PERNUM + PERWT + FAMSIZE + NCHILD + RELATED + AGE + YRIMMIG + GenderBin + WorkForceBin + SelfEmployedBin + InsuranceBin + LabourBin + NaturalisedBin + MixedBin, data = data_anal_boot) 
reg3<- lm(data_anal_boot$INCTOT~HHWT + GQ + OWNERSHP + OWNCOST + RENT + ROOMS + PERNUM + PERWT + FAMSIZE + NCHILD + RELATED + AGE + ANCESTR2 + UHRSWORK + MIGPUMA1 + TRANTIME + GenderBin + CitizenBin + WorkForceBin + SelfEmployedBin + StillMarriedBin + InsuranceBin + BioBin + LabourBin, data = data_anal_boot)
reg4<- lm(data_anal_boot$INCTOT~HHWT + GQ + OWNERSHP + OWNCOST + COSTFUEL + ROOMS + PERNUM + PERWT + FAMSIZE + NCHILD + RELATED + AGE + BPLD + ANCESTR1D + ANCESTR2 + MIGPUMA1 + TRANTIME + GenderBin + WorkForceBin + SelfEmployedBin + StillMarriedBin + InsuranceBin + MathBin + LabourBin + BlackBin, data = data_anal_boot)
reg5<- lm(data_anal_boot$INCTOT~HHWT + GQ + OWNERSHP + OWNCOST + ROOMS + PERNUM + PERWT + FAMSIZE + NCHILD + RELATED + AGE + BPLD + YRIMMIG + MIGPUMA1 + GenderBin + CitizenBin + WorkForceBin + SelfEmployedBin + InsuranceBin + LabourBin + WhiteBin, data = data_anal_boot)

summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)
summary(reg5)

## can also step wise the subsets but I wouldn't recomend it because remember we regressed on a sample of the data

#Test models
y_test <- data_to_analyse$INCTOT[100:200]
y_preserve<- data_to_analyse$INCTOT[100:200]
data_test <- subset(data_to_analyse[100:200,], select = -c(INCTOT))
data_test_input1 <- subset(data_test, select = c(HHWT , GQ , OWNERSHP , OWNCOST , PERNUM , PERWT , FAMSIZE , NCHILD , RELATED , AGE , ANCESTR1D , ANCESTR2 , YRIMMIG , MIGPUMA1 , GenderBin , WorkForceBin , SelfEmployedBin ,InsuranceBin , LabourBin , NaturalisedBin))
data_test_input2 <- subset(data_test, select = c(HHWT , GQ , OWNERSHP , RENT , COSTFUEL , ROOMS , PERNUM , PERWT , FAMSIZE , NCHILD , RELATED , AGE , YRIMMIG , GenderBin , WorkForceBin , SelfEmployedBin , InsuranceBin , LabourBin , NaturalisedBin , MixedBin))
data_test_input3 <- subset(data_test, select = c(HHWT , GQ , OWNERSHP , OWNCOST , RENT , ROOMS , PERNUM , PERWT , FAMSIZE , NCHILD , RELATED , AGE , ANCESTR2 , UHRSWORK , MIGPUMA1 , TRANTIME , GenderBin , CitizenBin , WorkForceBin , SelfEmployedBin , StillMarriedBin , InsuranceBin , BioBin , LabourBin))
data_test_input4 <- subset(data_test, select = c(HHWT , GQ , OWNERSHP , OWNCOST , COSTFUEL , ROOMS , PERNUM , PERWT , FAMSIZE , NCHILD , RELATED , AGE , BPLD , ANCESTR1D , ANCESTR2 , MIGPUMA1 , TRANTIME , GenderBin , WorkForceBin , SelfEmployedBin , StillMarriedBin , InsuranceBin , MathBin , LabourBin , BlackBin))
data_test_input5 <- subset(data_test, select = c(HHWT , GQ , OWNERSHP , OWNCOST , ROOMS , PERNUM , PERWT , FAMSIZE , NCHILD , RELATED , AGE , BPLD , YRIMMIG , MIGPUMA1 , GenderBin , CitizenBin , WorkForceBin , SelfEmployedBin , InsuranceBin , LabourBin , WhiteBin))

reg_test1 <- predict(reg1, newdata = data_test_input1)
reg_test2 <- predict(reg2, newdata = data_test_input2)
reg_test3 <- predict(reg3, newdata = data_test_input3)
reg_test4 <- predict(reg4, newdata = data_test_input4)
reg_test5 <- predict(reg5, newdata = data_test_input5)
avg_reg_mod <- (reg_test1+ reg_test2+ reg_test3+ reg_test4+ reg_test5)/5
 
y_test<- unnormalise(y_test, data$INCTOT)
diff_y_reg1 <- abs(y_test - unnormalise(reg_test1,data$INCTOT))
diff_y_reg2 <- abs(y_test -unnormalise(reg_test2,data$INCTOT))
diff_y_reg3 <- abs(y_test -unnormalise(reg_test3,data$INCTOT))
diff_y_reg4 <- abs(y_test -unnormalise(reg_test4,data$INCTOT))
diff_y_reg5 <- abs(y_test -unnormalise(reg_test5,data$INCTOT))
diff_y_avg_mod <- abs(y_test - unnormalise(avg_reg_mod,data$INCTOT))

unnormalise(reg_test1,data$INCTOT)

hist(diff_y_reg1, breaks = 50)
hist(diff_y_avg_mod, breaks = 50)
### models seem to do well, remove far outlier and replot
stdev_diff_y_reg1<- sqrt(var(diff_y_reg1))
mean_diff_y_reg1<- mean(diff_y_reg1)

stdev_diff_y_reg2<- sqrt(var(diff_y_reg2))
mean_diff_y_reg2<- mean(diff_y_reg2)

stdev_diff_y_reg3<- sqrt(var(diff_y_reg3))
mean_diff_y_reg3<- mean(diff_y_reg3)

stdev_diff_y_reg4<- sqrt(var(diff_y_reg4))
mean_diff_y_reg4<- mean(diff_y_reg4)

stdev_diff_y_reg5<- sqrt(var(diff_y_reg5))
mean_diff_y_reg5<- mean(diff_y_reg5)

stdev_diff_y_avg <- sqrt(var(diff_y_avg_mod))
mean_diff_y_avg <- mean(diff_y_avg_mod)

diff_reg1_mod_clip <- diff_y_reg1[which(diff_y_reg1 < (mean_diff_y_reg1+2*stdev_diff_y_reg1))]
diff_reg2_mod_clip <- diff_y_reg2[which(diff_y_reg2 < (mean_diff_y_reg2+2*stdev_diff_y_reg2))]
diff_reg3_mod_clip <- diff_y_reg3[which(diff_y_reg3 < (mean_diff_y_reg3+2*stdev_diff_y_reg3))]
diff_reg4_mod_clip <- diff_y_reg4[which(diff_y_reg4 < (mean_diff_y_reg4+2*stdev_diff_y_reg4))]
diff_reg5_mod_clip <- diff_y_reg5[which(diff_y_reg5 < (mean_diff_y_reg5+2*stdev_diff_y_reg5))]
diff_avg_mod_clip <- diff_y_avg_mod[which(diff_y_avg_mod < (mean_diff_y_avg+2*stdev_diff_y_avg))]

hist(diff_reg1_mod_clip, breaks = 50)
hist(diff_reg2_mod_clip, breaks = 50)
hist(diff_reg3_mod_clip, breaks = 50)
hist(diff_reg4_mod_clip, breaks = 50)
hist(diff_reg5_mod_clip, breaks = 50)
hist(diff_avg_mod_clip, breaks = 50)
##at end, modularise code and make it return predict with error rate of model

# the results aren't great but it could be worse considering the data we got
## check to see if avg of all 5 models is statisically better than the others 
## although 4 looks like the best one

test <- t.test(diff_avg_mod_clip,diff_reg4_mod_clip, alternative = "greater")
test$p.value
## p_value supports the null hypothesis which dictates the average of all 5 models perfomes better than the others
## in hinde sight, non of these model yield satisfactory results in testing 
## despite a low residual error of 17%


##Lets try a neural network
install.packages("neuralnetwork")
library(neuralnet)

str(data_anal_boot)
data_anal_boot$GenderBin<- as.numeric(data_anal_boot$GenderBin)
data_anal_boot$CitizenBin<- as.numeric(data_anal_boot$CitizenBin)
data_anal_boot$WorkForceBin<- as.numeric(data_anal_boot$WorkForceBin)
data_anal_boot$SelfEmployedBin<- as.numeric(data_anal_boot$SelfEmployedBin)
data_anal_boot$StillMarriedBin<- as.numeric(data_anal_boot$StillMarriedBin)
data_anal_boot$InsuranceBin<- as.numeric(data_anal_boot$InsuranceBin)
data_anal_boot$BioBin <- as.numeric(data_anal_boot$BioBin)
data_anal_boot$MathBin <- as.numeric(data_anal_boot$MathBin)
data_anal_boot$LabourBin <- as.numeric(data_anal_boot$LabourBin)
data_anal_boot$NaturalisedBin <- as.numeric(data_anal_boot$NaturalisedBin)
data_anal_boot$MixedBin <- as.numeric(data_anal_boot$MixedBin)
data_anal_boot$BlackBin <- as.numeric(data_anal_boot$BlackBin)
data_anal_boot$WhiteBin <- as.numeric(data_anal_boot$WhiteBin)

head(data_anal_boot)
nn_model <- neuralnet(data_anal_boot$INCTOT~HHWT + GQ + OWNERSHP + OWNCOST + ROOMS + PERNUM + PERWT + FAMSIZE + NCHILD + RELATED + AGE + BPLD + YRIMMIG + MIGPUMA1 + GenderBin + CitizenBin + WorkForceBin + SelfEmployedBin + InsuranceBin + LabourBin + WhiteBin, 
                      data = data_anal_boot, hidden = c(20,1), act.fct="logistic", linear.output = FALSE, 
                      threshold = 0.005,
                      stepmax  = 3000,
                      learningrate = 0.05,
                      startweights = NULL)
nn_model$result.matrix[1]

str(data_test_input5)
data_test_input5$GenderBin <- as.numeric(data_test_input5$GenderBin)     
data_test_input5$CitizenBin  <- as.numeric(data_test_input5$CitizenBin)     
data_test_input5$WorkForceBin  <- as.numeric(data_test_input5$WorkForceBin)   
data_test_input5$SelfEmployedBin<- as.numeric(data_test_input5$SelfEmployedBin)  
data_test_input5$InsuranceBin   <- as.numeric(data_test_input5$InsuranceBin)  
data_test_input5$LabourBin    <- as.numeric(data_test_input5$LabourBin)    
data_test_input5$WhiteBin <- as.numeric(data_test_input5$WhiteBin)  

nn_test <- compute(nn_model, data_test_input5)
nn_test$net.result
y_test
nn_perf <- y_test- unnormalise(nn_test$net.result, data$INCTOT) 
diff_abs_nn_perf <- abs(nn_perf)
hist(diff_abs_nn_perf, breaks = 50)

## as you can see visually, the neural network beforms better 
# this can be due to the fact that neural network is more senstitive to the range of the data
# as the activation function must span the range of the output data set
# this resitriction (which can often be a disadvantage) is an advantage in this case
# to comfirm NN is indeed better statistically 

stdev_diff_abs_nn_perf <- as.numeric(sqrt(var(diff_abs_nn_perf)))
mean_diff_abs_nn_perf <- mean(diff_abs_nn_perf)
diff_abs_nn_perf_clip <- diff_abs_nn_perf[which( diff_abs_nn_perf < (mean_diff_abs_nn_perf + 2*stdev_diff_abs_nn_perf))]
hist(diff_abs_nn_perf_clip, breaks = 50)

nn_v_reg_test<- t.test(diff_abs_nn_perf_clip,diff_reg4_mod_clip, alternative = "less")
nn_v_reg_test$p.value

#statistically probability of the null (diff_abs_nn_perf_clip >= diff_reg4_mod_clip) is so small 
## that when we set the test in the reverse sense (diff_abs_nn_perf_clip <= diff_reg4_mod_clip)
# R approximates the p-value to be one
## so visually and statistically, neural network is the superior model for this data set.

## Why did regression perform so bad? well they are a couple of reasons
# 1) MULTICOLLINEARITY; Regression's kryptonite. This will render any superiorly backstep selected model obselete if multicolineary variables are not removed first
#     that is to say, the determinant of the correlation matrix of indep variables should be as far from zero as possible
#     This is why it is essential to remove multicolinear variables before peforming backwards step regression 
# 2) THE range connundrum: Neural network can be more catered to the range of our output variables, whereas regression naturally does not
    # of course they are ways to ensure regressors shift input to insure output stays within the range but this will require further analysis and transformation 
      # mostly around picking specific basis function on the right side of the regression equation




