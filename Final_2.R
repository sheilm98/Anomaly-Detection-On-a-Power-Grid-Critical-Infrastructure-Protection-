#########PART 1##################


# Use tidyverse package
library(tidyverse)
library(depmixS4)

# Read data
df <- read.table("TermProjectData.txt", header = TRUE, sep = ",")
head(df)
nrow(df)
#omit the NA values
df <- na.omit(df)

head(df)
nrow(df)

df$Date <- as.POSIXlt(df$Date, format = "%d/%m/%Y")
df$Time <- as.POSIXlt(df$Time, format = "%H:%M:%S")


# Select only the numeric fields for PCA
numeric_fields <- subset(df, select = -c(Date, Time))


# Picking principle component
pca <- prcomp(numeric_fields, scale = TRUE)
pca
summary(pca) #pc1 most variation


# Finding features with most impact
abs(pca$rotation[,1]) #absolute value of pc1 scores

#Global Intensity and Global Active Power are the two most important variables

#########PART 2##################

# Pick time period for the data (Tuesdays 9am to 11am)
observation2 <- subset(df, df$Time >= as.POSIXlt("09:00:00", format = "%H:%M:%S") & df$Time < as.POSIXlt("11:00:00", format = "%H:%M:%S"))
observation2$Day <- weekdays(as.Date(observation2$Date, format = "%d/%m/%Y"))
observation2 <- subset(observation2, observation2$Day == 'Tuesday')


# Scale Global_intensity & Global_active_power
observation2$Global_intensity_scaled <- scale(observation2$Global_intensity, center = TRUE, scale = TRUE)
observation2$Global_active_power_scaled <- scale(observation2$Global_active_power, center = TRUE, scale = TRUE)
head(observation2)

# Choose number of training and testing observation
days_training <- subset(observation2, observation2$Date >= as.POSIXlt("16/12/2006", format = "%d/%m/%Y") & observation2$Date <= as.POSIXlt("28/4/2009", format = "%d/%m/%Y"))
#14856
count(days_training)
days_testing <- subset(observation2, observation2$Date > as.POSIXlt("28/4/2009", format = "%d/%m/%Y") & observation2$Date <= as.POSIXlt("1/12/2009", format = "%d/%m/%Y"))
#3720
count(days_testing)


library(depmix)
set.seed(1)

state_num <- c(4:15)
state_num_len <- length(state_num)

train_LLL <- numeric(state_num_len)
train_bic <- numeric(state_num_len)

test_LLL <- numeric(state_num_len)
test_bic <- numeric(state_num_len)

#Train Model

for (i in 1:length(state_num)){
  writeLines("\n\n\n#########################################")
  writeLines(paste("NUMBER OF STATES:", state_num[i]))  
  test_model<- depmix(response = list(Global_intensity_scaled ~ 1, Global_active_power_scaled ~ 1)
                        , data = days_training, nstates = state_num[i], ntimes = 14856
                        , family = list(gaussian(), gaussian()))
  fit_train_model <- fit(test_model)
  train_LLL[i] = logLik(fit_train_model)
  train_bic[i] = BIC(fit_train_model)
  #logLik(fit_train_model)
  #BIC(fit_train_model)
  print(fit_train_model)
}

plot(state_num, train_LLL, col = "blue", xlab = "Number of states", ylab = "LLL vs BIC")
lines(state_num, train_LLL, col = "blue")
points(state_num, train_bic, col = "red")
lines(state_num, train_bic, col = "red")
abline(h=0)
legend("bottomright", legend = c("LLL", "BIC"), lty = c(1,1), col = c("blue", "red"))


###########################
set.seed(1)
# STATES = 14
model_14 <- depmix(response = list(Global_intensity_scaled ~ 1, Global_active_power_scaled ~ 1)
                  , data = days_training, nstates = 14, ntimes = 14856
                  , family = list(gaussian(), gaussian()))
fit_train_model_14 <- fit(model_14)
train_LLL_14 = logLik(fit_train_model_14)
train_bic_14 = BIC(fit_train_model_14)
print(fit_train_model_14)


#####TEST 14#####
set.seed(1)


test_model_14 <- depmix(response =list(Global_intensity_scaled ~ 1, Global_active_power_scaled ~ 1), 
                        data = days_testing, nstates = 14, 
                        family = list(gaussian(), gaussian()), 
                        ntimes = 3720)

testMod <- setpars(test_model_14, getpars(fit_train_model_14))

fbTest <- forwardbackward(testMod)
train_logLik <- logLik(fit_train_model_14)
test_logLik <- logLik(testMod)

fbTest
train_logLik  
test_logLik


###########################
# STATES = 15
model_15 <- depmix(response = list(Global_intensity_scaled ~ 1, Global_active_power_scaled ~ 1)
                   , data = days_training, nstates = 15, ntimes = 14856
                   , family = list(gaussian(), gaussian()))
fit_train_model_15 <- fit(model_15)
train_LLL_15 = logLik(fit_train_model_15)
train_bic_15 = BIC(fit_train_model_15)
print(fit_train_model_15)

  

#####TEST 15#####  

test_model_15 <- depmix(response =list(Global_intensity_scaled ~ 1, Global_active_power_scaled ~ 1), 
                        data = days_testing, nstates = 15, 
                        family = list(gaussian(), gaussian()), 
                        ntimes = 3720)

testMod1 <- setpars(test_model_15, getpars(fit_train_model_15))

fbTest1 <- forwardbackward(testMod1)
train_logLik1 <- logLik(fit_train_model_15)
test_logLik1 <- logLik(testMod1)

fbTest1
train_logLik1  
test_logLik1

  
##############PART 3##################
#fit_train_model_15
##########Anomaly Data Set 1############
set.seed(1)

anomalyData1 <- read.table("DataWithAnomalies1.txt", header = TRUE, sep = ',')

anomalyData1$Date <- as.POSIXlt(anomalyData1$Date, format = "%d/%m/%Y")
anomalyData1$Time <- as.POSIXlt(anomalyData1$Time, format = "%H:%M:%S")

# Pick time period for the data (Tuesdays 9am to 11am)
obs_ad1 <- subset(anomalyData1, anomalyData1$Time >= as.POSIXlt("09:00:00", format = "%H:%M:%S") 
                       & anomalyData1$Time < as.POSIXlt("11:00:00", format = "%H:%M:%S"))
obs_ad1 <- na.omit(obs_ad1)
obs_ad1$Day <- weekdays(as.Date(obs_ad1$Date, format = "%d/%m/%Y"))
obs_ad1 <- subset(obs_ad1, obs_ad1$Day == 'Tuesday')

# Scale Global_intensity & Global_active_power
obs_ad1$Global_intensity_scaled <- scale(obs_ad1$Global_intensity, center = TRUE, scale = TRUE)
obs_ad1$Global_active_power_scaled <- scale(obs_ad1$Global_active_power, center = TRUE, scale = TRUE)
head(obs_ad1)

#6000
nrow(obs_ad1)


model_anom1 <- depmix(response =list(Global_active_power ~ 1, Global_intensity ~ 1),
                      data = obs_ad1, nstates = 15, 
                      family = list(gaussian(), gaussian())
                      , ntimes = 6000)
newAnom1 <- setpars(model_anom1, getpars(fit_train_model_15))

fbAnom1 <- forwardbackward(newAnom1)
anom1LogLik <-logLik(newAnom1)
anom1LogLik


##########Anomaly Data Set 2############

anomalyData2 <- read.table("DataWithAnomalies2.txt", header = TRUE, sep = ',')


anomalyData2$Date <- as.POSIXlt(anomalyData2$Date, format = "%d/%m/%Y")
anomalyData2$Time <- as.POSIXlt(anomalyData2$Time, format = "%H:%M:%S")

# Pick time period for the data (Tuesdays 9am to 11am)
obs_ad2 <- subset(anomalyData2, anomalyData2$Time >= as.POSIXlt("09:00:00", format = "%H:%M:%S") 
                  & anomalyData2$Time < as.POSIXlt("11:00:00", format = "%H:%M:%S"))

obs_ad2 <- na.omit(obs_ad2)

obs_ad2$Day <- weekdays(as.Date(obs_ad2$Date, format = "%d/%m/%Y"))
obs_ad2 <- subset(obs_ad2, obs_ad2$Day == 'Tuesday')

# Scale Global_intensity & Global_active_power
obs_ad2$Global_intensity_scaled <- scale(obs_ad2$Global_intensity, center = TRUE, scale = TRUE)
obs_ad2$Global_active_power_scaled <- scale(obs_ad2$Global_active_power, center = TRUE, scale = TRUE)

#head(obs_ad2)

#6000
nrow(obs_ad2)


model_anom2 <- depmix(response =list(Global_active_power ~ 1, Global_intensity ~ 1),
                      data = obs_ad2, nstates = 15, 
                      family = list(gaussian(), gaussian())
                      , ntimes = 6000)
newAnom2 <- setpars(model_anom2, getpars(fit_train_model_15))

fbAnom2 <- forwardbackward(newAnom2)
anom2LogLik <-logLik(newAnom2)
anom2LogLik

##########Anomaly Data Set 3############

anomalyData3 <- read.table("DataWithAnomalies3.txt", header = TRUE, sep = ',')
anomalyData3 <- na.omit(anomalyData3)

anomalyData3$Date <- as.POSIXlt(anomalyData3$Date, format = "%d/%m/%Y")
anomalyData3$Time <- as.POSIXlt(anomalyData3$Time, format = "%H:%M:%S")

# Pick time period for the data (Tuesdays 9am to 11am)
obs_ad3 <- subset(anomalyData3, anomalyData3$Time >= as.POSIXlt("09:00:00", format = "%H:%M:%S") 
                  & anomalyData3$Time < as.POSIXlt("11:00:00", format = "%H:%M:%S"))
obs_ad3$Day <- weekdays(as.Date(obs_ad3$Date, format = "%d/%m/%Y"))
obs_ad3 <- subset(obs_ad3, obs_ad3$Day == 'Tuesday')

# Scale Global_intensity & Global_active_power
obs_ad3$Global_intensity_scaled <- scale(obs_ad3$Global_intensity, center = TRUE, scale = TRUE)
obs_ad3$Global_active_power_scaled <- scale(obs_ad3$Global_active_power, center = TRUE, scale = TRUE)

#head(obs_ad3)

#6000
nrow(obs_ad3)


model_anom3 <- depmix(response =list(Global_active_power ~ 1, Global_intensity ~ 1),
                      data = obs_ad3, nstates = 15, 
                      family = list(gaussian(), gaussian())
                      , ntimes = 6000)
newAnom3 <- setpars(model_anom3, getpars(fit_train_model_15))

fbAnom3 <- forwardbackward(newAnom3)
anom3LogLik <-logLik(newAnom3)

anom3LogLik


#============================COMPARING LogLikelihood============================#
sprintf("LOG-LIKELIHOODS")
sprintf("Train: %f", train_logLik)
sprintf("Test: %f", test_logLik)
sprintf("Anomaly1: %f", anom1LogLik)
sprintf("Anomaly2: %f", anom2LogLik)
sprintf("Anomaly3: %f", anom3LogLik)



