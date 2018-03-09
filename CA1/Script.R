# ATTRIBUTE TYPE
sapply(BANKINGrel, class)

# PERCENTAGE OF MISSING VALUES
sum(is.na(BANKINGrel$age))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$job))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$marital))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$education))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$housing))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$loan))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$contact))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$month))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$day_of_week))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$duration))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$campaign))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$pdays))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$previous))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$poutcome))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$cons.price.idx))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$cons.conf.idx))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$euribor3m))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$nr.employed))/nrow(BANKINGrel)*100
sum(is.na(BANKINGrel$y))/nrow(BANKINGrel)*100

# MAX
max(BANKINGrel$age,na.rm=TRUE)
max(BANKINGrel$duration,na.rm=TRUE)
max(BANKINGrel$campaign,na.rm=TRUE)
max(BANKINGrel$pdays,na.rm=TRUE)
max(BANKINGrel$previous,na.rm=TRUE)
max(BANKINGrel$cons.price.idx,na.rm=TRUE)
max(BANKINGrel$cons.conf.idx,na.rm=TRUE)
max(BANKINGrel$euribor3m,na.rm=TRUE)
max(BANKINGrel$nr.employed,na.rm=TRUE)

# MIN
min(BANKINGrel$age,na.rm=TRUE)
min(BANKINGrel$duration,na.rm=TRUE)
min(BANKINGrel$campaign,na.rm=TRUE)
min(BANKINGrel$pdays,na.rm=TRUE)
min(BANKINGrel$previous,na.rm=TRUE)
min(BANKINGrel$cons.price.idx,na.rm=TRUE)
min(BANKINGrel$cons.conf.idx,na.rm=TRUE)
min(BANKINGrel$euribor3m,na.rm=TRUE)
min(BANKINGrel$nr.employed,na.rm=TRUE)

# MEAN
mean(BANKINGrel$age,na.rm=TRUE)
mean(BANKINGrel$duration,na.rm=TRUE)
mean(BANKINGrel$campaign,na.rm=TRUE)
mean(BANKINGrel$pdays,na.rm=TRUE)
mean(BANKINGrel$previous,na.rm=TRUE)
mean(BANKINGrel$cons.price.idx,na.rm=TRUE)
mean(BANKINGrel$cons.conf.idx,na.rm=TRUE)
mean(BANKINGrel$euribor3m,na.rm=TRUE)
mean(BANKINGrel$nr.employed,na.rm=TRUE)

# MODE FUNCTION
Mode <- function(BANKINGrel){
  ux <- unique(BANKINGrel)
  return (ux[which.max(tabulate(match(BANKINGrel, ux)))])
}

# MODE
Mode(BANKINGrel$age)
Mode(BANKINGrel$duration)
Mode(BANKINGrel$campaign)
Mode(BANKINGrel$pdays)
Mode(BANKINGrel$previous)
Mode(BANKINGrel$cons.price.idx)
Mode(BANKINGrel$cons.conf.idx)
Mode(BANKINGrel$euribor3m)
Mode(BANKINGrel$nr.employed)

# MEDIAN
median(BANKINGrel$age,na.rm=TRUE)
median(BANKINGrel$duration,na.rm=TRUE)
median(BANKINGrel$campaign,na.rm=TRUE)
median(BANKINGrel$pdays,na.rm=TRUE)
median(BANKINGrel$previous,na.rm=TRUE)
median(BANKINGrel$cons.price.idx,na.rm=TRUE)
median(BANKINGrel$cons.conf.idx,na.rm=TRUE)
median(BANKINGrel$euribor3m,na.rm=TRUE)
median(BANKINGrel$nr.employed,na.rm=TRUE)

# Standard Deviation
sd(BANKINGrel$age,na.rm=TRUE)
sd(BANKINGrel$duration,na.rm=TRUE)
sd(BANKINGrel$campaign,na.rm=TRUE)
sd(BANKINGrel$pdays,na.rm=TRUE)
sd(BANKINGrel$previous,na.rm=TRUE)
sd(BANKINGrel$cons.price.idx,na.rm=TRUE)
sd(BANKINGrel$cons.conf.idx,na.rm=TRUE)
sd(BANKINGrel$euribor3m,na.rm=TRUE)
sd(BANKINGrel$nr.employed,na.rm=TRUE)

# TYPE OF DISTRIBUTION THAT NUMERICS FOLLOW
shapiro.test(BANKINGrel$age) # LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION
shapiro.test(BANKINGrel$duration) #  LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION
shapiro.test(BANKINGrel$campaign) #  LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION
shapiro.test(BANKINGrel$pdays) # LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION
shapiro.test(BANKINGrel$previous) # LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION
shapiro.test(BANKINGrel$cons.price.idx) # LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION
shapiro.test(BANKINGrel$cons.conf.idx) # LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION
shapiro.test(BANKINGrel$euribor3m) # LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION
shapiro.test(BANKINGrel$nr.employed) # LESS THAN 0.05, CAN SAY WITH 95% CONFIDENCE THAT DATA DOESN'T FIT NORMAL DISTRIBUTION

# CORRELATION AGE
cor(BANKINGrel$age, BANKINGrel$duration, use="complete.obs")                  # 0.05092651
cor(BANKINGrel$age, BANKINGrel$campaign, use="complete.obs")                  # 0.009360903
cor(BANKINGrel$age, BANKINGrel$pdays, use="complete.obs")                     # -0.05165584
cor(BANKINGrel$age, BANKINGrel$previous, use="complete.obs")                  # 0.07009161
cor(BANKINGrel$age, BANKINGrel$cons.price.idx, use="complete.obs")            # 0.009423496
cor(BANKINGrel$age, BANKINGrel$cons.conf.idx, use="complete.obs")             # 0.07293301   HIGHEST
cor(BANKINGrel$age, BANKINGrel$euribor3m, use="complete.obs")                 # -0.01758317
cor(BANKINGrel$age, BANKINGrel$nr.employed, use="complete.obs")               # -0.03878907

# CORRELATION DURATION
cor(BANKINGrel$duration, BANKINGrel$age, use="complete.obs")                  # 0.05092651   HIGHEST
cor(BANKINGrel$duration, BANKINGrel$campaign, use="complete.obs")             # -0.1065406
cor(BANKINGrel$duration, BANKINGrel$pdays, use="complete.obs")                # -0.0784778
cor(BANKINGrel$duration, BANKINGrel$previous, use="complete.obs")             # 0.04562788
cor(BANKINGrel$duration, BANKINGrel$cons.price.idx, use="complete.obs")       # 0.02987555
cor(BANKINGrel$duration, BANKINGrel$cons.conf.idx, use="complete.obs")        # -0.02512887
cor(BANKINGrel$duration, BANKINGrel$euribor3m, use="complete.obs")            # -0.04008613
cor(BANKINGrel$duration, BANKINGrel$nr.employed, use="complete.obs")          # -0.06332767

# CORRELATION CAMPAIGN
cor(BANKINGrel$campaign, BANKINGrel$age, use="complete.obs")                  # 0.009360903
cor(BANKINGrel$campaign, BANKINGrel$duration, use="complete.obs")             # -0.1065406
cor(BANKINGrel$campaign, BANKINGrel$pdays, use="complete.obs")                # 0.06938422
cor(BANKINGrel$campaign, BANKINGrel$previous, use="complete.obs")             # -0.09497193
cor(BANKINGrel$campaign, BANKINGrel$cons.price.idx, use="complete.obs")       # 0.1366676
cor(BANKINGrel$campaign, BANKINGrel$cons.conf.idx, use="complete.obs")        # 0.003685311
cor(BANKINGrel$campaign, BANKINGrel$euribor3m, use="complete.obs")            # 0.1674055
cor(BANKINGrel$campaign, BANKINGrel$nr.employed, use="complete.obs")          # 0.1755955    HIGHEST

# CORRELATION PDAYS
cor(BANKINGrel$pdays, BANKINGrel$age, use="complete.obs")                     # -0.05165584
cor(BANKINGrel$pdays, BANKINGrel$duration, use="complete.obs")                # -0.0784778
cor(BANKINGrel$pdays, BANKINGrel$campaign, use="complete.obs")                # 0.06938422
cor(BANKINGrel$pdays, BANKINGrel$previous, use="complete.obs")                # -0.6031517
cor(BANKINGrel$pdays, BANKINGrel$cons.price.idx, use="complete.obs")          # 0.03839727
cor(BANKINGrel$pdays, BANKINGrel$cons.conf.idx, use="complete.obs")           # -0.05826343
cor(BANKINGrel$pdays, BANKINGrel$euribor3m, use="complete.obs")               # 0.2994986
cor(BANKINGrel$pdays, BANKINGrel$nr.employed, use="complete.obs")             # 0.3834322    HIGHEST

# CORRELATION PREVIOUS
cor(BANKINGrel$previous, BANKINGrel$age, use="complete.obs")                  # 0.07009161   HIGHEST
cor(BANKINGrel$previous, BANKINGrel$duration, use="complete.obs")             # 0.04562788
cor(BANKINGrel$previous, BANKINGrel$campaign, use="complete.obs")             # -0.09497193
cor(BANKINGrel$previous, BANKINGrel$pdays, use="complete.obs")                # -0.6031517
cor(BANKINGrel$previous, BANKINGrel$cons.price.idx, use="complete.obs")       # -0.1128567
cor(BANKINGrel$previous, BANKINGrel$cons.conf.idx, use="complete.obs")        # -0.07672111
cor(BANKINGrel$previous, BANKINGrel$euribor3m, use="complete.obs")            # -0.4705007
cor(BANKINGrel$previous, BANKINGrel$nr.employed, use="complete.obs")          # -0.5373021

# CORRELATION CONS.PRICE.IDX
cor(BANKINGrel$cons.price.idx, BANKINGrel$age, use="complete.obs")            # 0.009423496
cor(BANKINGrel$cons.price.idx, BANKINGrel$duration, use="complete.obs")       # 0.02987555
cor(BANKINGrel$cons.price.idx, BANKINGrel$campaign, use="complete.obs")       # 0.1366676    HIGHEST
cor(BANKINGrel$cons.price.idx, BANKINGrel$pdays, use="complete.obs")          # 0.03839727
cor(BANKINGrel$cons.price.idx, BANKINGrel$previous, use="complete.obs")       # -0.1128567
cor(BANKINGrel$cons.price.idx, BANKINGrel$cons.conf.idx, use="complete.obs")  # 0.09847915
cor(BANKINGrel$cons.price.idx, BANKINGrel$euribor3m, use="complete.obs")      # 0.6315915
cor(BANKINGrel$cons.price.idx, BANKINGrel$nr.employed, use="complete.obs")    # 0.4245895

# CORRELATION CONS.CONF.IDX
cor(BANKINGrel$cons.conf.idx, BANKINGrel$age, use="complete.obs")             # 0.07293301
cor(BANKINGrel$cons.conf.idx, BANKINGrel$duration, use="complete.obs")        # -0.02512887
cor(BANKINGrel$cons.conf.idx, BANKINGrel$campaign, use="complete.obs")        # 0.003685311
cor(BANKINGrel$cons.conf.idx, BANKINGrel$pdays, use="complete.obs")           # -0.05826343
cor(BANKINGrel$cons.conf.idx, BANKINGrel$previous, use="complete.obs")        # -0.07672111
cor(BANKINGrel$cons.conf.idx, BANKINGrel$cons.price.idx, use="complete.obs")  # 0.09847915
cor(BANKINGrel$cons.conf.idx, BANKINGrel$euribor3m, use="complete.obs")       # 0.3460003    HIGHEST
cor(BANKINGrel$cons.conf.idx, BANKINGrel$nr.employed, use="complete.obs")     # 0.1739799

# CORRELATION EURIBOR3M
cor(BANKINGrel$euribor3m, BANKINGrel$age, use="complete.obs")                 # -0.01758317
cor(BANKINGrel$euribor3m, BANKINGrel$duration, use="complete.obs")            # -0.04008613
cor(BANKINGrel$euribor3m, BANKINGrel$campaign, use="complete.obs")            # 0.1674055
cor(BANKINGrel$euribor3m, BANKINGrel$pdays, use="complete.obs")               # 0.2994986
cor(BANKINGrel$euribor3m, BANKINGrel$previous, use="complete.obs")            # -0.4705007
cor(BANKINGrel$euribor3m, BANKINGrel$cons.price.idx, use="complete.obs")      # 0.6315915
cor(BANKINGrel$euribor3m, BANKINGrel$cons.conf.idx, use="complete.obs")       # 0.3460003
cor(BANKINGrel$euribor3m, BANKINGrel$nr.employed, use="complete.obs")         # 0.9384368    HIGHEST

# CORRELATION NE.EMPLOYED
cor(BANKINGrel$nr.employed, BANKINGrel$age, use="complete.obs")               # -0.03878907
cor(BANKINGrel$nr.employed, BANKINGrel$duration, use="complete.obs")          # -0.06332767
cor(BANKINGrel$nr.employed, BANKINGrel$campaign, use="complete.obs")          # 0.1755955
cor(BANKINGrel$nr.employed, BANKINGrel$pdays, use="complete.obs")             # 0.3834322
cor(BANKINGrel$nr.employed, BANKINGrel$previous, use="complete.obs")          # -0.5373021
cor(BANKINGrel$nr.employed, BANKINGrel$cons.price.idx, use="complete.obs")    # 0.4245895
cor(BANKINGrel$nr.employed, BANKINGrel$cons.conf.idx, use="complete.obs")     # 0.1739799
cor(BANKINGrel$nr.employed, BANKINGrel$euribor3m, use="complete.obs")         # 0.9384368    HIGHEST

# HISTOGRAM FOR AGE (PREDICTOR) VS Y (TARGET)
ggplot(BANKINGrel, aes(x = age, fill = y)) + geom_histogram()