###############################################
## Survival analysis
## data challenge 1 dsc
## September 09 2021
###############################################
# install.packages("rjson")
# install.packages("lme4")
# install.packages("survival")
# install.packages("survminer")
## install.packages("ggfortify")
library(lme4)
library(survival)
library(survminer)
library(ggfortify)

##~~~~~~~~~~~~~~~~~~~~~~~
## loading data
##~~~~~~~~~~~~~~~~~~~~~~~

##
## Mevluet data
##
d <- read.csv("mevluet_data_merged.csv", header = T)
summary(d)
dim(d)
colnames(d) ## these are all the variables we can work with

## all company names
comp_names <- unique(d$Company.Name)
length(comp_names)
## all activity sectors
sector_vec <- unique(d$Sector)
length(sector_vec)
sector_vec


##
## the survival datasets
##
d_surv <- read.csv("companies_filling_minimal.csv", header = T)
summary(d_surv)
dim(d_surv)

## reformatting dates
years <- format(as.character(d_surv$most_recent_filling), format = "%y%m%d")
head(years)
summary(years)
min(years)
max(years)

## censorship
status <- numeric(length(d_surv$most_recent_filling))
status[years != max(years)] <- 1
summary(status)
head(status)

## the survival dataset
time <- difftime(years, min(years), units = "weeks")
head(time)
d_surv$time <- time
d_surv$years <- years
d_surv$status <- status

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## company survival analyses
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

d_surv <- read.csv("companies_filling_minimal.csv", header = T)
summary(d_surv)
dim(d_surv)

## kaplan meier
km_fit <- survfit(Surv(time, status) ~ 1, data = d_surv)
plot(km_fit, xlab = "Time (in weeks after the first log", ylab = "proportion of compagny still logging")
crisis_start <- difftime(format("2007-12-01", format = "%y%m%d"), min(years), units = "weeks")
abline(v = as.numeric(crisis_start), col = "red", lwd = 2)
autoplot(km_fit)

## getting company sectors and other variables for this survival dataset
logical_compname <- (as.character(d_surv$Company.Name) %in% as.character(d$Company.Name))
length(logical_compname[logical_compname == T])

ds_all <- d_surv[logical_compname == T, ]
logical_compname_rev <- (as.character(d$Company.Name) %in% as.character(d_surv$Company.Name))
length(logical_compname_rev)
comp_in_surv <- d$Company.Name[logical_compname_rev == T]
## sectors
sectors_for_surv <- sapply(1:length(unique(comp_in_surv)), function(x) d$Sector[d$Company.Name == unique(comp_in_surv)[x]][1]) 
length(sectors_for_surv)
ds_all$sector <- sectors_for_surv
## measures of wealth
## here, we can take average (or other central) measures over the time period, or measure representing changes 
mean_wealth_fun <- function(col){
  avg_vec <- sapply(1:length(unique(comp_in_surv)), function(x) mean(col[d$Company.Name == unique(comp_in_surv)[x]], na.rm = T)) 
  return(avg_vec)
}
change_wealth_fun <- function(col){
  change_vec <- sapply(1:length(unique(comp_in_surv)), function(x) mean(col[d$Company.Name == unique(comp_in_surv)[x]], na.rm = T)) 
  return(change_vec)
}

summary(ds_sector)

## kaplan meier
km_fit <- survfit(Surv(time, status) ~ 1, data = ds_sector)
autoplot(km_fit)
km_fit_sect <- survfit(Surv(time, status) ~ sector, data = ds_sector)
autoplot(km_fit_sect)
plot(km_fit_sect)
ggsurvplot(km_fit_sect)
abline(v = as.numeric(crisis_start), col = "red", lwd = 2)

cox_test_sect <- coxph(Surv(time, status) ~ sector, data = ds_sector)
summary(cox_test_sect)

