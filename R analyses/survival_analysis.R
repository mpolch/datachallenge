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
# library(survminer)
library(ggfortify)

##~~~~~~~~~~~~~~~~~~~~~~~
## loading data
##~~~~~~~~~~~~~~~~~~~~~~~

##
## Mevluet data
##
d_tot <- read.csv("mevluet_data_merged.csv", header = T)
# d_tot <- read.csv("prices_mevluet_data.csv", header = T)
summary(d_tot)
dim(d_tot)
# colnames(d_tot) ## these are all the variables we can work with

## all company names
comp_names <- unique(d_tot$Company.Name)
length(comp_names)
## all activity sectors
sector_vec <- unique(d_tot$Sector)
length(sector_vec)
sector_vec

##
## the survival datasets
##
d_surv <- read.csv("companies_filling_minimal.csv", header = T)
d_surv <- read.csv("scraped_companies_merged_survival.csv", header = T)
## this dataset involves companies ratio measures over the years they have been logging information
## We also have information about when they last logged, as an estimation of their death time
summary(d_surv)
dim(d_surv)
d_surv <- d_surv[, -c(1, 2, 9)]
head(d_surv)

## reformatting the dataset for a survival analysis: one row per company: only when using the "scraped_companies_merged_survival.csv" data
comp_names <- unique(d_surv$Company.Name)
no_index <- 1:length(d_surv$Company.Name)
indexes <- sapply(1:length(comp_names), function(x) no_index[d_surv$Company.Name == comp_names[x] & is.na(d_surv$most_recent_10k_filling) == F][1])
d <- d_surv[, c(2, 3, 4)] ## for the minimal dataset
d <- d_surv[indexes, c(1, 2, 9)] ## for the scrapped dataset
head(d)
colnames(d) <- c("name", "CIK", "death") ## for both datasets

## reformatting year at death
d$death <- format(as.character(d$death), format = "%y%m%d")
min(d$death)
max(d$death)

## censorship
d$status <- numeric(length(d$death))
d$status[d$death != max(d$death)] <- 1
summary(d$status)
head(d$status)

## lifespan in weeks
time <- difftime(d$death, min(d$death), units = "weeks")
head(time)
d$lifetime <- as.numeric(time)
head(d)

## measures of wealth: only for the scrapped dataset
## here, we can take average (or other central) measures over the time period, or measure representing changes 
mean_wealth_fun <- function(col, comp_names_vec){
  names <- unique(comp_names_vec)
  avg_vec <- sapply(1:length(names), function(x) mean(col[comp_names_vec == names[x]], na.rm = T)) 
  return(avg_vec)
}
change_wealth_fun <- function(col, comp_names_vec){
  names <- unique(comp_names_vec)
  change_vec <- numeric(length(names))
  count <- 1
  for(name in names){
    ratio_vec <- col[comp_names_vec == name]
    ratio_vec <- na.exclude(ratio_vec)
    if(length(ratio_vec) > 1){
      initial_val <- ratio_vec[length(ratio_vec)]
      last_val <- ratio_vec[1]
      change_vec[count] <- last_val - initial_val
    } else{
      change_vec[count] <- NA
    }
    count <- count + 1
  }
  return(change_vec)
}

## calculations for the scrapped dataset
mean_ratio <- mean_wealth_fun(d_surv$current_ratio, d_surv$Company.Name)
ratio_change <- change_wealth_fun(d_surv$current_ratio, d_surv$Company.Name)
d$mean_ratio <- mean_ratio
d$change_in_ratio <- ratio_change

## calculations for the minimal dataset
liquidity_ratio <- d_tot$Total.Current.Assets/d_tot$Total.Current.Liabilities
d_tot_mean_ratio <- mean_wealth_fun(liquidity_ratio, d_tot$Company.Name)
length(d_tot_mean_ratio)
length(unique(d_tot$Company.Name))
d_tot_change_ratio <- change_wealth_fun(liquidity_ratio, d_tot$Company.Name)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## company survival analyses
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## kaplan meier
km_fit <- survfit(Surv(lifetime, status) ~ 1, data = d)
plot(km_fit, xlab = "Time (in weeks after the first log)", ylab = "proportion of compagny still alive", main = "Company death with time")
crisis_start <- difftime(format("2008-09-15", format = "%y%m%d"), min(d$death), units = "weeks")
abline(v = as.numeric(crisis_start), col = "red", lwd = 2.5)
text(110, 0.4, "Before crisis", cex = 1.5)
text(350, 0.4, "After crisis", cex = 1.5)
# autoplot(km_fit)

## getting company sectors and other variables for this survival dataset
logical_compname <- (as.character(d$name) %in% as.character(unique(d_tot$Company.Name)))
length(logical_compname[logical_compname == T])

d_sector <- d[logical_compname == T, ]
logical_compname_rev <- (as.character(d_tot$Company.Name) %in% as.character(d$name))
length(logical_compname_rev[logical_compname_rev == T])
comp_in_surv <- d_tot$Company.Name[logical_compname_rev == T]
## sectors
sectors_for_surv <- sapply(1:length(unique(comp_in_surv)), function(x) d_tot$Sector[d_tot$Company.Name == unique(comp_in_surv)[x]][1]) 
length(sectors_for_surv)
d_sector$sector <- sectors_for_surv

## the effect of sectors on survival
cox_test_sector <- coxph(Surv(lifetime, status) ~ sector, data = d_sector)
summary(cox_test_sector)
## kaplan meier
km_fit <- survfit(Surv(lifetime, status) ~ 1, data = d_sector)
autoplot(km_fit)
km_fit_sect <- survfit(Surv(lifetime, status) ~ sector, data = d_sector)
plot(km_fit_sect)
abline(v = as.numeric(crisis_start), col = "red", lwd = 2)
autoplot(km_fit_sect)

## the effect of mean ratio on survival
# for the minimal 
d_ratio <- d[as.character(d$name) %in% as.character(unique(d_tot$Company.Name)),]
d_ratio$mean_ratio <- d_tot_mean_ratio[(unique(d_tot$Company.Name) %in% d$name) == T]
d_ratio$change_ratio <- d_tot_change_ratio[(unique(d_tot$Company.Name) %in% d$name) == T]
cox_test_mean_ratio <- coxph(Surv(lifetime, status) ~ mean_ratio, data = d_ratio)
summary(cox_test_mean_ratio)
cox_test_change_ratio <- coxph(Surv(lifetime, status) ~ change_ratio, data = d_ratio)
summary(cox_test_change_ratio)

# for the scrapped dataset
d_ratio <- d[is.na(d$mean_ratio) == F, ]
cox_test_mean_ratio <- coxph(Surv(lifetime, status) ~ mean_ratio, data = d_ratio)
summary(cox_test_mean_ratio)

## the effect of ratio change on survival
d_ratio <- d[is.na(d$change_in_ratio) == F, ]
cox_test_change_ratio <- coxph(Surv(lifetime, status) ~ change_in_ratio, data = d_ratio)
summary(cox_test_change_ratio)


