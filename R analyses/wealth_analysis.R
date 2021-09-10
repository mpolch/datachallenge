###############################################
## Wealth analysis
## data challenge 1 dsc
## September 09 2021
###############################################
# install.packages("rjson")
# install.packages("lme4")
# install.packages("ggfortify")
library(lme4)
library(ggfortify)

##
## loading data
##

## Mevluet data
d <- read.csv("mevluet_data_merged.csv", header = T)
summary(d)
dim(d)
# colnames(d) ## these are all the variables we can work with

## all company names
comp_names <- unique(d$Company.Name)
length(comp_names)
## all activity sectors
sector_vec <- unique(d$Sector)
length(sector_vec)
sector_vec

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Company wealth and expenses over the years: 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##
## graphing function
##
graph_fun <- function(response_variable, predictor_variable, grouping_variable, response_name, predictor_name = "Years"){
  min_x <- min(predictor_variable, na.rm = T)
  max_x <- max(predictor_variable, na.rm = T)
  min_y <- min(response_variable, na.rm = T)
  max_y <- max(response_variable, na.rm = T)
  grouping_vec <- unique(grouping_variable)
  grouping_col = rainbow(length(grouping_vec))
  plot(NULL, xlim = c(min_x, max_x), ylim = c(min_y, max_y), xlab = predictor_name, ylab = response_name)
  for(comp in comp_names){
    comp_group <- grouping_variable[d$Company.Name == comp][1]
    color <- grouping_col[grouping_vec == comp_group]
    lines(response_variable[d$Company.Name == comp] ~ predictor_variable[d$Company.Name == comp], type = "b", col = color)
  }
  legend(min_x, max_y, grouping_vec, grouping_col)
}

## a function to represent changes in wealth relative to wealth when the company first logged information
graph_fun_scalled <- function(response_variable, predictor_variable, grouping_variable, response_name, predictor_name = "Years"){
  scalled_resp <- sapply(1:length(comp_names), function(x) (response_variable[d$Company.Name == comp_names[x]] + 1) - (response_variable[d$Company.Name == comp_names[x]][1] + 1))
  scalled_resp <- unlist(scalled_resp)
  min_x <- min(predictor_variable, na.rm = T)
  max_x <- max(predictor_variable, na.rm = T)
  min_y <- min(scalled_resp, na.rm = T)
  max_y <- max(scalled_resp, na.rm = T)
  grouping_vec <- unique(grouping_variable)
  grouping_col = rainbow(length(grouping_vec))
  plot(NULL, xlim = c(min_x, max_x), ylim = c(min_y, max_y), xlab = predictor_name, ylab = response_name)
  for(comp in comp_names){
    comp_group <- grouping_variable[d$Company.Name == comp][1]
    color <- grouping_col[grouping_vec == comp_group]
    lines(scalled_resp[d$Company.Name == comp] ~ predictor_variable[d$Company.Name == comp], type = "b", col = color)
  }
  legend(min_x, max_y, grouping_vec, grouping_col)
}

## some example graphs: do not graph them all, this will take computing time
graph_fun(d$Revenue, d$Fiscal.Year, d$Sector, "revenues")
graph_fun_scalled(d$Revenue, d$Fiscal.Year, d$Sector, "revenues")
graph_fun(d$Dividends.Paid, d$Fiscal.Year, d$Sector, "dividends")
graph_fun_scalled(d$Dividends.Paid, d$Fiscal.Year, d$Sector, "dividends")
graph_fun(d$Research...Development, d$Fiscal.Year, d$Sector, "investment in research")
graph_fun_scalled(d$Research...Development, d$Fiscal.Year, d$Sector, "investment in research")
graph_fun(d$Gross.Profit, d$Fiscal.Year, d$Sector, "gross profit")
graph_fun_scalled(d$Gross.Profit, d$Fiscal.Year, d$Sector, "gross profit")
graph_fun(d$Treasury.Stock, d$Fiscal.Year, d$Sector, "treasury")
graph_fun(d$Total.Assets, d$Fiscal.Year, d$Sector, "total assets")
graph_fun_scalled(d$Total.Assets, d$Fiscal.Year, d$Sector, "total assets")

graph_fun(d$Revenue[d$Sector == "Healthcare"], d$Fiscal.Year[d$Sector == "Healthcare"], d$Sector[d$Sector == "Healthcare"], "revenues")

## graphing the right wealth variables
# liquidity ratio
liquidity_ratio <- d$Total.Current.Assets/d$Total.Current.Liabilities
liquidity_ratio[d$Total.Current.Liabilities == 0] <- NA
graph_fun(liquidity_ratio, d$Fiscal.Year, d$Sector, "liquidity ratio")
graph_fun_scalled(liquidity_ratio, d$Fiscal.Year, d$Sector, "liquidity ratio")
# solvency ratio
solvency_ratio <- d$Long.Term.Debt/d$Total.Equity
solvency_ratio[is.nan(solvency_ratio) == T] <- NA
solvency_ratio[solvency_ratio == Inf] <- NA
graph_fun(solvency_ratio, d$Fiscal.Year, d$Sector, "solvency ratio")
graph_fun_scalled(solvency_ratio, d$Fiscal.Year, d$Sector, "solvency ratio")

##
## statistical analysis: random slopes model (ATTENTION: the model does not converge)
##

## we apply random slopes models to look for effects of sectors or other variables on revenue evolution over years in each company
data_test <- data.frame(d$Company.Name, d$Fiscal.Year, d$Revenue, d$Sector)
colnames(data_test) <- c("company", "year", "revenue", "sector")
data_test <- na.exclude(data_test)
std_revenue <- (data_test$revenue - mean(data_test$revenue))/sd(data_test$revenue) ## standardizing data
data_test$std_revenue <- std_revenue
summary(data_test)
## the model
m1 <- lmer(std_revenue ~ year + sector + (1 + year | company), data = data_test, control = lmerControl(optimizer ="Nelder_Mead"))
summary(m1)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## wealth per sectors over the years
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##
## graphing function
##
graph_fun_sector_scalled <- function(response_variable, predictor_variable, grouping_variable, response_name, predictor_name = "Years"){
  scalled_resp <- sapply(1:length(comp_names), function(x) (response_variable[d$Company.Name == comp_names[x]] + 1) - (response_variable[d$Company.Name == comp_names[x]][1] + 1))
  scalled_resp <- unlist(scalled_resp)
  min_x <- min(predictor_variable, na.rm = T)
  max_x <- max(predictor_variable, na.rm = T)
  min_y <- min(scalled_resp, na.rm = T)
  max_y <- max(scalled_resp, na.rm = T)
  grouping_vec <- unique(grouping_variable)
  grouping_col = rainbow(length(grouping_vec))
  plot(NULL, xlim = c(min_x, max_x), ylim = c(min_y, max_y), xlab = predictor_name, ylab = response_name)
  for(sect in grouping_vec){
    color <- grouping_col[grouping_vec == sect]
    points(scalled_resp[d$Sector == sect] ~ predictor_variable[d$Sector == sect], col = color)
    abline(lm(scalled_resp[d$Sector == sect] ~ predictor_variable[d$Sector == sect]), col = color, lwd = 2)
  }
  abline(h = 0, col = "black", lty = 2, lwd = 2)
  legend(min_x, max_y, grouping_vec, grouping_col)
}

## some examples
graph_fun_sector_scalled(d$Revenue, d$Fiscal.Year, d$Sector, "revenues")
graph_fun_sector_scalled(d$Dividends.Paid, d$Fiscal.Year, d$Sector, "revenues")
graph_fun_sector_scalled(d$Research...Development, d$Fiscal.Year, d$Sector, "investment in research")
graph_fun_sector_scalled(d$Gross.Profit, d$Fiscal.Year, d$Sector, "gross profit")
graph_fun_sector_scalled(d$Treasury.Stock, d$Fiscal.Year, d$Sector, "treasury")
graph_fun_sector_scalled(d$Total.Assets, d$Fiscal.Year, d$Sector, "total assets")

##
## statistical analysis: random slopes model
## changes in wealth over the years per sectors
##
data_test <- data.frame(d$Company.Name, d$Fiscal.Year, d$Revenue, d$Sector)
colnames(data_test) <- c("company", "year", "revenue", "sector")
data_test <- na.exclude(data_test)
std_revenue <- (data_test$revenue - mean(data_test$revenue))/sd(data_test$revenue)
data_test$std_revenue <- std_revenue
summary(data_test)
m1 <- lmer(std_revenue ~ year + sector + (1 + year | sector), data = data_test, control = lmerControl(optimizer ="nloptwrap"))
summary(m1)
