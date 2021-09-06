###############################################
## Survival analysis
## data challenge 1 dsc
##
###############################################
# install.packages("rjson")
# install.packages("lme4")
library(lme4)


d <- read.csv("mevluet_data_merged.csv", header = T)
summary(d)
dim(d)
colnames(d)

comp_names <- unique(d$Company.Name)
length(comp_names)
sector_vec <- unique(d$Sector)
length(sector_vec)
sector_vec

min(d$Revenue, na.rm = T)
length(d$Revenue[d$Revenue <= 0])
plot(d$Revenue[d$Revenue > 0] ~ d$Sector[d$Revenue > 0], log = "y")

comp1 <- d[d$Company.Name == comp_names[1], ]
dim(comp1)
comp_names[1]
plot(comp1$Revenue ~ comp1$Fiscal.Year, type = "b")
plot(comp1$Dividends.Paid ~ comp1$Fiscal.Year, type = "b")
plot(comp1$Research...Development ~ comp1$Fiscal.Year, type = "b")

col_class <- sapply(1:length(colnames(comp1)), function(x) class(comp1[,x]))
comp1_numeric_col <- comp1[, col_class == "numeric"]
summary(comp1_numeric_col)
max(comp1_numeric_col[1,], na.rm = T)
comp1_numeric_col
num_col_vec <- colnames(comp1_numeric_col)

col_val <- numeric(length(num_col_vec))
sum_other_cols <- numeric(length(num_col_vec))
count <- 1
for(col_nb in 1:length(num_col_vec)){
  col <- num_col_vec[col_nb]
  other_cols <- num_col_vec[-col_nb]
  col_val[count] <- comp1_numeric_col[1, col]
  sum_other_cols[count] <- sum(comp1_numeric_col[1, other_cols], na.rm = T)
  count <- count + 1
}
data.frame(num_col_vec, col_val, sum_other_cols)
col_val[col_val >= sum_other_cols]


##
## Company earnings and expenses over the years
##

## graph function
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

colnames(d)
graph_fun(d$Revenue, d$Fiscal.Year, d$Sector, "revenues")
graph_fun(d$Dividends.Paid, d$Fiscal.Year, d$Sector, "dividends")
graph_fun(d$Research...Development, d$Fiscal.Year, d$Sector, "investment in research")
graph_fun(d$Gross.Profit, d$Fiscal.Year, d$Sector, "gross profit")
graph_fun(d$Treasury.Stock, d$Fiscal.Year, d$Sector, "treasury")
graph_fun(d$Total.Assets, d$Fiscal.Year, d$Sector, "total assets")


liquidity_ratio <- d$Total.Current.Assets/d$Total.Current.Liabilities
graph_fun(liquidity_ratio, d$Fiscal.Year, d$Sector, "liquidity ratio")
liquidity_ratio[d$Total.Current.Liabilities == 0] <- NA

solvency_ratio <- d$Long.Term.Debt/d$Total.Equity
min(solvency_ratio, na.rm = T)
max(solvency_ratio, na.rm = T)
solvency_ratio[is.nan(solvency_ratio) == T] <- NA
solvency_ratio[solvency_ratio == Inf] <- NA
graph_fun(solvency_ratio, d$Fiscal.Year, d$Sector, "solvency ratio")



## could apply random slopes models to look for effects of sectors or other variables on revenue evolution over years
data_test <- data.frame(d$Company.Name, d$Fiscal.Year, d$Revenue, d$Sector)
colnames(data_test) <- c("company", "year", "revenue", "sector")
data_test <- na.exclude(data_test)
std_revenue <- (data_test$revenue - mean(data_test$revenue))/sd(data_test$revenue)
data_test$std_revenue <- std_revenue
summary(data_test)
m1 <- lmer(std_revenue ~ year + sector + (1 + year | company), data = data_test, control = lmerControl(optimizer ="Nelder_Mead"))
summary(m1)
m1 <- lmer(std_revenue ~ year + sector + (1 + year | company), data = data_test, control = lmerControl(optimizer ="Nelder_Mead"))
summary(m1)







