###################################
## Selective disappearance 
## analysis
##
###################################
library(ggplot2)

d <- read.csv("scraped_companies_merged_survival.csv", header = T)
summary(d)
d <- d[, -c(1, 2, 9)]
head(d_surv)
dim(d)

d <- d[is.na(d$current_ratio) == F, ]

max(d$current_ratio, na.rm = T)
min(d$current_ratio, na.rm = T)
plot(d$current_ratio ~ d$fiscal_year, log = "y")

##
## ratio per years per company graph
##

## company time of death
d$death <- format(as.character(d$most_recent_10k_filling), format = "%y%m%d")
d$surv_crisis <- numeric(length(d$death))
critical_time <- "2010-09-15"
d$surv_crisis[d$death > critical_time] <- "survived"
d$surv_crisis[d$death <= critical_time] <- "died"
col_vec <- numeric(length(d$death))
col_vec[d$surv_crisis == 0] <- "red"
col_vec[d$surv_crisis == 1] <- "blue"
names <- unique(d$Company.Name)
plot(NULL, xlim = c(min(d$fiscal_year), max(d$fiscal_year)), ylim = c(min(d$current_ratio), max(d$current_ratio)), 
     xlab = "Year", ylab = "current ratio", las = 1, log = "y")
for(name in names){
  x <- d$fiscal_year[d$Company.Name == name]
  y <- d$current_ratio[d$Company.Name == name]
  col <- col_vec[d$Company.Name == name][1]
  lines(y ~ x, type = "b", col = col)
}



## boxplot current ratio per year depending on whether or not they survived
d$fiscal_year <- as.factor(d$fiscal_year)
ggplot(d, aes(x=fiscal_year, y=current_ratio, fill=surv_crisis)) + 
  geom_boxplot() + scale_y_continuous(trans='log') +
  labs(x = "Years", y = "Current Ratio") + coord_cartesian(ylim = c(0.05, 100)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "top") +
  scale_fill_discrete(name="Crisis Survival within 2 years")

##
## statistics
##

d_stats <- d[d$fiscal_year == 2002 | d$fiscal_year == 2003 | d$fiscal_year == 2004 | d$fiscal_year == 2005 | d$fiscal_year == 2006 | d$fiscal_year == 2007 | d$fiscal_year == 2008 | d$fiscal_year == 2009, ]
hist(log(d_stats$current_ratio), breaks = 200)

m <- lm(log(current_ratio) ~ fiscal_year*surv_crisis, data = d_stats)
summary(m)
m1 <- lm(log(current_ratio) ~ fiscal_year + surv_crisis, data = d_stats)
anova(m, m1, test = "Chi")


