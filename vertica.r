#### FINAL  
library(distributedR)
distributedR_start()

# find the size
library(vRODBC)
connect <- odbcConnect("Vertica")
data <- sqlQuery(connect, "SELECT COUNT(*) FROM loan_stats AS loans LIMIT 10")

# create a frame with two partitions
node_size = 200000
max_rows = 333163
options("scipen"=100, "digits"=4)
dc <- dframe(c(333163,105),c(node_size,105))
# dc<-dframe(npartitions=c(2,1))
#iterate dynamically through however many partitions we created (i.e. 2)
foreach(i, 1:npartitions(dc), initArrays<-function(y=splits(dc,i), index=i) {
  library(vRODBC)
  options("scipen"=100, "digits"=4)
  node_size = 200000
  max_rows = 333163
  connect <- odbcConnect("Vertica")
  offset <- (node_size * (index - 1))
  limit <- min(max_rows - offset,node_size)
  y<-sqlQuery(connect, paste("SELECT * FROM loan_stats AS loans WHERE issue_d < TO_DATE('2013-01-01','YYYY-MM-dd') OFFSET",offset,"LIMIT",limit,sep=" "))
  y$is_bad <- data.frame(c(1:limit))
  y$year_issued <- data.frame(c(1:limit))
  y$month_issued <- data.frame(c(1:limit))
  update(y)
})


  # poor coverage can be used to unset columns for non-parametric algorithms
  # poor_coverage <- sapply(df, function(x) {
  #   coverage <- 1 - sum(is.na(x)) / length(x)
  #   coverage < 0.8
  # })
  # df <- df[,poor_coverage==FALSE]

foreach(i, 1:npartitions(dc), initArrays<-function(df=splits(dc,i), index=i) {
  library(lubridate)
  library(stringr)
  bad_indicators <- c("Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off")
  unique(df$loan_status)
  df$is_bad <- ifelse(df$loan_status %in% bad_indicators, 1,
                         ifelse(df$loan_status=="", NA,
                                  0))
  unique(df$is_bad)

  df$issue_d <- as.Date(df$issue_d)
  df$year_issued <- year(df$issue_d)
  df$month_issued <- month(df$issue_d)
  df$earliest_cr_line <- as.Date(df$earliest_cr_line)
  df$revol_util <- str_replace_all(df$revol_util, "[%]", "")
  df$revol_util <- as.numeric(df$revol_util)

  df$home_ownership <- factor(df$home_ownership)
  df$is_rent <- df$home_ownership=="RENT"
  # df.term$fico_range <- factor(df.term$fico_range)
  # df.term$fico_ordered <- as.numeric(df.term$fico_range)
  # df.term$fico_range <- factor(df.term$fico_range)
  # df.term$fico_range <- factor(df.term$fico_range_low)
  # df.term$fico_ordered <- as.numeric(df.term$fico_range)
  update(df)
})

# TO DO: FIX
idx <- runif(nrow(dc)) > 0.75
train <- dc[idx==FALSE,]
test <- dc[idx==TRUE,]

  # library(plyr)
  # outcomes <- ddply(df, .(year_issued, month_issued), function(x) {
  #   c("percent_bad"=sum(x$is_bad) / nrow(x),
  #     "n_loans"=nrow(x))
  # })

library(HPdregression)
mylogit <- hpdglm(factor(train$is_bad), 
                  dframe(train$last_fico_range_high,
                    train$last_fico_range_low,
                    train$pub_rec_bankruptcies,
                    train$revol_util,
                    train$inq_last_6mths,
                    train$is_rent), 
                  family = "binomial", na_action="exclude")

distributedR_shutdown()
