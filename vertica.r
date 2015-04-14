#### FINAL  
library(distributedR)
distributedR_start()

# find the size
library(vRODBC)
connect <- odbcConnect("Vertica")
data <- sqlQuery(connect, "SELECT COUNT(*) FROM loan_stats AS loans WHERE issue_d < TO_DATE('2013-01-01','YYYY-MM-dd') LIMIT 1")

# create a frame with n partitions
node_size = 10000  #partition size
max_rows = data$COUNT
options("scipen"=100, "digits"=4)
dc <- dframe(c(max_rows,104),c(node_size,104))
# dc<-dframe(npartitions=c(2,1))
#iterate dynamically through however many partitions we created
foreach(i, 1:npartitions(dc), initArrays<-function(y=splits(dc,i), index=i,n_size=node_size,m_rows=max_rows) {
  library(vRODBC)
  options("scipen"=100, "digits"=4)
  connect <- odbcConnect("Vertica")
  offset <- (n_size * (index - 1))
  limit <- min(m_rows - offset,n_size)
  y<-sqlQuery (connect, paste("SELECT * FROM loan_stats AS loans WHERE issue_d < TO_DATE('2013-01-01','YYYY-MM-dd') OFFSET",offset,"LIMIT",limit,sep=" "))
  y$is_bad <- data.frame(c(1:limit))
  y$year_issued <- data.frame(c(1:limit))
  y$month_issued <- data.frame(c(1:limit))
  y$is_rent <- data.frame(c(1:limit))
  update(y)
})

foreach(i, 1:npartitions(dc), transform<-function(df=splits(dc,i), index=i) {
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

train_partitions <- ceiling(max_rows/node_size*0.75)  #e.g. 8
test_partitions <- ceiling(max_rows/node_size) - train_partitions #e.g. 2

train <- dframe(c(train_partitions*node_size,104),c(node_size,104))
test <- dframe(c(max_rows-train_partitions*node_size,104),c(node_size,104))

#alternative way
# train <- dframe(npartitions=c(train_partitions,1))    # create new dframe to hold training data set
# test <- dframe(npartitions=c(test_partitions,1))     # create new dframe to hold test data set

# Following is a cluster-equivalent to this:
# idx <- runif(nrow(dc)) > 0.75
# train <- dc[idx==FALSE,]
# test <- dc[idx==TRUE,]

foreach(i, 1:npartitions(dc), function(
  dc_i=splits(dc, i), 
  train_i=splits(train, min(i,train_partitions)), 
  test_i=splits(test, max(1,i-train_partitions)), 
  idx=i,
  split_parts = train_partitions) {
     if(idx <= split_parts) {                  #Copy 1st max_rows/node_size*0.75 partitions as training data set
        train_i <- dc_i
        update(train_i)
     }else {
        test_i <- dc_i               # Copy rest max_rows/node_size*0.25 partitions as test data set
        update(test_i)
     }
})

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
