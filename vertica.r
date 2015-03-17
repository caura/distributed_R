#### FINAL  
library(distributedR)
distributedR_start()

# find the size
library(vRODBC)
connect <- odbcConnect("Vertica")
data <- sqlQuery(connect, "SELECT COUNT(*) FROM loan_stats AS loans LIMIT 10")

# create a frame with two partitions
# dc <- dframe(c(333163,100),c(200000,100))
dc<-dframe(npartitions=c(2,1))
#iterate dynamically through however many partitions we created (i.e. 2)
foreach(i, 1:npartitions(dc), initArrays<-function(y=splits(dc,i), index=i) {
  library(vRODBC)
  connect <- odbcConnect("Vertica")
  limit <- 10
  y<-sqlQuery(connect, paste("SELECT * FROM loan_stats AS loans LIMIT",limit,sep=" "))
  update(y)
})


foreach(i, 1:npartitions(dc), initArrays<-function(df=splits(dc,i), index=i) {
  poor_coverage <- sapply(df, function(x) {
    coverage <- 1 - sum(is.na(x)) / length(x)
    coverage < 0.8
  })
  df <- df[,poor_coverage==FALSE]
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
  outcomes <- ddply(df, .(year_issued, month_issued), function(x) {
    c("percent_bad"=sum(x$is_bad) / nrow(x),
      "n_loans"=nrow(x))
  })
  unique(df.term$year_issued)
  df.term <- subset(df, year_issued < 2013)
  df.term$home_ownership <- factor(df.term$home_ownership)
  df.term$is_rent <- df.term$home_ownership=="RENT"
  # df.term$fico_range <- factor(df.term$fico_range)
  # df.term$fico_ordered <- as.numeric(df.term$fico_range)
  # df.term$fico_range <- factor(df.term$fico_range)
  # df.term$fico_range <- factor(df.term$fico_range_low)
  # df.term$fico_ordered <- as.numeric(df.term$fico_range)
  idx <- runif(nrow(df.term)) > 0.75
  train <- df.term[idx==FALSE,]
  test <- df.term[idx==TRUE,]
  update(df)
})

library(HPdregression)
mylogit <- hpdglm(factor(is_bad) ~ last_fico_range_high + last_fico_range_low +
pub_rec_bankruptcies + revol_util + inq_last_6mths + is_rent, data = train, family = "binomial", na.action=na.omit)

distributedR_shutdown()
