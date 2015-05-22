# library(distributedR)
library(HPdata)
source('/home/looker/cat2num.R'
#distributedR_start(cluster_conf='cluster_conf.xml')
distributedR_start()
cat2num(srcTable='training', dsn='Vertica', dstTable='training_modified',view=TRUE)
historical_data <- db2darrays(tableName='training_modified',dsn='Vertica', 
      # Y - matrix
      list('"loans.is_bad"'),
      # X - matrix
      list('"loans.issue_year"','"loans.is_rent"',
      '"borrower.revol_utilization"','"borrower.fico_range_high"',
      '"borrower.inquiries_last_6mths"',
      '"borrower.pub_rec_bankruptcies"'))

library(HPdregression)
theModel <- hpdglm(responses=historical_data$Y, predictors=historical_data$X, family=binomial)
# do the necessery out of sample testing using tableName='testing'
# ...
# ...
￼
# push model coefficients back to the database
deploy.model(model=theModel, dsn='Vertica', modelName='glmLoans', modelComments='A logistic regression model for LendingClub Data')
distributedR_shutdown()