# library(distributedR)
library(HPdata)
distributedR_start()
historical_data <- db2darrays(tableName='training',dsn='Vertica', 
      # Y - matrix
      list('"loans.is_bad"'),
      # X - matrix
      list('"loans.issue_year"','"loans.is_rent"',
      '"borrower.revol_utilization"','"borrower.fico_range_high"',
      '"borrower.fico_range_low"','"borrower.inquiries_last_6mths"',
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