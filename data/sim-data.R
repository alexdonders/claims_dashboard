# simulate transactions on insurance claims
# transactions will include claim closures, payments and changes in case reserves
# this is all completely made up and does not accurately resemble actual claims

library(lubridate)
library(dplyr)
library(randomNames)

# number of claims
n_claims = 1000
beg_date = as.Date("2012-01-01")
end_date = as.Date("2017-06-21")
accident_range = as.numeric(end_date - beg_date)

set.seed(1234)
accident_date = sample(0:accident_range, size = n_claims, replace = TRUE)
payment_fun = function(n) rlnorm(n,7.5, 1.5)

claims = dplyr::data_frame(
  claim_num = paste0("claim-", 1:n_claims),
  accident_date = beg_date + lubridate::days(accident_date),
  state = sample(c("TX", "CA", "GA", "FL"), size = n_claims, replace = TRUE),
  claimant = randomNames::randomNames(n_claims),
  report_date = rbinom(n_claims,5,0.25),
  status = rbinom(n_claims,1,0.96),
  payment = payment_fun(n_claims)
)

claims = claims %>% mutate(report_date = accident_date + report_date,
                           payment = ifelse(status == 0, 0, payment),
                           case = payment * runif(n_claims,0.25,4.0),
                           transaction_date = report_date
                           ) %>%
                    arrange(accident_date)

n_trans = rnbinom(n_claims,3,0.25)
trans_lag = lapply(n_trans, function(x) rnbinom(x,7,0.1))
trans_lag = lapply(trans_lag, function(x){if(length(x)==0)0 else x})

for(i in seq_len(n_claims)){
  trans_lag[[i]] = data_frame(
    "trans_lag" = trans_lag[[i]],
    "claim_num" = paste0("claim-",i)
  )
}


trans_tbl = bind_rows(trans_lag)
trans_tbls = trans_tbl %>% 
              group_by(claim_num) %>% 
              mutate(trans_lag = cumsum(trans_lag)) %>%
              ungroup()

zero_claims = dplyr::filter(claims, status == 0)
first_trans = dplyr::filter(claims, status == 1)

subsequent_trans = left_join(trans_tbl, first_trans, by = "claim_num") %>% filter(!is.na(accident_date))
n_trans = nrow(subsequent_trans)

subsequent_trans = subsequent_trans %>% 
                      mutate(payment = payment_fun(n_trans),
                             case = pmax(case * rnorm(n_trans, 1.5, 0.1) - payment, 500),
                             transaction_date = report_date + trans_lag) %>%
                      select(-trans_lag)




                          
