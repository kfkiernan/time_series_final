# This example is adapted from
# https://rviews.rstudio.com/2018/04/11/introduction-to-fama-french/

library(tidyquant)
library(tidyverse)
library(timetk)
library(glue)
require(astsa)

#symbols <- c("AAPL")#,"F", "TSLA")
symbols <- c("LITE")

prices <-
  getSymbols(symbols, src = 'yahoo',
             from = "2012-12-31",
             to = "2019-06-30",
             auto.assign = TRUE, warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

#w <- c(.33,.33,.33)#, 0.25, 0.20, 0.20, 0.10)
#w <- c(.85,.10,.05)#, 0.25, 0.20, 0.20, 0.10)
#w <- c(.05,.10,.85)#, 0.25, 0.20, 0.20, 0.10)
w <- c(1)

asset_returns_long <- 
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>% 
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  na.omit()

portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset,
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")


temp = tempfile()
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_3_Factors_csv.zip",temp,quiet = T)
Global_3_Factors <- read_csv(unz(temp, "Global_3_Factors.csv"),
                    skip = 6) %>%
  rename(date = X1) %>%
  mutate_at(vars(-date), as.numeric) %>%
  mutate(date =
           rollback(ymd(parse_date_time(date, "%Y%m") + months(1)))) %>%
  filter(date >=
           first(portfolio_returns_tq_rebalanced_monthly$date) & date <=
           last(portfolio_returns_tq_rebalanced_monthly$date))


ff_portfolio_returns <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  left_join(Global_3_Factors, by = "date") %>% 
  mutate(MKT_RF = Global_3_Factors$`Mkt-RF`/100,
         SMB = Global_3_Factors$SMB/100,
         HML = Global_3_Factors$HML/100,
         RF = Global_3_Factors$RF/100,
         R_excess = round(returns - RF, 4))


ff_dplyr_byhand <-
  ff_portfolio_returns %>% 
  do(model = 
       lm(R_excess ~ MKT_RF + SMB + HML, 
          data = .)) %>% 
  tidy(model, conf.int = T, conf.level = .95)

ff_dplyr_byhand %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  select(-statistic)

ff_dplyr_byhand %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>%
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, shape = term, color = term)) + 
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  labs(title = "FF 3-Factor Coefficients",
       x = "",
       y = "coefficient",
       caption = "data source: Fama-French website and yahoo! Finance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption  = element_text(hjust = 0))
