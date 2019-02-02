library(cronR)
cron_clear(ask = FALSE)

f <- "some_script.R"
cmd <- cron_rscript(f)
cron_add(cmd, frequency = 'daily',
         id = 'daily_quote', at = '15:30', # 9:20a
         days_of_week = c(1,2,3,4,5))
