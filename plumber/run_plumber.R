#!/usr/bin/env Rscript

#library(plumber)
#root <- pr("plumber.R")
#root
#root %>% pr_run(port = "8989")

#pr_run(root,  port = "8989")
library(plumber)
r <- plumb("plumber.R")  
r$run(host="0.0.0.0",port=8989)
