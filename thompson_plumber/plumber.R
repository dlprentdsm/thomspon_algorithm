#!/usr/bin/env Rscript
library(plumber)
pr <- plumber::plumb(paste0("/usr/special/","thompson_api.R"))
pr$run(host='0.0.0.0', port=6012, swagger = ifelse(Sys.getenv("SWAGGER")=="TRUE", TRUE, FALSE))
