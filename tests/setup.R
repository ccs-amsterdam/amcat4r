amcat_offline <- class(try(amcat4r:::get_config("http://localhost/amcat"))) == "try-error"
writeLines(paste0("amcat_offline=", amcat_offline,
                  "\n", "bearer=", tok$access_token), ".Renviron")
