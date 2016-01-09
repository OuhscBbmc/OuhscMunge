ds_safety_long <- retrieve_column_info("DhsWaiver", "tblSafetyLongVictim")

readr::write_csv(ds_safety_long, "./inst/test-data/safety-long.csv")
base::saveRDS(ds_safety_long, "./inst/test-data/safety-long.rds")
