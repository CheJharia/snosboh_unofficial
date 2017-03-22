################################################################################
# READ: read survey data into R environment
Dworcester <- readr::read_csv(file = "worcester-raw-V2.csv"
                             , progress = T
                             , trim_ws = T
)
