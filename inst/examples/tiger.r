ofile <- "C:/data/ftp2.census.gov/geo/tiger/TIGER2015/COUNTY/tl_2015_us_county.shp"
library(dplyr)
tiger_db <- src_sqlite("tiger.sqlite3", create = TRUE)

## read in and build a database

library(rgdal)
x <- readOGR(ofile, "tl_2015_us_county")
library(spbabel)
tab <- sptable(x)

x_tab  <- copy_to(tiger_db, tab, temporary = FALSE, 
                  indexes = list(c("object_", "branch_"), "x_", "y_"))

x_tab

## ------------------------------------------------

## Now we can drop the vertices out of memory and just deal with our attribute table

county <- as.data.frame(x) %>% as_data_frame() %>% mutate(object_ = row_number())
rm(tab, x)


spFromSQL <- function(data_x, sqltab) {
  #tempname <- sprintf("z_%s", paste(sample(c(letters, 0:9), 45, replace = TRUE), collapse = ""))
  #sx <- copy_to(tiger_db, data_x %>% select_("object_"), temporary = TRUE, name = tempname)
  longtab <- 
    sqltab %>% inner_join(dplyr::select(data_x, object_), "object_", copy = TRUE) %>% 
    arrange(object_, branch_, order_) %>% collect()
  
  spFromTable(longtab, attr_tab = data_x %>% select_(quote(-object_)))
}
x <- sample_n(county, 130)
plot(spFromSQL(x, x_tab))
plot(spFromSQL(county[521, ], x_tab))


sqltab  %>% group_by(object_)  %>% select(branch_)  %>% distinct()  %>% summarize(n = n())  %>% summarize(min = min(n), max = max(n))


# ## use to calculate area in small doses
# area <- numeric(nrow(county))
# for (i in seq_along(area)) {
#   area[i] <- rgeos::gArea(spFromSQL(county[i, ], x_tab))
# }
