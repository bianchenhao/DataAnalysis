#################################
library(RJSONIO)
library(data.table)
dt.ranges <- as.character(seq.Date(as.Date("2020-01-26"), Sys.Date(), by = 1))

### 
l <- list() #分省
lc <- list() #分市
for (i in dt.ranges) {
  print(i)
  url <- sprintf("http://69.171.70.18:5000/data/city_level_%sT%s.csv", i, "10")
  tryCatch({
    c1 <- fromJSON(url)
  }, error = function(cond) {
    url <- sprintf("http://69.171.70.18:5000/data/city_level_%sT%s.csv", i, "18")
    c1 <- fromJSON(url)
  })
  
  ### 分省
  d1 <- data.table(id = names(c1$confirmedCount), confirmedCount = c1$confirmedCount, deadCount = c1$deadCount)
  d0 <- data.table(id = names(c1$provinceShortName), provincename = c1$provinceShortName)
  e <- unique(merge(d0, d1, by = "id", all = TRUE)[, .(provincename, confirmedCount, deadCount, dt = as.Date(i) - 1)])
  l <- c(l, list(e))
  
  ### 分市
  d1 <- data.table(id = names(c1$city.confirmedCount), confirmedCount = c1$city.confirmedCount, deadCount = c1$city.deadCount)
  d2 <- data.table(id = names(c1$city.cityName), cityName = c1$city.cityName)
  d0 <- data.table(id = names(c1$provinceShortName), provincename = c1$provinceShortName)
  e <- unique(merge(merge(d0, d1, by = "id", all = TRUE), d2, by = "id", all = TRUE)[, .(provincename, cityName, confirmedCount, deadCount, dt = as.Date(i) - 1)])
  lc <- c(lc, list(e))
}
# 分省数据合并
province_dat <- rbindlist(l)
# 分市数据合并
city_dat <- rbindlist(lc)
### 导出rds或csv
# saveRDS(list(province_dat, city_dat), "ncov.rds")
fwrite(province_dat, "province_dat.csv")
fwrite(city_dat, "city_dat.csv")
