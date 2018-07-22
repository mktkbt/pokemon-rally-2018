
library(tidyverse)
library(glue)

load("GA.rdata")
load("distance_matrix.rdata")
load("stations.rdata")
load("pokemon_routes.rdata")

station_names <- rownames(distance_matrix)  

# 全経路情報を求める
calc_tour_route <- function(tour) {
  tour_route_seq <- embed(tour, 2)[,2:1] 
  
  tibble(S1_name = station_names[tour_route_seq[,1]],
         S2_name = station_names[tour_route_seq[,2]]) %>%
    inner_join(pokemon_routes %>% select(S1_name, S2_name, S1_cd, S2_cd, time, routes, route), 
               by=c("S1_name", "S2_name")) %>%
    inner_join(stations %>% transmute(station_cd, S1_lon=lon, S1_lat=lat, S1_no=station_no), 
               by=c(S1_cd="station_cd")) %>%
    inner_join(stations %>% transmute(station_cd, S2_lon=lon, S2_lat=lat, S2_no=station_no), 
               by=c(S2_cd="station_cd")) %>%
    nest(-c(S1_name,S2_name)) %>%
    mutate(data = map(data, head, 1)) %>%
    unnest
}

# 全経路の順番で駅の座標（緯度経度）を求める
tour_route_lonlat <- function(tour) {
  tour_route <- calc_tour_route(tour)
  
  format_lonlat <- function(v) { glue("{v$S2_lon},{v$S2_lat}") }
  
  c(glue("{tour_route[1,]$S1_lon},{tour_route[1,]$S1_lat}"),
    tour_route %>%
      rowwise %>%
      do(lonlat = format_lonlat(.)) %>%
      ungroup %>%
      unlist) %>%
    paste(collapse=",")
}

# 総時間を求める
tour_route_time <- function(tour) {
  tour_route <- calc_tour_route(tour)
  sum(tour_route$time)
}


# 進化の経緯の経路情報
Proc_lonlat <-
  tibble(n = c(seq(5,45,by=5),
               seq(50,490,by=10),
               seq(500,1950,by=50),
               seq(2000,4900,by=100),
               seq(5000,20000,by=200))) %>%
  filter(n < GA@iter) %>%
  rowwise %>%
  mutate(time = tour_route_time(c(1, GA@bestSol[[n]][1,] + 1, 1))) %>%
  mutate(lonlat = tour_route_lonlat(c(1, GA@bestSol[[n]][1,] + 1, 1))) %>%
  ungroup %>%
  bind_rows(data_frame(n = GA@iter, 
                       time = tour_route_time(c(1, GA@solution[1,] + 1, 1)),
                       lonlat = tour_route_lonlat(c(1, GA@solution[1,] + 1, 1)))) %>%
  do(data = map(list(.), function(v) { glue("{v$n},{v$time},{v$lonlat}")} )) %>%
  unlist 

# 進化の経緯の経路情報をhtmlに埋め込み
html <- scan("map_template.html", what = character(), sep = "\n", blank.lines.skip = F)
route.line <- '<textarea id="route"></textarea>'
html[html == route.line] <- paste("<textarea id=\"route\">", paste(Proc_lonlat, collapse="\n"), "\n</textarea>", sep="")
out.file <- file("map.html")
writeLines(html, out.file)
close(out.file)

# 経路情報の詳細表示用
route_print <- 
  calc_tour_route(rev(c(1, GA@solution[1, ] + 1, 1))) %>%
  rowwise %>%
  do(data = map(list(.), function(v) {
    route <- 
      v$route %>% 
      rowwise %>% 
      do(data=map(list(.), function(v) {
        glue("{v$S1_name} ({v$line_name}) {v$S2_name}")
      })) %>%
      ungroup %>%
      unlist %>%
      paste(collapse = "/")
    glue("| {v$S1_name} | {route} | {v$time} | {v$S2_name} |")
  })) %>%
  ungroup %>%
  unlist %>%
  paste(collapse = "\n")

cat(route_print)
