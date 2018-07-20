library(tidyverse)
library(magrittr)
library(XML)
library(glue)

if (F) {
  # 路線情報を取得する
  # 取得して絞り込んだ内容（＋移動時間の情報）をline_info.csvに保存しておいて、以降はそれを読み込む
  lines <- 
    # 県を指定して路線情報を取得
    tibble(pref_cd = c(11,12,13,14)) %>%
    rowwise() %>%
    mutate(data = list(xmlParse(glue("http://www.ekidata.jp/api/p/{pref_cd}.xml")))) %>%
    mutate(data = list(getNodeSet(data, "//line"))) %>%
    mutate(data = list(unlist(data))) %>%
    mutate(data = list(xmlToDataFrame(data, stringsAsFactors=F))) %>%
    unnest %>%
    select(-pref_cd) %>%
    distinct %>%
    # JR限定、なぜか宇都宮線だけは路線名に「JR」がついていない
    filter(grepl("JR", line_name) | line_cd == '11319') %>%
    arrange(line_cd)
}

# 路線情報を読み取り
lines <- read_csv("line_info.csv", 
                  col_types = cols(line_cd = col_character(), 
                                   line_name = col_character(), 
                                   move_time = col_double(), 
                                   wait_time = col_double()))


# 駅情報を取得
stations <-
  lines %>%
  select(line_cd, line_name) %>%
  rowwise() %>%
  mutate(data = list(xmlParse(glue("http://www.ekidata.jp/api/l/{line_cd}.xml")))) %>%
  mutate(data = list(getNodeSet(data, "//station"))) %>%
  mutate(data = list(unlist(data))) %>%
  mutate(data = list(xmlToDataFrame(data, stringsAsFactors=F))) %>%
  # 駅番号を追加
  mutate(data = list(bind_cols(data, station_no = 1:nrow(data)))) %>%
  unnest

if (F) {
  # 取得した駅情報を保存しておいて、次回以降はそのファイルを読み込むようにする
  save(stations, file="stations.rdata")

  # 駅情報の読み込み
  load("stations.rdata")
}


# 路線に「裏山手線」を追加
lines %<>%
  filter(line_cd == 11302) %>%
  mutate(line_cd = "113020", line_name='JR山手線２') %>%
  bind_rows(lines)

# 裏山手線の駅情報、本来の山手線の駅番号を更新したもの（田端から駒込）
yamanote2 <-
  stations %>%
  filter(line_cd == 11302) %>%
  mutate(line_cd = "113020", line_name='JR山手線２') %>%
  mutate(station_no = station_no + if_else(station_no > 15, -15, 15)) 

# 駅情報に裏山手線を追加
stations %<>%
  bind_rows(yamanote2)

# 各の路線数、これが２以上なら乗換駅
stations %<>%
  nest(-station_g_cd) %>%
  mutate(station_g_count = map_dbl(data, function(v) { nrow(v)})) %>%
  unnest

# 駅情報のうちポケモン駅の情報
pokemon_stations <- 
  read_csv("pokemon.csv") %>%
  inner_join(stations %>% select(station_g_cd, station_name) %>% distinct, by=c(station_name="station_name")) 



# 駅間の移動情報をtibbleにする
compose_route <- function(line_cd,line_name,S1_cd,S1_g_cd,S1_name,S2_name,S2_cd,S2_g_cd,time) {
  data_frame(line_cd=line_cd,
             line_name = line_name,
             S1_cd = S1_cd,
             S1_g_cd = S1_g_cd,
             S1_name = S1_name,
             S2_cd = S2_cd,
             S2_g_cd = S2_g_cd,
             S2_name = S2_name,
             time = time)
}


# 乗り換えなしで移動できる駅の組み合わせ
message("direct_routes")
direct_routes <-
  # 路線ごとに、全駅の組み合わせを作る
  stations %>%
  rename(d_line_cd = line_cd, d_line_name = line_name) %>%
  nest(-c(d_line_cd, d_line_name)) %>%
  mutate(data = map(data, function(v) {
    expand.grid(d_S1_cd = v$station_cd, d_S2_cd = v$station_cd, stringsAsFactors = F) %>%
      as_tibble %>%
      filter(d_S1_cd != d_S2_cd) %>%
      mutate(d_S1_g_cd = map_chr(d_S1_cd, function(scd) { (v %>% filter(station_cd == scd))[["station_g_cd"]] })) %>%
      mutate(d_S2_g_cd = map_chr(d_S2_cd, function(scd) { (v %>% filter(station_cd == scd))[["station_g_cd"]] })) %>%
      mutate(d_S1_name = map_chr(d_S1_cd, function(scd) { (v %>% filter(station_cd == scd))[["station_name"]] })) %>%
      mutate(d_S2_name = map_chr(d_S2_cd, function(scd) { (v %>% filter(station_cd == scd))[["station_name"]] })) %>%
      mutate(n = map2_dbl(d_S1_cd, d_S2_cd, function(S1_cd, S2_cd) {
        S1_no <- (v %>% filter(station_cd == S1_cd))[["station_no"]]
        S2_no <- (v %>% filter(station_cd == S2_cd))[["station_no"]]
        abs(S1_no - S2_no)
      })) 
  })) %>%
  unnest %>%
  # 路線ごとに求めた駅の組み合わせ駅間の移動時間を求める
  inner_join(lines, by=c(d_line_cd="line_cd", d_line_name="line_name")) %>%
  mutate(d_time = n * move_time + wait_time) %>%
  select(-c(n, move_time, wait_time)) %>%
  # 乗車区間を作っておく
  rowwise %>%
  mutate(d_route = list(compose_route(d_line_cd,d_line_name,d_S1_cd,d_S1_g_cd,d_S1_name,d_S2_name,d_S2_cd,d_S2_g_cd,d_time))) %>%
  ungroup %>%
  # 同じ駅間では、最も移動時間の短いもの（路線）だけを残す
  nest(-c(d_S1_g_cd, d_S2_g_cd)) %>%
  mutate(data = map(data, function(data) { data %>% arrange(d_time) %>% head(1) })) %>%
  unnest %>%
  # 並べ替え
  arrange(d_S1_g_cd, d_S2_g_cd)

# 乗り換えなしで移動できる駅の組み合わせのうち、
# 出発駅が乗換駅のもの、かつ
# 到着駅がの乗換駅またはポケモン駅
message("transit_routes")
transit_routes <-
  direct_routes %>%
  inner_join(stations %>% 
               filter(station_g_count>1) %>% 
               select(line_cd, station_g_cd), 
             by=c(d_line_cd = "line_cd", d_S1_g_cd = "station_g_cd")) %>%
  left_join(pokemon_stations %>% 
              select(station_g_cd, pokemon), 
            by=c(d_S2_g_cd="station_g_cd")) %>%
  left_join(stations %>% 
              select(station_g_cd, station_g_count) %>%
              distinct, 
            by=c(d_S2_g_cd="station_g_cd")) %>%
  filter(!is.na(pokemon) | station_g_count>1) %>%
  select(-pokemon, -station_g_count)

# 駅間の組み合わせの初期値
# 乗り換えなし、かつ
# 出発駅がポケモン駅、かつ
# 到着駅がの乗換駅またはポケモン駅
message("initial_routes")
routes <- 
  direct_routes %>%
  transmute(S1_g_cd = d_S1_g_cd,
            S2_g_cd = d_S2_g_cd,
            line_cd = d_line_cd,
            line_name = d_line_name,
            S1_cd = d_S1_cd,
            S2_cd = d_S2_cd,
            S1_name = d_S1_name,
            S2_name = d_S2_name,
            time = d_time,
            routes = 1,
            route = d_route) %>%
  arrange(S1_g_cd, S2_g_cd) %>%
  inner_join(pokemon_stations %>% select(station_g_cd), by=c(S1_g_cd="station_g_cd")) %>%
  left_join(pokemon_stations %>% 
              select(station_g_cd, pokemon), 
            by=c(S2_g_cd="station_g_cd")) %>%
  left_join(stations %>% 
              select(station_g_cd, station_g_count) %>%
              distinct, 
            by=c(S2_g_cd="station_g_cd")) %>%
  filter(!is.na(pokemon) | station_g_count>1) %>%
  select(-pokemon, -station_g_count)

max_routes <- 0

# 乗り継ぎを追加して、新たな経路を求める。
# 乗り継ぎを追加することで既存の経路より短い時間になるものは置き換える
# 乗り継ぎを追加しても、経路が追加されない、かつ、置き換えが発生しなくなるまで繰り返し
while(max_routes < max(routes$routes)) {
  
  message("routes ", max_routes <- max(routes$routes), " transits")

  routes %<>%
    # これまで見つけた経路で到着駅が乗換駅のものに対して
    # 乗換駅から乗り換えなしで行ける経路を追加する
    inner_join(transit_routes, by=c(S2_g_cd = "d_S1_g_cd")) %>%
    # 最後の路線と乗り換え後路線は別路線
    filter(line_cd != d_line_cd) %>%
    # 出発駅と乗り換え後の到着駅は別駅
    filter(S1_g_cd != d_S2_g_cd) %>%
    # 経路情報を更新
    mutate(S2_g_cd = d_S2_g_cd,
           line_cd = d_line_cd,
           line_name = d_line_name,
           S2_cd = d_S2_cd,
           S2_name = d_S2_name,
           time = time + d_time,
           routes = routes + 1) %>%
    rowwise %>%
    mutate(route = list(bind_rows(route, d_route))) %>%
    ungroup %>%
    # 列を絞り込み
    select(S1_g_cd, S2_g_cd, 
           line_cd, line_name,
           S1_cd, S2_cd, S1_name, S2_name,
           time, routes, route) %>%
    # これまでの経路に追加
    bind_rows(routes) %>%
    # 出発駅と到着駅が同じ場合は、時間がもっとも短いものだけを残す
    nest(-c(S1_g_cd, S2_g_cd)) %>%
    mutate(data = map(data, function(data) { data %>% arrange(time, routes) %>% head(1) })) %>%
    unnest %>%
    # 並べ替え
    arrange(S1_g_cd, S2_g_cd)
}

# 見つかった経路のうち、始点・終点がポケモン駅のもの
pokemon_routes <- 
  routes %>%
  inner_join(pokemon_stations %>% select(station_g_cd), by=c(S1_g_cd="station_g_cd")) %>%
  inner_join(pokemon_stations %>% select(station_g_cd), by=c(S2_g_cd="station_g_cd")) 

save(pokemon_routes, file="pokemon_routes.rdata")

# ポケモン駅間の距離行列を求める
distance_matrix_tbl <- 
  pokemon_routes %>% 
  select(S1_name, S2_name, time) %>% 
  spread(key=S2_name, value=time)

distance_matrix <-
  distance_matrix_tbl %>% 
  select(-S1_name) %>%
  as.matrix

# 東京駅（または池袋駅）をゴールにする
finish_station <- '東京'
# finish_station <- '池袋'

# 距離行列でゴール駅を先頭に持ってくる
distance_matrix <-rbind(distance_matrix[distance_matrix_tbl$S1_name == finish_station, ],
                        distance_matrix[distance_matrix_tbl$S1_name != finish_station, ])
distance_matrix <-cbind(distance_matrix[, distance_matrix_tbl$S1_name == finish_station],
                        distance_matrix[, distance_matrix_tbl$S1_name != finish_station])
station_names <- c(finish_station, distance_matrix_tbl$S1_name[distance_matrix_tbl$S1_name != finish_station])

colnames(distance_matrix) <- rownames(distance_matrix) <- station_names

save(distance_matrix, file="distance_matrix.rdata")


