# Light Analytics about Mowing Frequency

# Settings
getwd()

# Loadings
library(dplyr) # Rich data frame manipulation
library(ggplot2) # Rich plotting
library(mapdata) # Rich mapping
library(tidyr) # Tidy data

# Mowing Frequency ####

# Import the data
data_diary_raw <- read.csv("data/diary.csv")

# Check the data
head(data_diary_raw)
glimpse(data_diary_raw)

# Shape the data
data_diary <- data_diary_raw |>
  select(年月,草刈り) |>
  mutate(
    年月 = factor(年月, levels = unique(data_diary_raw$年月) |> sort(), ordered = T),
    草刈り = factor(草刈り, levels = unique(data_diary_raw$草刈り) |> sort(), ordered = T)
  )
glimpse(data_diary)
summary(data_diary)

tbl_diary <- table(data_diary) |>
  prop.table(margin = 1) |>
  as.data.frame()
tbl_diary

mean_mowingRatio <- mean(tbl_diary$Freq[tbl_diary$草刈り == "草刈りした日"])

# Visualize the data
ggplot(tbl_diary |> filter(草刈り == "草刈りした日"), aes(x = 年月, y = Freq, fill = Freq)) +
  geom_col(show.legend = F) +
  geom_hline(yintercept = mean_mowingRatio, linetype = "dashed", colour = "#777777") +
  geom_text(
    aes(x = 4, y = mean_mowingRatio + .01, label = paste0("全期間平均:", scales::percent( mean_mowingRatio)))
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradient(low = "#ffffff", high = "#333333")+
  labs(
    title = "全作業日のうち草刈りをした日の割合",
    subtitle = paste0("期間内の作業日数合計:", nrow(data_diary), "日"),
    caption = "※橋本の作業日誌から抽出。橋本作業日以外にもほかの人たちで草刈りをした日はあるので、実数とは少しずれている点に注意。",
    y = "草刈をした日の割合",
    x = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = 1)
  )

# Area Flow (All Country vs Tohigi) ####

# Get the area data
data_areaFlow_raw <- read.csv("data/本地けい畔別耕地面積統計_累年.csv")
head(data_areaFlow_raw)
glimpse(data_areaFlow_raw)

# Shape the data
data_areaFlow <- data_areaFlow_raw |>
  mutate(区分 = factor(区分, ordered = F))
glimpse(data_areaFlow)

data_areaFlow_gthrd <- gather(
  data_areaFlow,
  key = "Index",
  value = "value",
  -c(年次, 区分),
  factor_key = T
) |>
  separate(
    Index,
    into = c("田畑", "用途", "その他"),
    sep = "_",
    remove = T,
    fill = "right"
  )
glimpse(data_areaFlow_gthrd)

# Visualization
ggplot(data_areaFlow_gthrd |> filter(その他 == "増減率"), aes(x = 年次, y = value, lty = 用途, col = 区分)) +
  geom_hline(yintercept = 1, colour = "#777777") +
  geom_line(linewidth = 1.2) +
  # geom_point(size = 2) +
  scale_x_continuous(breaks = seq(from = 1965, to = 2022, by = 5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1.5, by = .1)) +
  scale_color_manual(values = c("#aaaaaa", "#333333")) +
  facet_wrap(~田畑) +
  labs(
    title = "1965年を1とした時の耕地面積の増減",
    caption = "作物統計調査（農林水産省）より橋本作成"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    axis.title.y = element_blank()
  )

# Ridge Ratio ####

# Import the data
data_areaPref_raw <- read.csv("本地けい畔別耕地面積累年統計_都道府県別.csv")
head(data_areaPref_raw)
glimpse(data_areaPref_raw)

# Get mapdata
map_data <- map_data("japan") |>
  mutate(region = tolower(region))
head(map_data)
glimpse(map_data)

# Shape the data
data_areaPref <- data_areaPref_raw |>
  mutate(
    region = tolower(region),
    田_けい畔率_パーセント = 田_けい畔_ha/田_本地_ha,
    畑_けい畔率_パーセント = 畑_けい畔_ha/畑_本地_ha,
  )

glimpse(data_areaPref)
data_areaPref_gth <- gather(
  data_areaPref,
  key = "Index",
  value = "value",
  -c(年次,都道府県名, region),
  factor_key = T
) |>
  separate(
    Index,
    into = c("田畑", "用途", "単位"),
    sep = "_"
  )
head(data_areaPref_gth)

data_areaPref_2022 <- filter(data_areaPref_gth, 年次 == 2022, 用途 == "けい畔率") |>
  right_join(map_data, by = "region", relationship= "many-to-many")
head(data_areaPref_2022)
tail(data_areaPref_2022)

# Visualize
ggplot(data_areaPref_2022, aes(x = long, y = lat, group = group, fill = value) ) +
  geom_polygon(linewidth = 0.5, colour = "#aaaaaa") +
  coord_map() +
  scale_fill_gradient(low = "#ffffff", high = "#333333", labels = scales::percent) +
  facet_wrap(~田畑) +
  labs(
    title = "都道府県別けい畔率（2022年比較）",
    subtitle = "※けい畔率：本地（作付けできる部分）に対するけい畔の広さ",
    caption = "作物統計調査（農水省）より橋本作成",
    fill = "けい畔率"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = 1),
    strip.background = element_rect(linewidth = 0.5),
    axis.title = element_blank()
  )

# Ridge Area Per Farmer (2019) ####

# Get the data
data_pop_raw <- read.csv("data/販売農家人口.csv")
head(data_pop_raw)
glimpse(data_pop_raw)

# Shape the data
data_pop <- data_pop_raw |>
  mutate(region = tolower(region))
data_ridgePerPop2019 <- data_areaPref |>
  filter(年次 == 2019) |>
  select(region, 田_けい畔_ha, 畑_けい畔_ha) |>
  left_join(data_pop) |>
  mutate(
    田_農家当たりのけい畔面積_ha = 田_けい畔_ha/販売農家人口,
    畑_農家当たりのけい畔面積_ha = 畑_けい畔_ha/販売農家人口
  )
data_ridgePerPop2019_gth <- gather(
  data_ridgePerPop2019,
  key = "Index",
  value = "value",
  -c(region, 都道府県)
) |>
  separate(
    Index,
    into = c("田畑", "用途", "単位"),
    sep = "_"
  )
head(data_ridgePerPop2019_gth)
tail(data_ridgePerPop2019_gth)
glimpse(data_ridgePerPop2019_gth)

data_ridgePerPop2019_map <- data_ridgePerPop2019_gth |>
  filter(用途 == "農家当たりのけい畔面積") |>
  right_join(map_data, by = "region", relationship = "many-to-many")
head(data_ridgePerPop2019_map)
glimpse(data_ridgePerPop2019_map)

data_ridgePerPop2019_cor <- data_ridgePerPop2019 |>
  select(-c(田_農家当たりのけい畔面積_ha,畑_農家当たりのけい畔面積_ha)) |>
  gather(
    key = "Index",
    value = "面積",
    -c(region, 都道府県, 販売農家人口)
  ) |>
  separate(
    col = Index,
    into = c("田畑", "用途", "単位"),
    sep = "_"
  ) |>
  mutate(
    人口当たり面積 = 面積/販売農家人口
  )
head(data_ridgePerPop2019_cor)
glimpse(data_ridgePerPop2019_cor)

# Classification
# km <- kmeans(
#   x = data_ridgePerPop |> select(販売農家人口, 田_けい畔_ha, 畑_けい畔_ha) |> scale(),
#   centers = 4,
# )
# data_km <- data.frame(
#   都道府県 = data_ridgePerPop$都道府県,
#   cluster = as.factor(km$cluster)
# )
# data_ridgePerPop2019_cor <- left_join(data_ridgePerPop2019_cor, data_km)
# head(data_ridgePerPop2019_cor)

# Clustering
m_rice <- mean(data_ridgePerPop2019_cor$人口当たり面積[data_ridgePerPop2019_cor$田畑 == "田"])
m_field <- mean(data_ridgePerPop2019_cor$人口当たり面積[data_ridgePerPop2019_cor$田畑 == "畑"])
sd_rice <- sd(data_ridgePerPop2019_cor$人口当たり面積[data_ridgePerPop2019_cor$田畑 == "田"])
sd_field <- sd(data_ridgePerPop2019_cor$人口当たり面積[data_ridgePerPop2019_cor$田畑 == "畑"])
data_ridgePerPop2019_cor <- mutate(
  data_ridgePerPop2019_cor,
  zscore = ifelse(田畑 == "田", (人口当たり面積 - m_rice)/sd_rice, (人口当たり面積 - m_field)/sd_field),
  cluster = case_when(
    zscore < -1 ~ "一人当たり面積少",
    zscore > 1 ~ "一人当たり面積大",
    TRUE ~ "ほどほど"
  ) |>
    factor(levels = c("一人当たり面積少", "ほどほど", "一人当たり面積大"))
)
head(data_ridgePerPop2019_cor)

# Visualize
ggplot(data_ridgePerPop2019_map, aes(x = long, y = lat, group = group, fill = value*10000)) +
  geom_polygon(linewidth = .5, colour = "#aaaaaa") +
  coord_map() +
  scale_fill_gradient(low = "#ffffff", high = "#333333") +
  facet_wrap(~田畑) +
  labs(
    title = "都道府県別 農家当たりのけい畔面積（2019年比較）",
    subtitle = "※農家人口には「販売農家（経営耕地面積30a以上または農産物販売金額が50万円以上の農家）」の数値を利用。",
    caption = "けい畔面積は作物統計調査（農水省）、農家人口は農業構造動態調査（同）より橋本作成",
    fill = "農家当たりのけい畔面積（平米）"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = 1),
    strip.background = element_rect(linewidth = 0.5),
    axis.title = element_blank()
  )

ggplot(data_ridgePerPop2019_cor, aes(x = 販売農家人口, y = 面積, label = 都道府県, colour = cluster)) +
  geom_text() +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("#333333", "#777777", "#333333")) +
  facet_wrap(~田畑) +
  labs(
    title = "都道府県別 けい畔面積と販売農家人口（2019年比較）",
    subtitle = "※販売農家（経営耕地面積30a以上または農産物販売金額が50万円以上の農家）",
    caption = "けい畔面積は作物統計調査（農水省）、農家人口は農業構造動態調査（同）より橋本作成",
    y = "けい畔面積（ha）"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = 1),
    strip.background = element_rect(linewidth = 0.5)
  )

# Color Plots ####

## Mowing Frequency ####
ggplot(tbl_diary |> filter(草刈り == "草刈りした日"), aes(x = 年月, y = Freq, fill = Freq)) +
  geom_col(show.legend = F) +
  geom_hline(yintercept = mean_mowingRatio, linetype = "dashed", colour = "#777777") +
  geom_text(
    aes(x = 4, y = mean_mowingRatio + .01, label = paste0("全期間平均:", scales::percent( mean_mowingRatio)))
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradient(low = "#ffffff", high = "#773333")+
  labs(
    title = "全作業日のうち草刈りをした日の割合",
    subtitle = paste0("期間内の作業日数合計:", nrow(data_diary), "日"),
    caption = "※橋本の作業日誌から抽出。橋本作業日以外にもほかの人たちで草刈りをした日はあるので、実数とは少しずれている点に注意。",
    y = "草刈をした日の割合",
    x = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = 1)
  )

## Area Time Series ####

ggplot(data_areaFlow_gthrd |> filter(その他 == "増減率"), aes(x = 年次, y = value, lty = 用途, col = 区分)) +
  geom_hline(yintercept = 1, colour = "#777777") +
  geom_line(linewidth = 1.2) +
  # geom_point(size = 2) +
  scale_x_continuous(breaks = seq(from = 1965, to = 2022, by = 5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1.5, by = .1)) +
  scale_color_manual(values = c("#aaaaaa", "#337733")) +
  facet_wrap(~田畑) +
  labs(
    title = "1965年を1とした時の耕地面積の増減",
    caption = "作物統計調査（農林水産省）より橋本作成"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    axis.title.y = element_blank()
  )

# Ridge Ratio Map ####
ggplot(data_areaPref_2022, aes(x = long, y = lat, group = group, fill = value) ) +
  geom_polygon(linewidth = 0.5, colour = "#aaaaaa") +
  coord_map() +
  scale_fill_gradient(low = "#ffffff", high = "#773333", labels = scales::percent) +
  facet_wrap(~田畑) +
  labs(
    title = "都道府県別けい畔率（2022年比較）",
    subtitle = "※けい畔率：本地（作付けできる部分）に対するけい畔の広さ",
    caption = "作物統計調査（農水省）より橋本作成",
    fill = "けい畔率"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = 1),
    strip.background = element_rect(linewidth = 0.5),
    axis.title = element_blank()
  )

# Ridge per Pop Map ####
ggplot(data_ridgePerPop2019_map, aes(x = long, y = lat, group = group, fill = value*10000)) +
  geom_polygon(linewidth = .5, colour = "#aaaaaa") +
  coord_map() +
  scale_fill_gradient(low = "#ffffff", high = "#773333") +
  facet_wrap(~田畑) +
  labs(
    title = "都道府県別 農家当たりのけい畔面積（2019年比較）",
    subtitle = "※農家人口には「販売農家（経営耕地面積30a以上または農産物販売金額が50万円以上の農家）」の数値を利用。",
    caption = "けい畔面積は作物統計調査（農水省）、農家人口は農業構造動態調査（同）より橋本作成",
    fill = "農家当たりのけい畔面積（平米）"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = 1),
    strip.background = element_rect(linewidth = 0.5),
    axis.title = element_blank()
  )

# Ridge per Pop Corrplot ####
ggplot(data_ridgePerPop2019_cor, aes(x = 販売農家人口, y = 面積, label = 都道府県, colour = cluster)) +
  geom_text(fontface = "bold") +
  geom_text(
    data = data_ridgePerPop2019_cor |> filter(都道府県 == "栃木県"),
    aes(
      x = 販売農家人口,
      y = 面積,
      label = 都道府県,
      colour = "#337733"
    ),
    fontface = "bold"
  ) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("#333377", "#555555", "#773333", "#337733"), labels = c("一人当たり面積少", "ほどほど", "一人当たり面積大", "栃木県")) +
  facet_wrap(~田畑) +
  labs(
    title = "都道府県別 けい畔面積と販売農家人口（2019年比較）",
    subtitle = "※販売農家（経営耕地面積30a以上または農産物販売金額が50万円以上の農家）",
    caption = "けい畔面積は作物統計調査（農水省）、農家人口は農業構造動態調査（同）より橋本作成",
    y = "けい畔面積（ha）"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(hjust = 1),
    legend.text = element_text(face = "bold"),
    strip.background = element_rect(linewidth = 0.5)
  )
