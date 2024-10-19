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
data_diary_raw <- read.csv("diary.csv")

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
data_areaFlow_raw <- read.csv("本地けい畔別耕地面積統計_累年.csv")
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

# Ridge Area Per Farmer ####
