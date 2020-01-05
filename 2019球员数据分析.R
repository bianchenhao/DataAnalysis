library(devtools)
library(purrr)
library(dplyr)
library(tidyr)
# remotes::install_github('ewenme/understatr')
library(understatr)
library(ggplot2)
library(pals)
library(scales)       ## plotting scales
library(ggimage)      ## images for flags
library(ggforce)      ## plotting text labels
library(cowplot)      ## plotting grid
library(glue)         ## text
library(ggrepel)      ## plotting text labels
library(magick)
library(patchwork)

windowsFonts(myFont = windowsFont("微软雅黑")) # 导入微软雅黑字体

# 1. 主题 --------------------------------------------------------------------

theme_copaAmerica <- function(
  title.size = 24,
  subtitle.size = 14,
  caption.size = 8,
  axis.text.size = 14,
  axis.text.x.size = 12,
  axis.text.y.size = 12,
  axis.title.size = 16,
  strip.text.size = 18,
  panel.grid.major.x = element_line(size = 0.5, color = "white"),
  panel.grid.major.y = element_line(size = 0.5, color = "white"),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.ticks = element_line(color = "white")) {
  ## Theme:
  theme(text = element_text(family = "myFont", color = "white"),
        plot.title = element_text(family = "myFont", face = "bold", 
                                  size = title.size, color = "yellow"),
        plot.subtitle = element_text(size = subtitle.size),
        plot.caption = element_text(size = caption.size),
        panel.background = element_rect(fill = "#009b3a"),
        plot.background = element_rect(fill = "#002776"),
        legend.background = element_rect(fill = '#002776'),
        legend.key = element_rect(fill = '#002776', colour = '#002776'),
        axis.text = element_text(size = axis.text.size, color = "white"),
        axis.text.x = element_text(size = axis.text.x.size, color = "white"),
        axis.text.y = element_text(size = axis.text.y.size, color = "white"),
        axis.title = element_text(size = axis.title.size),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.x = panel.grid.major.x,
        panel.grid.major.y = panel.grid.major.y,
        panel.grid.minor.x = panel.grid.minor.x,
        panel.grid.minor.y = panel.grid.minor.y,
        strip.text = element_text(color = "yellow", face = "bold", 
                                  size = strip.text.size, 
                                  margin = margin(4.4, 4.4, 4.4, 4.4)),
        strip.background = element_blank(),
        axis.ticks = axis.ticks
  )
}

# 2. 数据获取与清洗 -----------------------------------------------------------------

player_name <- c('Neymar2099', 'Kylian Mbappe-Lottin3423', 'Cristiano Ronaldo2371', 'Lionel Messi2097',
         'Karim Benzema2370', 'Wu Lei7398', 'Robert Lewandowski227', 'Son Heung-Min 453', 'Jamie Vardy755', 'Pierre-Emerick Aubameyang318')
player_codes <- c('2099', '3423', '2371', '2097', '2370', '7398', '227', '453', '755', '318')
name_matchdt <- data.frame(en = c("Neymar", "Kylian Mbappe-Lottin", "Cristiano Ronaldo", "Lionel Messi", "Karim Benzema",
                                  "Wu Lei", "Robert Lewandowski", "Son Heung-Min", "Jamie Vardy", "Pierre-Emerick Aubameyang"),
                           cn = c("内马尔", "姆巴佩", "C罗", "梅西", "本泽马", "武磊", "莱万多夫斯基", "孙兴慜", "杰米·瓦尔迪", "奥巴梅扬"))

understat_data <- player_codes %>% 
  map(., ~ understatr::get_player_seasons_stats(.x)) %>% 
  reduce(bind_rows) %>% 
  select(-player_id, -position, -yellow, -red)

understat_data <- merge(understat_data, name_matchdt, by.x = "player_name", by.y = "en")
understat_data$player_name <- understat_data$cn
understat_data$cn <- NULL

comparison_data <- understat_data %>% 
  filter(year == 2019) %>% 
  select(-team_name, -year) %>% 
  rename(Shots = shots, KP = key_passes) %>% 
  gather(key = "key", value = "value", -player_name) %>% 
  mutate(key = forcats::as_factor(key) %>% 
           forcats::fct_relevel(., 
                                "xG", "goals", "npxG", "npg", 
                                "xA", "assists", "xGChain", "xGBuildup",
                                "Shots", "KP", "games", "time"))
# 3. 10名球员基础指标总览 -----------------------------------------------------------------

base_plot1 <- comparison_data %>%
  filter(key %in% c("games")) %>%  # , "time", "Shots", "goals")
  mutate(key = "上场数") %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  # geom_vline(xintercept = 1.5, size = 2) +
  # geom_vline(xintercept = 2.5, size = 2) +
  # geom_vline(xintercept = 3.5, size = 2) +
  # geom_vline(xintercept = 4.5, size = 2) +
  # geom_vline(xintercept = 5.5, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_fill_manual(values = pals::glasbey(16), name = "Player") +
  labs(title = "五大联赛顶级射手数据对比",
       x = NULL, y = NULL)  +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))

base_plot2 <- comparison_data %>%
  filter(key %in% c("time")) %>%  # , "time", "Shots", "goals")
  mutate(key = "上场时间") %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  # geom_vline(xintercept = 1.5, size = 2) +
  # geom_vline(xintercept = 2.5, size = 2) +
  # geom_vline(xintercept = 3.5, size = 2) +
  # geom_vline(xintercept = 4.5, size = 2) +
  # geom_vline(xintercept = 5.5, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_fill_manual(values = pals::glasbey(16), name = "Player") +
  labs(title = " ",
       x = NULL, y = NULL)  +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))

base_plot3 <- comparison_data %>%
  filter(key %in% c("Shots")) %>%  # , "time", "Shots", "goals")
  mutate(key = "射门数") %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  # geom_vline(xintercept = 1.5, size = 2) +
  # geom_vline(xintercept = 2.5, size = 2) +
  # geom_vline(xintercept = 3.5, size = 2) +
  # geom_vline(xintercept = 4.5, size = 2) +
  # geom_vline(xintercept = 5.5, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_fill_manual(values = pals::glasbey(16), name = "Player") +
  labs(title = NULL,
       x = NULL, y = NULL)  +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))

base_plot4 <- comparison_data %>%
  filter(key %in% c("goals")) %>%  # , "time", "Shots", "goals")
  mutate(key = "进球数") %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  # geom_vline(xintercept = 1.5, size = 2) +
  # geom_vline(xintercept = 2.5, size = 2) +
  # geom_vline(xintercept = 3.5, size = 2) +
  # geom_vline(xintercept = 4.5, size = 2) +
  # geom_vline(xintercept = 5.5, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_fill_manual(values = pals::glasbey(16), name = "Player") +
  labs(title = NULL,
       x = NULL, y = NULL)  +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))
(base_plot1 + base_plot2) / (base_plot3 + base_plot4) 

# 4. 10名球员主要分析指标总览 --------------------------------------------------------------

comparison_strikers_plot <- comparison_data %>%
  filter(key != "Shots", key != "KP",
         key != "xGBuildup", key != "xGChain") %>% 
  mutate(value = value / time * 90) %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  geom_vline(xintercept = 1.5, size = 2) +
  geom_vline(xintercept = 2.5, size = 2) +
  geom_vline(xintercept = 3.5, size = 2) +
  geom_vline(xintercept = 4.5, size = 2) +
  geom_vline(xintercept = 5.5, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01),
                     limits = c(0, 1.30)) +
  scale_fill_manual(values = pals::glasbey(16), name = "Player") +
  labs(title = "五大联赛顶级射手数据对比",
       x = NULL, y = "每90分钟数据")  +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))
comparison_strikers_plot


# 5. npxG与goals散点图 --------------------------------------------------------------

expected_goal_plot <- understat_data %>% 
  filter(year == 2019) %>% 
  select(player_name, time, npxG, xG, goals) %>% 
  mutate_at(c("npxG", "xG", "goals"), ~. / time * 90) %>% 
  ggplot(aes(x = npxG, y = goals, fill = player_name)) +
  geom_abline(intercept = 0, slope = 1, color = "white", size = 1.1) +
  geom_point(shape = 21, size = 5, color = "black", stroke = 1.1) +
  scale_x_continuous(limits = c(0, 1.1),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.3),
                     expand = c(0, 0)) +
  scale_fill_manual(values = pals::glasbey(10), name = "Player") +
  labs(title = "除点球外期望进球 VS 真实进球",
        x = "每90分钟除点球外期望进球(npxG)",
       y = "每90分钟真实进球数(goals)")  +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))

expected_goal_plot

# 6. KP与shots图 --------------------------------------------------------------

kp_shots_plot <- comparison_data %>%
  filter(key == "Shots" | key == "KP") %>% 
  mutate(value = value / time * 90) %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01),
                     limits = c(0, 6),
                     breaks = c(0, 1, 2, 3, 4, 5, 6),
                     labels = c(0, 1, 2, 3, 4, 5, 6)) +
  scale_fill_manual(values = pals::glasbey(17), name = "Player") +
  geom_vline(xintercept = 1.5, size = 2) +
  labs(title = "五大联赛巨星的关键传球和射门数比较",
       x = NULL, y = "每90分钟数据")   +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))

kp_shots_plot

# 7. xGBuildup与xGChain散点图 --------------------------------------------------------------

xgbuildup_xgchain_plot <- comparison_data %>%
  filter(key == "xGBuildup" | key == "xGChain") %>% 
  mutate(value = value / time * 90) %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01),
                     limits = c(0, 1.55),
                     breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5),
                     labels = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)) +
  scale_fill_manual(values = pals::glasbey(17), name = "Player") +
  geom_vline(xintercept = 1.5, size = 2) +
  labs(title = "五大联赛巨星的进攻参与度和控球参与度比较",
       x = NULL, y = "每90分钟数据")  +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))

xgbuildup_xgchain_plot


