library(data.table)
library(dplyr)
library(ggplot2)
library(rayshader)
library(RColorBrewer)
library(stringr)
windowsFonts(myFont = windowsFont("微软雅黑")) # 导入微软雅黑字体

provinces_dat <- fread("https://raw.githubusercontent.com/bianchenhao/DataAnalysis/master/ncov/province_dat.csv", encoding = "UTF-8")
cities_dat <- fread("https://raw.githubusercontent.com/bianchenhao/DataAnalysis/master/ncov/city_dat.csv", encoding = "UTF-8")

# 1. 全国各省累积确诊 ---------------------------------------------------------------
### 数据清洗
data <- dcast(provinces_dat, dt ~ provincename, value.var = "confirmedCount")
data <- tail(data, 10)
data[is.na(data)] <- 0

data0 <- melt.data.table(data, id.vars = "dt", variable.name = "区域")
data1 <- data0[区域 != "湖北"]
y_orders <- data1[dt == max(dt)][order(value), 区域] # 对区域重新排序，画图备用
# 将日期清洗成缩写
data1[, dt := substring(dt, 7, 10)]
data1[, dt := str_replace_all(dt, "0(?=[0-9])", "")]

### 2D热力图
param1 <- 1/2.5
ggplot(data1, aes(x = dt, y = 区域)) + 
  geom_tile(aes(fill = value, color = value), color = "#F5F5F5") + 
  scale_x_discrete(NULL, expand = c(0, 0), position = "top") +
  scale_y_discrete(NULL, expand = c(0, 0), limits = y_orders, position = "right") +
  scale_fill_gradientn(colours = brewer.pal(9,"OrRd")) +
  ggtitle("全国除湖北各省份累积确诊数", paste0("数据更新于北京时间 ", Sys.Date(), " 10:00")) +
  labs(caption = "数据来源: 丁香园\n绘图：@会痛的stone") +
  geom_text(aes(label = value), color = "white", fontface = "bold", size = 7 * param1) +
  theme(text = element_text(family = "myFont"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(face= "italic", size = 12 * param1), 
        legend.position = "none",
        axis.text = element_text(size = 16 * param1, face ="bold"),
        axis.ticks.y = element_blank(),
        title = element_text(size = 16 * param1, face ="bold"),
        panel.border = element_rect(color = "#F5F5F5", fill = NA)) -> 
  provinces_2d
ggsave("provinces_2d.png", provinces_2d, width = 8 * param1, height = 22 * param1)

### 3D热力图
ggplot(data1, aes(x = dt, y = 区域)) + 
  geom_tile(aes(fill = value, color = value), color = "#696969") + 
  scale_x_discrete(NULL, expand = c(0, 0)) +
  scale_y_discrete(NULL, expand = c(0, 0), limits = y_orders) + 
  scale_fill_gradientn(colours = brewer.pal(9,"OrRd")) +
  ggtitle("全国除湖北各省份累积确诊数") +
  labs(caption = "数据来源: 丁香园\n绘图：@会痛的stone") +
  theme(text = element_text(family = "myFont"),
        legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12, face ="bold"),
        panel.border = element_rect(color = "#696969", fill = NA)) -> 
  provinces_3d

plot_gg(provinces_3d, multicore = TRUE, width = 4, height = 11, scale = 500, 
        background = "#afceff", shadowcolor = "#3a4f70", windowsize = c(2564, 1421), # windowsize受pc的清晰度影响，若超过会自动生成pc支持的最大尺寸
        zoom = 0.6, phi = 30, theta = -45, sunangle = 45) # 需要运行1~3分钟
Sys.sleep(0.2)
render_snapshot(filename = "provinces_3d.png")

# 2. 全国各省死亡率 -------------------------------------------------------------------
### 数据清洗
data <- dcast(provinces_dat[, .(provincename, mortality = round(deadCount/confirmedCount * 100, 1), dt)], dt ~ provincename, value.var = "mortality")
data <- tail(data, 10)
data[is.na(data)] <- 0

data0 <- melt.data.table(data, id.vars = "dt", variable.name = "区域")
y_orders <- data0[dt == max(dt)][order(value), 区域] # 对区域重新排序，画图备用
# 将日期清洗成缩写
data0[, dt := substring(dt, 7, 10)]
data0[, dt := str_replace_all(dt, "0(?=[0-9])", "")]

### 2D热力图
param1 <- 1/2.5
ggplot(data0, aes(x = dt, y = 区域)) + 
  geom_tile(aes(fill = value, color = value), color = "#F5F5F5") + 
  scale_x_discrete(NULL, expand = c(0, 0), position = "top") +
  scale_y_discrete(NULL, expand = c(0, 0), limits = y_orders, position = "right") +
  scale_fill_gradientn(colours = brewer.pal(9,"OrRd")) +
  ggtitle("全国各省份死亡率（%）", paste0("数据更新于北京时间 ", Sys.Date(), " 10:00")) +
  labs(caption = "数据来源: 丁香园\n绘图：@会痛的stone") +
  geom_text(aes(label = value), color = "white", fontface = "bold", size = 7 * param1) +
  theme(text = element_text(family = "myFont"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(face= "italic", size = 12 * param1), 
        legend.position = "none",
        axis.text = element_text(size = 16 * param1, face ="bold"),
        axis.ticks.y = element_blank(),
        title = element_text(size = 16 * param1, face ="bold"),
        panel.border = element_rect(color = "#F5F5F5", fill = NA)) -> 
  provinces_m_2d
ggsave("provinces_m_2d.png", provinces_m_2d, width = 8 * param1, height = 22 * param1)

#####################
# 3. 上海情况 -------------------------------------------------------------------
shanghai <- cities_dat[provincename == "上海"] %>% 
  dcast(dt ~ cityName, value.var = "confirmedCount")
shanghai1 <- shanghai[, colnames(shanghai)[grepl("区|人员|dt", colnames(shanghai))], with = FALSE][dt >= "2020-01-31"]
shanghai1[is.na(shanghai1)] <- 0
shanghai1 <- tail(shanghai1, 10)
shanghai1 <- melt.data.table(shanghai1, id.vars = "dt", variable.name = "区域")
shanghai1 <- shanghai1[!区域 %in% c("未知地区", "待明确地区")]
y_orders <- shanghai1[dt == max(dt)][order(value), 区域]

# 将日期清洗成缩写
shanghai1[, dt := substring(dt, 7, 10)]
shanghai1[, dt := str_replace_all(dt, "0(?=[0-9])", "")]

### 2D热力图
param1 <- 1/2.5
ggplot(shanghai1, aes(x = dt, y = 区域)) + 
  geom_tile(aes(fill = value, color = value), color = "#F5F5F5") + 
  scale_x_discrete(NULL, expand = c(0, 0), position = "top") +
  scale_y_discrete(NULL, expand = c(0, 0), limits = y_orders, position = "right") +
  # scale_fill_viridis("累积确诊数") +
  scale_fill_gradientn(colours = brewer.pal(9,"OrRd")) +
  ggtitle("上海各区分日累积确诊数", paste0("数据更新于北京时间 ", Sys.Date(), " 10:00")) +
  labs(caption = "数据来源: 丁香园\n绘图：@会痛的stone") +
  geom_text(aes(label = value), color = "white", fontface = "bold", size = 8 * param1) +
  theme(text = element_text(family = "myFont"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(face= "italic", size = 12 * param1), 
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 16 * param1, face ="bold"),
        title = element_text(size = 16 * param1, face ="bold"),
        panel.border = element_rect(color = "#F5F5F5", fill = NA)) -> 
  measles_gg1
ggsave("shanghai_area.png", measles_gg1, width = 7 * param1, height = 13 * param1)

### 3D热力图
param1 <- 1
ggplot(shanghai1, aes(x = dt, y = 区域)) + 
  geom_tile(aes(fill = value, color = value), color = "#696969") + 
  scale_x_discrete(NULL, expand = c(0, 0)) +
  scale_y_discrete(NULL, expand = c(0, 0), limits = y_orders) +
  # scale_fill_viridis("累积确诊数") +
  scale_fill_gradientn(colours = brewer.pal(9, "OrRd")) +
  ggtitle("上海各区分日累积确诊数") +
  labs(caption = "数据来源: 丁香园\n绘图：@会痛的stone") +
  # geom_text(aes(label = value)) +
  theme(text = element_text(family = "myFont", face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12, face ="bold"),
        panel.border = element_rect(color = "#696969", fill = NA)) -> 
  measles_gg
#
plot_gg(measles_gg, multicore = TRUE, width = 7, height = 13, scale = 500, 
        background = "#afceff", shadowcolor = "#3a4f70", windowsize = c(2564, 1421), # windowsize受pc的清晰度影响，若超过会自动生成pc支持的最大尺寸
        zoom = 0.6, phi = 30, theta = -45, sunangle = 45)# 需要运行1~3分钟
Sys.sleep(0.2)
render_snapshot("shanghai_area_3d.png")


