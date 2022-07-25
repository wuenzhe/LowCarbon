# 设置工作路径
getwd()

# 加载工具包
library(xlsx)
library(dplyr)
library(stargazer)

# 导入数据
df1 <- read.xlsx("氮肥深施.xlsx", sheetName = "Sheet1", header = T)
df2 <- read.xlsx("缓释肥.xlsx", sheetName = "Sheet1", header = T)
summary(df1)
summary(df2)

# 变量重名名
names(df1)
names(df1)[c(1: 9)] <- c("id", "CH4", "N2O", "CO2", "depth", "density", "irrigation", "temperature", "organics")
names(df2)
names(df2)[c(1: 9)] <- c("id", "CH4", "N2O", "CO2", "slow", "density", "irrigation", "temperature", "organics")

# 回归分析
## 氮肥深施
depth_CH4 <- lm(CH4 ~ depth + density + irrigation + temperature + organics, data = df1)
summary(depth_CH4)
depth_N2O <- lm(N2O ~ depth + density + irrigation + temperature + organics, data = df1)
summary(depth_N2O)
depth_CO2 <- lm(CO2 ~ depth + density + irrigation + temperature + organics, data = df1)
summary(depth_CO2)
## 缓释肥
slow_CH4 <- lm(CH4 ~ slow + density + irrigation + temperature + organics, data = df2)
summary(slow_CH4)
slow_N2O <- lm(N2O ~ slow + density + irrigation + temperature + organics, data = df2)
summary(slow_N2O)
slow_CO2 <- lm(CO2 ~ slow + density + irrigation + temperature + organics, data = df2)
summary(slow_CO2)

# 输出结果
stargazer(depth_CH4, depth_N2O, depth_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "depth.html")
stargazer(slow_CH4, slow_N2O, slow_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "slow.html")
