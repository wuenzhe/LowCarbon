# 1. 氮肥深施样本（48）
# 2. 缓释肥样本（44）
# 3. 缓释肥样本 + 氮肥深施度为0cm的样本（52）
# 4. 合并变量及样本（92）

# 设置工作路径
getwd()
setwd("D:/个人文件/Low Carbon/low carbon")

# 加载工具包
library(xlsx)
library(dplyr)
library(stargazer)

# 导入数据
df1 <- read.xlsx("raw data.xlsx", sheetName = "depth", header = T)
df2 <- read.xlsx("raw data.xlsx", sheetName = "slow_1", header = T)
df3 <- read.xlsx("raw data.xlsx", sheetName = "slow_2", header = T)
df4 <- read.xlsx("raw data.xlsx", sheetName = "merge", header = T)
summary(df1)
summary(df2)
summary(df3)
summary(df4)

# 变量重命名
names(df1)
names(df1)[c(1: 10)] <- c("id", "CH4", "N2O", "CO2", "depth", "ratio", "density", "irrigation", "temperature", "organics")
names(df2)
names(df2)[c(1: 10)] <- c("id", "CH4", "N2O", "CO2", "slow", "N", "density", "irrigation", "temperature", "organics")
names(df3)
names(df3)[c(1: 10)] <- c("id", "CH4", "N2O", "CO2", "slow", "N", "density", "irrigation", "temperature", "organics")
names(df4)
names(df4)[c(1: 12)] <- c("id", "CH4", "N2O", "CO2", "depth", "ratio", "N", "slow", "density", "irrigation", "temperature", "organics")

# 回归分析
## depth
depth_CH4 <- lm(CH4 ~ depth + ratio + density + irrigation + temperature + organics, data = df1)
summary(depth_CH4)
depth_N2O <- lm(N2O ~ depth + ratio + density + irrigation + temperature + organics, data = df1)
summary(depth_N2O)
depth_CO2 <- lm(CO2 ~ depth + ratio + density + irrigation + temperature + organics, data = df1)
summary(depth_CO2)
## slow_1
slow_1_CH4 <- lm(CH4 ~ slow + N + density + irrigation + temperature + organics, data = df2)
summary(slow_1_CH4)
slow_1_N2O <- lm(N2O ~ slow + N + density + irrigation + temperature + organics, data = df2)
summary(slow_1_N2O)
slow_1_CO2 <- lm(CO2 ~ slow + N + density + irrigation + temperature + organics, data = df2)
summary(slow_1_CO2)
## slow_2
slow_2_CH4 <- lm(CH4 ~ slow + N + density + irrigation + temperature + organics, data = df3)
summary(slow_2_CH4)
slow_2_N2O <- lm(N2O ~ slow + N + density + irrigation + temperature + organics, data = df3)
summary(slow_2_N2O)
slow_2_CO2 <- lm(CO2 ~ slow + N + density + irrigation + temperature + organics, data = df3)
summary(slow_2_CO2)
## merge
merge_CH4 <- lm(CH4 ~ depth + ratio + N + slow + density + irrigation + temperature + organics, data = df4)
summary(merge_CH4)
merge_N2O <- lm(N2O ~ depth + ratio + N + slow + density + irrigation + temperature + organics, data = df4)
summary(merge_N2O)
merge_CO2 <- lm(CO2 ~ depth + ratio + N + slow + density + irrigation + temperature + organics, data = df4)
summary(merge_CO2)

# 输出结果
stargazer(depth_CH4, depth_N2O, depth_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "depth.html")
stargazer(slow_1_CH4, slow_1_N2O, slow_1_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "slow_1.html")
stargazer(slow_2_CH4, slow_2_N2O, slow_2_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "slow_2.html")
stargazer(merge_CH4, merge_N2O, merge_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "merge.html")

