# 1. 氮肥深施样本（48）
# 2. 缓释肥样本（44）
# 3. 缓释肥样本 + 氮肥深施度为0cm的样本（52）
# 4. 合并变量及样本（92）

rm(list = ls())

# 设置工作路径
getwd()
setwd("D:/个人文件/Low Carbon/low carbon")

# 加载工具包
library(xlsx)
library(dplyr)
library(stargazer)

# 导入数据
df1 <- read.xlsx("raw data.xlsx", sheetName = "depth", header = T)
df2 <- read.xlsx("raw data.xlsx", sheetName = "SRNF_1", header = T)
df3 <- read.xlsx("raw data.xlsx", sheetName = "SRNF_2", header = T)
df4 <- read.xlsx("raw data.xlsx", sheetName = "merge", header = T)

# 变量重命名
names(df1)
names(df1)[c(1: 10)] <- c("id", "CH4", "N2O", "CO2", "depth", "ratio", "densi", "irrig", "temper", "organ")
names(df2)
names(df2)[c(1: 10)] <- c("id", "CH4", "N2O", "CO2", "SRNF", "N", "densi", "irrig", "temper", "organ")
names(df3)
names(df3)[c(1: 10)] <- c("id", "CH4", "N2O", "CO2", "SRNF", "N", "densi", "irrig", "temper", "organ")
names(df4)
names(df4)[c(1: 12)] <- c("id", "CH4", "N2O", "CO2", "depth", "ratio", "N", "SRNF", "densi", "irrig", "temper", "organ")

# 单位换算
df2$N <- df2$N / 2
df2$SRNF <- df2$SRNF / 2
df3$N <- df3$N / 2
df3$SRNF <- df3$SRNF / 2
df4$N <- df4$N / 2
df4$SRNF <- df4$SRNF / 2

# 描述性统计
summary(df1)
summary(df2)
summary(df3)
summary(df4)

# 回归分析
## depth
depth_CH4 <- lm(CH4 ~ depth + ratio + densi + irrig + temper + organ, data = df1)
summary(depth_CH4)
depth_N2O <- lm(N2O ~ depth + ratio + densi + irrig + temper + organ, data = df1)
summary(depth_N2O)
depth_CO2 <- lm(CO2 ~ depth + ratio + densi + irrig + temper + organ, data = df1)
summary(depth_CO2)
## SRNF_1
SRNF_1_CH4 <- lm(CH4 ~ SRNF + N + densi + irrig + temper + organ, data = df2)
summary(SRNF_1_CH4)
SRNF_1_N2O <- lm(N2O ~ SRNF + N + densi + irrig + temper + organ, data = df2)
summary(SRNF_1_N2O)
SRNF_1_CO2 <- lm(CO2 ~ SRNF + N + densi + irrig + temper + organ, data = df2)
summary(SRNF_1_CO2)
## SRNF_2
SRNF_2_CH4 <- lm(CH4 ~ SRNF + N + densi + irrig + temper + organ, data = df3)
summary(SRNF_2_CH4)
SRNF_2_N2O <- lm(N2O ~ SRNF + N + densi + irrig + temper + organ, data = df3)
summary(SRNF_2_N2O)
SRNF_2_CO2 <- lm(CO2 ~ SRNF + N + densi + irrig + temper + organ, data = df3)
summary(SRNF_2_CO2)
## merge
merge_CH4 <- lm(CH4 ~ N + SRNF + depth + ratio + densi + irrig + temper + organ, data = df4)
summary(merge_CH4)
merge_N2O <- lm(N2O ~ N + SRNF + depth + ratio + densi + irrig + temper + organ, data = df4)
summary(merge_N2O)
merge_CO2 <- lm(CO2 ~ N + SRNF + depth + ratio + densi + irrig + temper + organ, data = df4)
summary(merge_CO2)

# 输出结果
stargazer(depth_CH4, depth_N2O, depth_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "depth.html")
stargazer(SRNF_1_CH4, SRNF_1_N2O, SRNF_1_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "SRNF_1.html")
stargazer(SRNF_2_CH4, SRNF_2_N2O, SRNF_2_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "SRNF_2.html")
stargazer(merge_CH4, merge_N2O, merge_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "merge.html")

