# 1. 氮肥深施样本（18）
# 2. 缓释肥样本（11）
# 3. 缓释肥样本 + 氮肥深施度为0cm的样本（52）
# 1. 合并变量及样本（92）

rm(list = ls())

# 设置工作路径
getwd()
setwd("D:/个人文件/Low Carbon/low carbon")

# 加载工具包
library(xlsx)
library(dplyr)
library(stargazer)

# 导入数据
df <- read.xlsx("raw data.xlsx", sheetName = "merge", header = T)

# 变量重命名
names(df)
names(df)[c(1: 13)] <- c("id", "CH4", "N2O", "CO2", "depth", "ratio", "N", "SRNF", "densi", "irrig", "temper", "organ", "spe")

# 字符型变量处理
df$YLY6 <- df$spe
names(df)[13] <- "LYP9"
str(df)
df <- within(df, {
  irrig[irrig == "间歇性节水灌溉"] <- 1
  irrig[irrig == "淹水灌溉"] <- 0
  LYP9[LYP9 ==  "黄华占"] <- 0
  LYP9[LYP9 ==  "两优培九"] <- 1
  LYP9[LYP9 ==  "扬两优6号"] <- 0
  YLY6[YLY6 ==  "黄华占"] <- 0
  YLY6[YLY6 ==  "两优培九"] <- 0
  YLY6[YLY6 ==  "扬两优6号"] <- 1
})
df <- within(df, {
  irrig <- as.numeric(irrig)
  LYP9 <- as.numeric(LYP9)
  YLY6 <- as.numeric(YLY6)
})
str(df)

# 单位换算
df$N <- df$N / 2
df$SRNF <- df$SRNF / 2

# 描述性统计
summary(df)

# 回归分析
reg_result_CH4 <- lm(CH4 ~ N + SRNF + depth + ratio + densi + irrig + temper + organ + LYP9 + YLY6, data = df)
summary(reg_result_CH4)
reg_result_N2O <- lm(N2O ~ N + SRNF + depth + ratio + densi + irrig + temper + organ + LYP9 + YLY6, data = df)
summary(reg_result_N2O)
reg_result_CO2 <- lm(CO2 ~ N + SRNF + depth + ratio + densi + irrig + temper + organ + LYP9 + YLY6, data = df)
summary(reg_result_CO2)

# 输出结果
stargazer(reg_result_CH4, reg_result_N2O, reg_result_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "reg_results.html")
