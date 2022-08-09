
rm(list = ls())

# 设置工作路径
getwd()
setwd("D:/个人文件/Low Carbon/low carbon")

# 加载工具包
library(xlsx)
library(dplyr)
library(stargazer)

# 导入数据
df <- read.xlsx("raw data.xlsx", sheetName = "全样本", header = T)

# 变量重命名
names(df)
names(df)[c(1: 15)] <- c("id", "CH4", "N2O", "CO2", "depth", "ratio", "N", "SRNF", "densi", "irrigMethod", "irrigFreq", "temper", "organ", "Shannon", "spe")
names(df)

# 字符型变量处理
df$YLY6 <- df$spe
names(df)[15] <- "LYP9"
str(df)
df <- within(df, {
  irrigMethod[irrigMethod == "间歇性节水灌溉"] <- 1
  irrigMethod[irrigMethod == "淹水灌溉"] <- 0
  irrigMethod <- as.numeric(irrigMethod)
  LYP9[LYP9 ==  "黄华占"] <- 0
  LYP9[LYP9 ==  "两优培九"] <- 1
  LYP9[LYP9 ==  "扬两优6号"] <- 0
  LYP9 <- as.numeric(LYP9)
  YLY6[YLY6 ==  "黄华占"] <- 0
  YLY6[YLY6 ==  "两优培九"] <- 0
  YLY6[YLY6 ==  "扬两优6号"] <- 1
  YLY6 <- as.numeric(YLY6)
})
str(df)

# 单位换算
attach(df)
df$N <- N / 2
df$SRNF <- SRNF / 2
detach(df)

# 描述性统计
summary(df)

# 基准回归分析
reg_result_CH4 <- lm(CH4 ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + Shannon + LYP9 + YLY6, data = df)
summary(reg_result_CH4)
reg_result_N2O <- lm(N2O ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + Shannon + LYP9 + YLY6, data = df)
summary(reg_result_N2O)
reg_result_CO2 <- lm(CO2 ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + Shannon + LYP9 + YLY6, data = df)
summary(reg_result_CO2)
stargazer(reg_result_CH4, reg_result_N2O, reg_result_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "reg_results_127.html")

# 剔除Shannon
reg_result_CH4 <- lm(CH4 ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + LYP9 + YLY6, data = df)
summary(reg_result_CH4)
reg_result_N2O <- lm(N2O ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + LYP9 + YLY6, data = df)
summary(reg_result_N2O)
reg_result_CO2 <- lm(CO2 ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + LYP9 + YLY6, data = df)
summary(reg_result_CO2)
stargazer(reg_result_CH4, reg_result_N2O, reg_result_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "reg_results_127_剔除多样性.html")

# 选入92个样本观测
df92 <- df[1: 92, ]
summary(df92)
reg_result_CH4 <- lm(CH4 ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + Shannon + LYP9 + YLY6, data = df92)
summary(reg_result_CH4)
reg_result_N2O <- lm(N2O ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + Shannon + LYP9 + YLY6, data = df92)
summary(reg_result_N2O)
reg_result_CO2 <- lm(CO2 ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + Shannon + LYP9 + YLY6, data = df92)
summary(reg_result_CO2)
stargazer(reg_result_CH4, reg_result_N2O, reg_result_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "reg_results_92.html")

reg_result_CH4 <- lm(CH4 ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + LYP9 + YLY6, data = df92)
summary(reg_result_CH4)
reg_result_N2O <- lm(N2O ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + LYP9 + YLY6, data = df92)
summary(reg_result_N2O)
reg_result_CO2 <- lm(CO2 ~ N + SRNF + depth + ratio + densi + irrigFreq + temper + organ + LYP9 + YLY6, data = df92)
summary(reg_result_CO2)
stargazer(reg_result_CH4, reg_result_N2O, reg_result_CO2, title = "results", align = F, type = "text", no.space = TRUE, out = "reg_results_92_剔除多样性.html")
