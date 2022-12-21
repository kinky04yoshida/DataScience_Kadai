### 総合演習課題 ###

library(dplyr)

# csvファイルを読み込む
df <- read.csv("pitch_stats.csv", fileEncoding = "utf-8")
fourseam <- read.csv("fourseam.csv", fileEncoding = "utf-8")
sinker <- read.csv("sinker.csv", fileEncoding = "utf-8")
changeup <- read.csv("changeup.csv", fileEncoding = "utf-8")
curve <- read.csv("curve.csv", fileEncoding = "utf-8")
cutter <- read.csv("cutter.csv", fileEncoding = "utf-8")
slider <- read.csv("slider.csv", fileEncoding = "utf-8")
splitter <- read.csv("splitter.csv", fileEncoding = "utf-8")

# プレイヤーIDを基準に結合
df <- left_join(df, fourseam, by = "player_id")
df <- left_join(df, sinker, by = "player_id")
df <- left_join(df, changeup, by = "player_id")
df <- left_join(df, curve, by = "player_id")
df <- left_join(df, cutter, by = "player_id")
df <- left_join(df, slider, by = "player_id")
df <- left_join(df, splitter, by = "player_id")
head(df)

# 変化量の数値を絶対値に変換
df$ff_avg_break_x <- abs(df$ff_avg_break_x)
df$ff_avg_break_z <- abs(df$ff_avg_break_z)
df$sl_avg_break_x <- abs(df$sl_avg_break_x)
df$sl_avg_break_z <- abs(df$sl_avg_break_z)
df$ch_avg_break_x <- abs(df$ch_avg_break_x)
df$ch_avg_break_z <- abs(df$ch_avg_break_z)
df$cu_avg_break_x <- abs(df$cu_avg_break_x)
df$cu_avg_break_z <- abs(df$cu_avg_break_z)
df$si_avg_break_x <- abs(df$si_avg_break_x)
df$si_avg_break_z <- abs(df$si_avg_break_z)
df$fc_avg_break_x <- abs(df$fc_avg_break_x)
df$fc_avg_break_z <- abs(df$fc_avg_break_z)
df$fs_avg_break_x <- abs(df$fs_avg_break_x)
df$fs_avg_break_z <- abs(df$fs_avg_break_z)

### --- 重回帰分析 --- ###

#fourseam
df.fourseam = data.frame(df$ff_ba, df$player_id, df$ff_avg_speed, df$ff_avg_spin, df$ff_avg_break_x, df$ff_avg_break_z)
head(df.fourseam)
df.fourseam <- na.omit(df.fourseam)
head(df.fourseam)
fourseam.lm <- lm(df.fourseam$df.ff_ba~(df.fourseam$df.ff_avg_speed+df.fourseam$df.ff_avg_spin+df.fourseam$df.ff_avg_break_x+df.fourseam$df.ff_avg_break_z)^2, data = df)
summary(fourseam.lm)
fourseam.lm2 <- step(fourseam.lm)
summary(fourseam.lm2)

#slider
df.slider <- data.frame(df$player_id, df$sl_ba, df$sl_avg_speed, df$sl_avg_spin, df$sl_avg_break_x, df$sl_avg_break_z)
df.slider <- na.omit(df.slider)
slider.lm <- lm(df.slider$df.sl_ba~(df.slider$df.sl_avg_speed+df.slider$df.sl_avg_spin+df.slider$df.sl_avg_break_x+df.slider$df.sl_avg_break_z)^2)
summary(slider.lm)
slider.lm2 <- step(slider.lm)
summary(slider.lm2)

#changeup
df.changeup <- data.frame(df$player_id, df$ch_ba, df$ch_avg_speed, df$ch_avg_spin, df$ch_avg_break_x, df$ch_avg_break_z)
df.changeup <- na.omit(df.changeup)
changeup.lm <- lm(df.changeup$df.ch_ba~(df.changeup$df.ch_avg_speed+df.changeup$df.ch_avg_spin+df.changeup$df.ch_avg_break_x+df.changeup$df.ch_avg_break_z)^2)
summary(changeup.lm)
changeup.lm2 <- step(changeup.lm)
summary(changeup.lm2)

#curve
df.curve <- data.frame(df$player_id, df$cu_ba, df$cu_avg_speed, df$cu_avg_spin, df$cu_avg_break_x, df$cu_avg_break_z)
df.curve <- na.omit(df.curve)
curve.lm <- lm(df.curve$df.cu_ba~(df.curve$df.cu_avg_speed+df.curve$df.cu_avg_spin+df.curve$df.cu_avg_break_x+df.curve$df.cu_avg_break_z)^2)
summary(curve.lm)
curve.lm2 <- step(curve.lm)
summary(curve.lm2)

#sinker
df.sinker <- data.frame(df$player_id, df$si_ba, df$si_avg_speed, df$si_avg_spin, df$si_avg_break_x, df$si_avg_break_z)
df.sinker <- na.omit(df.sinker)
sinker.lm <- lm(df.sinker$df.si_ba~(df.sinker$df.si_avg_speed+df.sinker$df.si_avg_spin+df.sinker$df.si_avg_break_x+df.sinker$df.si_avg_break_z)^2)
summary(sinker.lm)
sinker.lm2 <- step(sinker.lm)
summary(sinker.lm2)

#cutter
df.cutter <- data.frame(df$player_id, df$fc_ba, df$fc_avg_speed, df$fc_avg_spin, df$fc_avg_break_x, df$fc_avg_break_z)
df.cutter <- na.omit(df.cutter)
cutter.lm <- lm(df.cutter$df.fc_ba~(df.cutter$df.fc_avg_speed+df.cutter$df.fc_avg_spin+df.cutter$df.fc_avg_break_x+df.cutter$df.fc_avg_break_z)^2)
summary(cutter.lm)
cutter.lm2 <- step(cutter.lm)
summary(cutter.lm2)

#splitter
df.splitter <- data.frame(df$player_id, df$fs_ba, df$fs_avg_speed, df$fs_avg_spin, df$fs_avg_break_x, df$fs_avg_break_z)
df.splitter <- na.omit(df.splitter)
splitter.lm <- lm(df.splitter$df.fs_ba~(df.splitter$df.fs_avg_speed+df.splitter$df.fs_avg_spin+df.splitter$df.fs_avg_break_x+df.splitter$df.fs_avg_break_z)^2)
summary(splitter.lm)
splitter.lm2 <- step(splitter.lm)
summary(splitter.lm2)
