library(tidyverse)
library(dplyr)
library(openxlsx)
library(stringr)
library(stargazer)
library(ggplot2)
master_data_new <- read.xlsx("master_d2.xlsx")

#(a) 記述統計
#1. 「(d) Master Dataの作成」で作成したデータの、各列に含まれるNAの数を数えなさい。
mas_na <- is.na(master_data_new)
mas_na_num <- colSums(mas_na)
print(mas_na_num)

#2. 問題背景などを知る上で役に立つ記述統計を作成しなさい
stargazer(master_data_new, type = "latex", out = "master_disc_table.tex", summary = TRUE)

#3. 4年卒業率の平均推移を計算し、図で示しなさい
mean_tot_grad_rate_year <- aggregate(tot_grad_rate ~ year, data = master_data_new, FUN = mean)
ggplot(data = mean_tot_grad_rate_year, aes(x = year, y= tot_grad_rate))+
  geom_line() +
  labs(x = "year", y = "4-year graduation rate", title = "Four-Year Graduation Rate") +
  scale_x_continuous(limits = c(1990,2010)) +
  scale_y_continuous(limits = c(.25,.45))

#4. semester導入率を計算し、図で示しなさい
sem_intro <- aggregate(semester ~ year, data = master_data_new, FUN = mean)
ggplot(data = sem_intro, aes(x = year, y= semester))+
  geom_line() +
  labs(x = "year", y = "Fraction of Schools on semesters", title = "Fraction of Schools on semesters") +
  scale_x_continuous(limits = c(1990,2010)) +
  scale_y_continuous(limits = c(.8,1))

#5. 以下の3つの変数を横軸、「4年卒業率」を縦軸にとった、散布図を作成しなさい。
#1. 女子学生比率
#2. 白人学生割合
#3. 学費(instatetuition)
