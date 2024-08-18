library(tidyverse)
library(dplyr)
library(openxlsx)
library(stringr)

#(a) Semester Dataの整形
#1. 生データを読み込みなさい (semester_dummy_1.csv, semester_dummy_2.csv)
data1 <- read.csv("semester_data_1.csv", skip = 1, header = TRUE )
data2 <- read.csv("semester_data_2.csv")
str(data1)
str(data2)

#2. semester_dummy_1.csvについては、1行目を列名としなさい
colnames(data2) <- colnames(data1)

#3. 2つのデータを適切に結合しなさい
data_combined <- bind_rows(data1, data2)

#4. ’Y’列を削除しなさい
data_cleaned <- select(data_combined, -Y)

#5. semester制が導入された年の列を作成しなさい。
data_cleaned <- data_cleaned %>%
  group_by(unitid) %>%
  mutate(
    ChangeYear_Dummy1 = ifelse(lag(semester, default = first(semester)) == 0 & semester == 1, year, NA)
  ) %>%
  ungroup() %>%
  group_by(unitid) %>%
  mutate(
    ChangeYear = first(na.omit(c(ChangeYear_Dummy1)))
  ) %>%
  ungroup() %>%
  select(-ChangeYear_Dummy1) 

#6. 5.を用いてsemester制導入後を示すダミー変数を作成しなさい
#2001年にsemester制が導入された場合、1991~2000年は0, 2001年以降は1となる変数
data_comp <- data_cleaned %>%
  mutate(Dummy_year = ifelse(year >= ChangeYear, 1, 0))

#(b) Gradrate Dataの整形
#1. 生データを読み込み、適切に結合しなさい
#2011以降はファイルはあるがデータなし
comb_grad <- data.frame()
for (year in 1991:2016) {
  if (year == 1994) next
  file <- paste0(year, ".xlsx")
  grad_data <- read.xlsx(file)
  comb_grad <- bind_rows(comb_grad, grad_data)
}

#2. 女子学生の4年卒業率に0.01をかけて、0から1のスケールに変更しなさい
comb_grad <- comb_grad %>% 
  mutate(women_gradrate_4yr = women_gradrate_4yr *0.01)

#3. 男女合計の4年卒業率と男子学生の4年卒業率を計算し、新たな列として追加しなさい
str(comb_grad)

#numにtypeを変更
comb_grad <- comb_grad %>% 
  mutate(tot4yrgrads = as.numeric(tot4yrgrads),
         totcohortsize = as.numeric(totcohortsize),
         m_4yrgrads = as.numeric(m_4yrgrads))

#卒業率の計算
comb_grad <- comb_grad %>% 
  mutate(tot_grad_rate = tot4yrgrads / totcohortsize,
         m_gradrate_4yr = m_4yrgrads / m_cohortsize)

#4. 計算した卒業率を有効数字3桁に調整しなさい
comb_grad <- comb_grad %>% 
  mutate(women_gradrate_4yr = round(women_gradrate_4yr, digits = 3),
         tot_grad_rate = round(tot_grad_rate, digits = 3),
         m_gradrate_4yr = round(m_gradrate_4yr, digits = 3))

#5. 1991年から2010年までのデータフレームに変形しなさい
#最初から２０１０年までのものになっていたのでパス

#(c) Covariates Dataの整形
#1. 生データを読み込みなさい (covariates.xlsx)
cov <- read.xlsx("covariates.xlsx")

#2. ’university_id’という列名を’unitid’に変更しなさい
cov <- cov %>% 
  rename(unitid = university_id)

#3. ’unitid’に含まれる”aaaa”という文字を削除しなさい
cov <- cov %>% 
  mutate(unitid = str_remove(unitid, "aaaa"))
  
#4. ‘category’列に含まれる’instatetuition’, ‘costs’, ’faculty’, ’white_cohortsize’
#   を別の列として追加しなさい(wide型に変更しなさい)
cov <- cov %>% 
  pivot_wider(
    names_from = category,
    values_from = value)

#5. outcomeやsemester_dummyに含まれる年を調べ、covariatesデータの期間を他のデータに揃えなさい
cov_fil <- cov %>% 
  mutate(year = as.numeric(year)) %>% 
  subset(year >= 1991 & year <= 2010)

#6. outcome_dataに含まれるunitidを特定し、covariatesに含まれるunitidをoutcomeデータに揃えなさい
cov_fil <- comb_grad %>%
  filter(unitid %in% cov_fil$unitid)

#(d) Master Dataの作成
#1. 結合に用いる変数を考え、semester_data, covariates_data, gradrate_dataを適切に結合しなさい
cov_fil <- cov_fil %>%
  arrange(unitid)

comb_grad <- comb_grad %>% 
  arrange(unitid)

master_data <- left_join(data_comp, cov_fil, comb_grad, by = c("unitid", "year"))
write.xlsx(master_data, file = "master_d2.xlsx")
getwd()
