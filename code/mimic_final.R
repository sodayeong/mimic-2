setwd('/Users/dayeong/Desktop/대학/22-1/비정형데이터분석/data/MIMIC2csv')

library(stringr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(plyr)
library(randomForest)
library(caret)
library(nnet)
library(rpart)
library(keras)

# 데이터 로딩
chartevents <- read.csv('chartevents.csv') # 환자 생체신호 데이터
d_chartitems <- read.csv('d_chartitems.csv') # ICU에 머무르며, 환자의 모든 생체 신호 데이터를 포함 하고 있음
icd9 <- read.csv('icd9.csv')
d_patients <- read.csv('d_patients.csv')
admissions <- read.csv('admissions.csv') # 환자의 입원정보
head(d_patients)
# chartevents > 필요 행 추출
head(chartevents)
chartevents = chartevents %>% select('subject_id', 'icustay_id', 'itemid', 'charttime', 'elemid', 'value1')

# 패혈증 환자(0/1) 클래스 생성 in icd9 
icd9$Class=ifelse(str_detect(icd9$description,'SEPTICEMIA')==TRUE, 1, 0) # 패혈증 클래스 생성 
table(icd9$Class)

# merge
data_1 <- NULL
data_1 = merge(icd9, admissions, by=c('subject_id', 'hadm_id'))
#data_1$admit_dt <- as.Date(data_1$admit_dt, format = '%d/%m/%Y')
#data_1$disch_dt <- as.Date(data_1$disch_dt, format = '%d/%m/%Y')

head(data_1)
str(data_1)

# 입원 시점의 데이터 & Class 추출
data_2 = data_1 %>% select('subject_id', 'hadm_id', 'code', 'Class', 'admit_dt')  
head(data_2)

time_1 = data_2 %>% select('subject_id', 'admit_dt')
time_2 = data_1 %>% select('admit_dt')
head(time_1)
head(time_2)

# 헬스케어 데이터 추출

# 심박수 데이터 > 70~140
chart_HR = subset(chartevents, itemid=='211')
chart_HR$value1 <- as.numeric(chart_HR$value1) 
head(chart_HR)

summary(chart_HR$value1)
nrow(chart_HR)
chart_HR$value1 <- ifelse(chart_HR$value1>140 | chart_HR$value1<70, NA, chart_HR$value1)
chart_HR <- na.omit(chart_HR) # 결측치 제거

# 혈압 데이터 > 120~80
chart_BP = subset(chartevents, itemid=='51')
chart_BP$value1 <- as.numeric(chart_BP$value1) 

summary(chart_BP$value1)
nrow(chart_BP)
chart_BP$value1 <- ifelse(chart_BP$value1>120 | chart_BP$value1<80, NA, chart_BP$value1)
chart_BP <- na.omit(chart_BP) # 결측치 제거

# 호흡률 데이터 > 12~20
chart_RR = subset(chartevents, itemid=='618')
chart_RR$value1 <- as.numeric(chart_RR$value1) 

summary(chart_RR$value1)
nrow(chart_RR)
chart_RR$value1 <- ifelse(chart_RR$value1<12 | chart_RR$value1>30, NA, chart_RR$value1)
chart_RR <- na.omit(chart_RR) # 결측치 제거

# 체온 데이터 > 34~40
chart_BT = subset(chartevents, itemid=='676')
chart_BT$value1 <- as.numeric(chart_BT$value1) 

summary(chart_BT$value1)
nrow(chart_BT)
chart_BT$value1 <- ifelse(chart_BT$value1<36 | chart_BT$value1>38, NA, chart_BT$value1)
chart_BT <- na.omit(chart_BT) # 결측치 제거
head(chart_BT)

colnames(chart_HR)[6] <- 'Heart Rate'
colnames(chart_RR)[6] <- 'Respiratory Rate'
colnames(chart_BP)[6] <- 'Blood Pressure'
colnames(chart_BT)[6] <- 'Boby Temperature'

chart_HR <- chart_HR %>% select('subject_id','charttime','Heart Rate')
chart_RR <- chart_RR %>% select('subject_id','charttime','Respiratory Rate')
chart_BP <- chart_BP %>% select('subject_id','charttime', 'Blood Pressure')
chart_BT <- chart_BT %>% select('subject_id','charttime', 'Boby Temperature')

head(chart_HR)
head(chart_RR)
head(chart_BP)
head(chart_BT)


# result 
chart_result_1 = inner_join(chart_HR, chart_RR, by=c('subject_id','charttime'), no.dups = FALSE)
head(chart_result_1)

chart_result_2 = inner_join(chart_result_1, chart_BP,by=c('subject_id','charttime'), no.dups = FALSE)
head(chart_result_2)

chart_result_3 = inner_join(chart_result_2, chart_BT,by=c('subject_id','charttime'), no.dups = FALSE)
head(chart_result_3)


# result + Class 조인 
icd_class <- icd9 %>% select('subject_id', 'Class') %>% unique()
head(table(icd_class$Class))

result_data <- inner_join(chart_result_3, icd_class, by='subject_id', no.dups = FALSE)
head(result_data)
nrow(result_data)
