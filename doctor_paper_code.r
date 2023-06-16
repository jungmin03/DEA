install.packages("tidyverse")
install.packages("AER")
install.packages("PMCMRplus")
install.packages("pastecs")
install.packages("base")


library(lpSolve)
library(readxl)
library(writexl)
library(deaR)
library(Benchmarking)
library(tidyverse)
library(tidyverse)
library(base)



# PPP raw data 읽기
PPP_data <- read_excel("E:/dr_paper_data_analysis/PPP_Case1_R_test1&test2.xlsx")
names(PPP_data)

# dea data 정리
dea_data = subset(PPP_data, select=c("DMU", "X1", "X2", "X3", "Y1", "Y2", "Y3"))
dea_data

# 투입 및 산출변수 기술통계량, 상관관계(전체)
corr_data = subset(PPP_data, select=c("X1", "X2", "X3", "Y1", "Y2", "Y3"))
summary(corr_data) # 기술통계량(min, median, mean, max 등)
corr_data_mean = c(mean(corr_data$X1), mean(corr_data$X2), mean(corr_data$X3), mean(corr_data$Y1), mean(corr_data$Y2), mean(corr_data$Y3))
corr_data_sd_err = c(sd(corr_data$X1), sd(corr_data$X2), sd(corr_data$X3), sd(corr_data$Y1), sd(corr_data$Y2), sd(corr_data$Y3))
corr_data_max = c(max(corr_data$X1), max(corr_data$X2), max(corr_data$X3), max(corr_data$Y1), max(corr_data$Y2), max(corr_data$Y3))
corr_data_min = c(min(corr_data$X1), min(corr_data$X2), min(corr_data$X3), min(corr_data$Y1), min(corr_data$Y2), min(corr_data$Y3))
corr_data_re = data.frame(corr_data_mean, corr_data_sd_err, corr_data_max, corr_data_min)
dimnames(corr_data_re) = list(row=c("X1", "X2", "X3", "Y1", "Y2", "Y3"), col=c("mean", "sd_err", "max", "min"))
corr_data_re

cor(corr_data, method = 'pearson')
cor.test(corr_data$X1, corr_data$X2)
cor.test(corr_data$X1, corr_data$X3)
cor.test(corr_data$X1, corr_data$Y1)
cor.test(corr_data$X1, corr_data$Y2)
cor.test(corr_data$X1, corr_data$Y3)
cor.test(corr_data$X2, corr_data$X3)
cor.test(corr_data$X2, corr_data$Y1)
cor.test(corr_data$X2, corr_data$Y2)
cor.test(corr_data$X2, corr_data$Y3)
cor.test(corr_data$X3, corr_data$Y1)
cor.test(corr_data$X3, corr_data$Y2)
cor.test(corr_data$X3, corr_data$Y3)
cor.test(corr_data$Y1, corr_data$Y2)
cor.test(corr_data$Y1, corr_data$Y3)
cor.test(corr_data$Y2, corr_data$Y3)
cor.test(corr_data$Y3, corr_data$Y3)


# 투입 및 산출변수 기술통계량(type별)
corr_data_type = subset(PPP_data, subset = type =="KOR", select=c("type", "X1", "X2", "X3", "Y1", "Y2", "Y3")) # type에서 IMI, NCATS, KOR 중 택 1
count(corr_data_type)
corr_data_type_mean = c(mean(corr_data_type$X1), mean(corr_data_type$X2), mean(corr_data_type$X3), mean(corr_data_type$Y1), mean(corr_data_type$Y2), mean(corr_data_type$Y3))
corr_data_type_sd_err = c(sd(corr_data_type$X1), sd(corr_data_type$X2), sd(corr_data_type$X3), sd(corr_data_type$Y1), sd(corr_data_type$Y2), sd(corr_data_type$Y3))
corr_data_type_max = c(max(corr_data_type$X1), max(corr_data_type$X2), max(corr_data_type$X3), max(corr_data_type$Y1), max(corr_data_type$Y2), max(corr_data_type$Y3))
corr_data_type_min = c(min(corr_data_type$X1), min(corr_data_type$X2), min(corr_data_type$X3), min(corr_data_type$Y1), min(corr_data_type$Y2), min(corr_data_type$Y3))
corr_data_type_re = data.frame(corr_data_type_mean, corr_data_type_sd_err, corr_data_type_max, corr_data_type_min)
dimnames(corr_data_type_re) = list(row=c("X1", "X2", "X3", "Y1", "Y2", "Y3"), col=c("mean", "sd_err", "max", "min"))
corr_data_type_re

# deaR 분석을 위한 data 정리
dea_data_set <- make_deadata(dea_data, inputs = 2:4, outputs = 5:7)

# DEA 분석 code
eval_pft_crs <- model_basic(dea_data_set, orientation = "oo", rts = "crs")
eval_pft_vrs <- model_basic(dea_data_set, orientation = "oo", rts = "vrs")
eval_pft_sbm_crs <- model_sbmeff(dea_data_set, orientation = "oo", rts = "crs")
eval_pft_sbm_vrs <- model_sbmeff(dea_data_set, orientation = "oo", rts = "vrs")

# DEA 분석 결과 출력
summary(eval_pft_crs, exportExcel = TRUE, filename = "C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_crs.xlsx")
summary(eval_pft_vrs, exportExcel = TRUE, filename = "C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_vrs.xlsx")
summary(eval_pft_sbm_crs, exportExcel = TRUE, filename = "C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_sbm_crs.xlsx")
summary(eval_pft_sbm_vrs, exportExcel = TRUE, filename = "C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_sbm_vrs.xlsx")

eff_crs <- read_excel("C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_crs.xlsx")
eff_vrs <- read_excel("C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_vrs.xlsx")
eff_se <- read_excel("C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_crs.xlsx")
eff_sbm_crs <- read_excel("C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_sbm_crs.xlsx")
eff_sbm_vrs <- read_excel("C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/eval_pft_sbm_vrs.xlsx")

eff_crs$eff_rec <- 1/(eff_crs$eff)
eff_vrs$eff_rec <- 1/(eff_vrs$eff)

eff_se$eff <- eff_crs$eff/eff_vrs$eff
eff_se$eff_rec <- eff_crs$eff_rec/eff_vrs$eff_rec

eff_sbm_crs$eff_rec <- eff_sbm_crs$eff
eff_sbm_vrs$eff_rec <- eff_sbm_vrs$eff

result_eff <- data.frame(eff_crs$DMU, eff_crs$eff_rec, eff_vrs$eff_rec, eff_se$eff_rec, eff_sbm_crs$eff_rec, eff_sbm_vrs$eff_rec)
names(result_eff) <- c("DMU", "eff_crs", "eff_vrs", "eff_se", "eff_sbm_crs", "eff_sbm_vrs")
  
result_eff_m <- merge(PPP_data, result_eff, by='DMU')

result_eff_m$eff_crs <- round(result_eff_m$eff_crs, 5)
result_eff_m$eff_vrs <- round(result_eff_m$eff_vrs, 5)
result_eff_m$eff_se <- round(result_eff_m$eff_se, 5)
result_eff_m$eff_sbm_crs <- round(result_eff_m$eff_sbm_crs, 5)
result_eff_m$eff_sbm_vrs <- round(result_eff_m$eff_sbm_vrs, 5)

names(result_eff_m)

# 사업별 효율성 지수 평균값 및 중위값(전체)
eff_mean = c(mean(result_eff_m$eff_crs), median(result_eff_m$eff_crs), mean(result_eff_m$eff_vrs), median(result_eff_m$eff_vrs), mean(result_eff_m$eff_se), median
(result_eff_m$eff_se), mean(result_eff_m$eff_sbm_crs), median(result_eff_m$eff_sbm_crs), mean(result_eff_m$eff_sbm_vrs), median(result_eff_m$eff_sbm_vrs))
eff_mean
eff_mean_re = data.frame(eff_mean)
eff_mean_re
dimnames(eff_mean_re) = list(row=c("mean_crs", "median_crs", "mean_vrs", "median_vrs", "mean_se", "median_se", "mean_sbm_crs", "median_sbm_crs", "mean_sbm_vrs", "median_sbm_vrs"), col=c("eff"))
eff_mean_re


# 사업별 효율성 지수 평균값 및 중위값(type별)
result_eff_m_type = subset(result_eff_m, subset = type =="KOR") # type에서 IMI, NCATS, KOR 중 택 1
eff_mean_type = c(mean(result_eff_m_type$eff_crs), median(result_eff_m_type$eff_crs), mean(result_eff_m_type$eff_vrs), median(result_eff_m_type$eff_vrs), mean(result_eff_m_type$eff_se), median(result_eff_m_type$eff_se), mean(result_eff_m_type$eff_sbm_crs), median(result_eff_m_type$eff_sbm_crs), mean(result_eff_m_type$eff_sbm_vrs), median(result_eff_m_type$eff_sbm_vrs))
eff_mean_type_re = data.frame(eff_mean_type)
dimnames(eff_mean_type_re) = list(row=c("mean_crs", "median_crs", "mean_vrs", "median_vrs", "mean_se", "median_se", "mean_sbm_crs", "median_sbm_crs", "mean_sbm_vrs", "median_sbm_vrs"), col=c("eff"))
eff_mean_type_re

# 효율성 지수 구간별 연구과제 수
## type이 전체 시
temp = result_eff_m$eff_sbm_vrs # result_eff_m$eff_crs, result_eff_m$eff_vrs, result_eff_m$eff_se, result_eff_m$eff_sbm_crs, result_eff_m$eff_sbm_vrs 중 택 1

val_avg_1 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(temp == 1)

val_avg_2 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(temp < 1 & temp >= 0.5)

val_avg_3 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(temp < 0.5 & temp >0)

val_avg_4 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(temp == 0)

val_avg_sum = sum(count(val_avg_1), count(val_avg_2), count(val_avg_3), count(val_avg_4))
temp = c(count(val_avg_1), count(val_avg_2), count(val_avg_3), count(val_avg_4))
temp
val_avg_sum

## type이 선택 필요 시(IMI, NCATS, KOR)
temp = result_eff_m$eff_se # result_eff_m$eff_crs, result_eff_m$eff_vrs, result_eff_m$eff_se, result_eff_m$eff_sbm_crs, result_eff_m$eff_sbm_vrs 중 택 1
temp_type = "NCATS" # IMI, NCATS, KOR 중 택 1

val_avg_1 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp == 1)

val_avg_2 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp < 1 & temp >= 0.5)

val_avg_3 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp < 0.5 & temp > 0)
    
val_avg_4 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp == 0)

val_avg_sum = sum(count(val_avg_1), count(val_avg_2), count(val_avg_3), count(val_avg_4))

count(val_avg_1)
count(val_avg_2)
count(val_avg_3)
count(val_avg_4)
val_avg_sum


## type, 연구단계 선택 필요 시(IMI, NCATS, KOR)
temp = result_eff_m$eff_crs # result_eff_m$eff_crs, result_eff_m$eff_vrs, result_eff_m$eff_se, result_eff_m$eff_sbm_crs, result_eff_m$eff_sbm_vrs 중 택 1
temp_type = "KOR" # IMI, NCATS, KOR 중 택 1
temp_stage = "Drug_discovery" # Drug_discovery, Drug_development 중 택 1

val_avg_1 <- result_eff_m %>%
    select(DMU, type, stage3, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp == 1 & stage3 == temp_stage)

val_avg_2 <- result_eff_m %>%
    select(DMU, type, stage3, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp < 1 & temp >= 0.5 & stage3 == temp_stage)

val_avg_3 <- result_eff_m %>%
    select(DMU, type, stage3, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp < 0.5 & temp > 0 & stage3 == temp_stage)
    
val_avg_4 <- result_eff_m %>%
    select(DMU, type, stage3, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp == 0 & stage3 == temp_stage)

val_avg_sum = sum(count(val_avg_1), count(val_avg_2), count(val_avg_3), count(val_avg_4))

count(val_avg_1)
count(val_avg_2)
count(val_avg_3)
count(val_avg_4)
val_avg_sum

val_avg_1
val_avg_2

# 효율성 지수 구간별 투입 및 산출변수 평균
## 전체 단계
temp = result_eff_m$eff_sbm_vrs # result_eff_m$eff_crs, result_eff_m$eff_vrs, result_eff_m$eff_se, result_eff_m$eff_sbm_crs, result_eff_m$eff_sbm_vrs 중 택 1
temp_type = "KOR" # IMI, NCATS, KOR 중 택 1
names(result_eff_m)

## type 전체
val_avg_1 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(temp <= 1 & temp >= 0.5)

val_avg_2 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(temp < 0.5)

val_avg_type_1 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp <= 1 & temp >= 0.5)

val_avg_type_2 <- result_eff_m %>%
    select(DMU, type, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp < 0.5)

avg_x_y_1 = c(mean(val_avg_1$X1), mean(val_avg_1$X2), mean(val_avg_1$X3), mean(val_avg_1$Y1), mean(val_avg_1$Y2), mean(val_avg_1$Y3))
avg_x_y_2 = c(mean(val_avg_2$X1), mean(val_avg_2$X2), mean(val_avg_2$X3), mean(val_avg_2$Y1), mean(val_avg_2$Y2), mean(val_avg_2$Y3))
avg_x_y_type_1 = c(mean(val_avg_type_1$X1), mean(val_avg_type_1$X2), mean(val_avg_type_1$X3), mean(val_avg_type_1$Y1), mean(val_avg_type_1$Y2), mean(val_avg_type_1$Y3))
avg_x_y_type_2 = c(mean(val_avg_type_2$X1), mean(val_avg_type_2$X2), mean(val_avg_type_2$X3), mean(val_avg_type_2$Y1), mean(val_avg_type_2$Y2), mean(val_avg_type_2$Y3))

avg_x_y_1
avg_x_y_2
avg_x_y_type_1
avg_x_y_type_2


## stage3(연구단계) 선택 필요 시
temp = result_eff_m$eff_crs # result_eff_m$eff_crs, result_eff_m$eff_vrs, result_eff_m$eff_se, result_eff_m$eff_sbm_crs, result_eff_m$eff_sbm_vrs 중 택 1
temp_stage3 = "Drug_discovery" # Drug_discovery, Drug_development 중 택 1
temp_type = "IMI" # IMI, NCATS, KOR 중 택 1
names(result_eff_m)

val_avg_stage3_1 <- result_eff_m %>%
    select(DMU, type, stage3, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(temp <= 1 & temp >= 0.5 & stage3 == temp_stage3)

val_avg_stage3_2 <- result_eff_m %>%
    select(DMU, type, stage3, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(temp < 0.5  & stage3 == temp2)

val_avg_stage3_type_1 <- result_eff_m %>%
    select(DMU, type, stage3, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp <= 1 & temp >= 0.5 & stage3 == temp_stage3)

val_avg_stage3_type_2 <- result_eff_m %>%
    select(DMU, type, stage3, X1, X2, X3, Y1, Y2, Y3) %>%
    filter(type == temp_type & temp < 0.5 & stage3 == temp_stage3)

val_avg_stage3_type_1

avg_x_y_1 = c(mean(val_avg_stage3_1$X1), mean(val_avg_stage3_1$X2), mean(val_avg_stage3_1$X3), mean(val_avg_stage3_1$Y1), mean(val_avg_stage3_1$Y2), mean(val_avg_stage3_1$Y3))
avg_x_y_2 = c(mean(val_avg_stage3_2$X1), mean(val_avg_stage3_2$X2), mean(val_avg_stage3_2$X3), mean(val_avg_stage3_2$Y1), mean(val_avg_stage3_2$Y2), mean(val_avg_stage3_2$Y3))
avg_x_y_stage3_type_1 = c(mean(val_avg_stage3_type_1$X1), mean(val_avg_stage3_type_1$X2), mean(val_avg_stage3_type_1$X3), mean(val_avg_stage3_type_1$Y1), mean(val_avg_stage3_type_1$Y2), mean(val_avg_stage3_type_1$Y3))
avg_x_y_stage3_type_2 = c(mean(val_avg_stage3_type_2$X1), mean(val_avg_stage3_type_2$X2), mean(val_avg_stage3_type_2$X3), mean(val_avg_stage3_type_2$Y1), mean(val_avg_stage3_type_2$Y2), mean(val_avg_stage3_type_2$Y3))

avg_x_y_1
avg_x_y_2
avg_x_y_stage3_type_1
avg_x_y_stage3_type_2



#연구과제 수
c(length(val_avg$type), length(which(val_avg$type=="IMI")), length(which(val_avg$type=="NCATS")), length(which(val_avg$type=="KOR")))

#투입 및 산출변수 평균
sub_val_avg <- subset(val_avg, select=c("X1", "X2", "X3", "Y1", "Y2", "Y3"))
apply(sub_val_avg, 2, mean)
aggregate(cbind(X1, X2, X3, Y1, Y2, Y3) ~ type, val_avg, mean)


# 사업-연구단계별 효율성 평균

result_eff_IMI_m <- result_eff_m %>%
    filter(result_eff_m$type == "IMI")

result_eff_NCATS_m <- result_eff_m %>%
    filter(result_eff_m$type == "NCATS")

result_eff_KOR_m <- result_eff_m %>%
    filter(result_eff_m$type == "KOR")

write_xlsx(result_eff_m, path="C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/result_eff.xlsx")
write_xlsx(result_eff_IMI_m, path="C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/result_eff_IMI.xlsx")
write_xlsx(result_eff_NCATS_m, path="C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/result_eff_NCATS.xlsx")
write_xlsx(result_eff_KOR_m, path="C:/Users/Jungmin Song/내 드라이브/1.KU MOT/3. 논문/(20230612)학위논문 실험/dea result/result_eff_KOR.xlsx")


stage_avg <- result_eff_m %>%
    select(DMU, stage3, stage4, eff_crs, eff_vrs, eff_se, eff_sbm_crs, eff_sbm_vrs) %>%
    filter(result_eff_m$stage3 == "Drug_discovery")

sub_stage_avg <- subset(stage_avg, select=c("eff_crs", "eff_vrs", "eff_sbm_crs", "eff_sbm_vrs"))
apply(sub_stage_avg, 2, median)
aggregate(cbind(eff_crs, eff_vrs, eff_sbm_crs, eff_sbm_vrs) ~ stage4, stage_avg, median)

# 사업-연구단계별 효율성 분류
names(result_eff_m)

# 대분류 및 type 별 분류
for (i in c("total_s3", "Drug_discovery", "Drug_development") ) {
    for (j in c('total_type', "IMI", "NCATS", "KOR") ) {
        if (i=="total_s3") {
            if (j=="total_type") {
                # temp <- subset(result_eff_m, select=c("DMU", "stage3", "stage4", "eff_crs", "eff_vrs", "eff_se", "eff_sbm_crs", "eff_sbm_vrs"))
                eff_total_s3_total_type = result_eff_m
            } else {
                temp <- result_eff_m %>%
                    #select(DMU, stage3, stage4, eff_crs, eff_vrs, eff_se, eff_sbm_crs, eff_sbm_vrs) %>%
                    filter(result_eff_m$type == j)
                assign(paste0("eff_total_s3_", j), temp)   
            } 
        } else if (j=="total_type") {
            temp <- result_eff_m %>%
                #select(DMU, stage3, stage4, eff_crs, eff_vrs, eff_se, eff_sbm_crs, eff_sbm_vrs) %>%
                filter(result_eff_m$stage3 == i)
            assign(paste0("eff_",i,"_total_type"), temp)
        } else {
            temp <- result_eff_m %>%
                #select(DMU, stage3, stage4, eff_crs, eff_vrs, eff_se, eff_sbm_crs, eff_sbm_vrs) %>%
                filter(result_eff_m$stage3 == i & result_eff_m$type == j)
            assign(paste0("eff_",i,"_",j), temp)
        }
    }
}


names(result_eff_m)
names(eff_total_s3_KOR)
names(eff_Drug_discovery_total_type)
names(eff_Drug_development_KOR)


re <- c('eff_total_s3_total_type', 'eff_total_s3_IMI', 'eff_total_s3_NCATS', 'eff_total_s3_KOR', 'eff_Drug_discovery_total_type', 'eff_Drug_discovery_IMI', 'eff_Drug_discovery_NCATS', 'eff_Drug_discovery_KOR', 'eff_Drug_development_total_type', 'eff_Drug_development_IMI', 'eff_Drug_development_NCATS', 'eff_Drug_development_KOR')

s4 <- c('total_s4', 'Target_selection&validation', 'Hit', 'Lead', 'Candidate', 'Pre-clinical', 'clinical')

# 사업 및 연구단계별 과제 개수
for(i in 1:length(re)){
    cat(re[i], ":")
    print(nrow(eval(parse(text = re[i]))))
}


# kruskal-wallis test

# kruskal.test(list(eff_total_s3_IMI$eff_crs, eff_total_s3_NCATS$eff_crs, eff_total_s3_KOR$eff_crs))
# kruskal.test(list(eff_total_s3_IMI$eff_vrs, eff_total_s3_NCATS$eff_vrs, eff_total_s3_KOR$eff_vrs))

# kruskal.test(list(eff_total_s3_IMI$eff_sbm_crs, eff_total_s3_NCATS$eff_sbm_crs, eff_total_s3_KOR$eff_sbm_crs))    
# kruskal.test(list(eff_total_s3_IMI$eff_sbm_vrs, eff_total_s3_NCATS$eff_sbm_vrs, eff_total_s3_KOR$eff_sbm_vrs))


# Wilcoxon-Mann-Whitney test
# wilcox_imi <- wilcox.test(eff_Drug_discovery_IMI$eff_sbm_vrs, eff_Drug_development_IMI$eff_sbm_vrs)
# wilcox_ncats <- wilcox.test(eff_Drug_discovery_NCATS$eff_sbm_vrs, eff_Drug_development_NCATS$eff_sbm_vrs)
# wilcox_kor <- wilcox.test(eff_Drug_discovery_KOR$eff_sbm_vrs, eff_Drug_development_KOR$eff_sbm_vrs)

# list(wilcox_imi, wilcox_ncats, wilcox_kor)

# Tamhane T2 test
# ty <- c(eff_total_s3_IMI$eff_crs, eff_total_s3_NCATS$eff_crs, eff_total_s3_KOR$eff_crs)
# ty2 <- c("IMI", "NCATS", "KOR")
# summary(aov(ty ~ ty2))

# Tobit regression
# names(eff_total_s3_total_type)

# tobit_inst <- lm(eff_sbm_vrs ~ proj_coordi_reinst, data=eff_total_s3_IMI)
# tobit_com <- lm(eff_sbm_vrs ~ proj_coordi_com, data=eff_total_s3_IMI)
# tobit_univ <- lm(eff_sbm_vrs ~ proj_coordi_univ, data=eff_total_s3_IMI)
# tobit_hosp <- lm(eff_sbm_vrs ~ proj_coordi_hosp, data=eff_total_s3_IMI)


# summary(tobit_inst)
# summary(tobit_com)
# summary(tobit_univ)
# summary(tobit_hosp)

