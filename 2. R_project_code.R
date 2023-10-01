setwd('C:\\project\\R_Source')

# ============================================================================
# <1. 데이터 셋 로드>

### 1) 서식코드
type.treatment <- read.csv('서식코드.csv', header = T)
type.treatment

### 2) 연령대코드
age <- read.csv('연령대코드.csv', header = T)
age
str(age)

### 3) 주상병코드
sickness <- read.csv('주상병코드.csv', header = T)
sickness

### 4) 진료과목코드
medical.department <- read.csv('진료과목코드.csv', header = T)
medical.department

### 5) 2019년 데이터셋
ds.2019 <- read.csv('NHIS_INCHON_2019.csv', header = T)
ds.2019

### 6) 2020년 데이터셋
ds.2020 <- read.csv('NHIS_INCHON_2020.csv', header = T)
ds.2020
# ============================================================================
# <2. R 시각화 분석>

# 1) 데이터 분석 : 수진자 연령대 분포(수진자는 진료자를 의미)
## - 2019년도 데이터

str(ds.2019)

df <- ds.2019
df
dim(df)

length(df$가입자.일련번호) # 719618개


# 중복된 값 제거를 위한 패키지 설치
install.packages("dplyr") 
library(dplyr)


# 가입자 일련번호 중복제거한 새로운 df생성
df_unique <- distinct(df, 가입자.일련번호, .keep_all = TRUE)
length(df_unique$가입자.일련번호) # 58084개


# df_unique$연령대코드와 age$코드 매칭
matching_rows <- which(df_unique$연령대코드 %in% age$코드)

df_unique$group.age <- NA
df_unique$group.age[matching_rows] <- age$연령대[match(df_unique$연령대코드[matching_rows], age$코드)]
df_unique

# 나이 범주 레벨 재설정 --> 5~9세가 오름차순 시 뒤로 밀려있기 때문
df_unique$group.age <- factor(df_unique$group.age, levels = c("0~4세", "5~9세", "10~14세", "15~19세", "20~24세", "25~29세", "30~34세", "35~39세", "40~44세", "45~49세", "50~54세", "55~59세", "60~64세", "65~69세", "70~74세", "75~79세", "80~84세", "85세+"))

age_counts <- table(df_unique$group.age)
age_counts

install.packages("ggplot2")
library(ggplot2)

ggplot(df_unique, aes(x = 가입자.일련번호, y = group.age)) +
  geom_bar(stat = 'identity', 
           width = 0.5, 
           fill = 'steelblue') +
  ggtitle('수진자 연령대 분포') +
  theme(plot.title = element_text(size = 25, face = 'bold')) +
  labs(x = '수진자수', y = '연령대')
# ============================================================================
# 2) 데이터 분석 : 진료과별 진료횟수
## - 상위 10개
## - 2019년도 데이터

df_medi <- ds.2019
df_medi

df_medi$group.medi <- NA

# 진료코드 매칭하여 진료과 데이터 넣기
matching_rows <- which(df_medi$진료과목코드 %in% medical.department$코드)
df_medi$group.medi[matching_rows] <- medical.department$진료과[match(df_medi$진료과목코드[matching_rows], medical.department$코드)]
df_medi

# 진료과별 빈도 계산
dept_freq <- table(df_medi$group.medi)
dept_freq

# 빈도 기준으로 내림차순 정렬하여 상위 10개 선택
top_10_depts <- names(head(sort(dept_freq, decreasing = TRUE), 10))
top_10_depts

# 데이터프레임에서 상위 10개 진료과 필터링
df_top_10_depts <- df_medi[df_medi$group.medi %in% top_10_depts, ]
df_top_10_depts

# 상위 10개 진료과 내림차순 정렬
df_top_10_depts$group.medi <- factor(df_top_10_depts$group.medi, levels = rev(top_10_depts))
df_top_10_depts

ggplot(df_top_10_depts, aes(x = group.medi)) +
  geom_bar(fill = 'steelblue') +
  ggtitle('진료과별 진료횟수') +
  theme(plot.title = element_text(size = 20, face = 'bold')) +
  labs(x = '진료과', y = '진료횟수') +
  coord_flip()
# ============================================================================
# 3-1) 코로나19 전후 비교 분석 : 연도별 수진자 연령대 분포
## <2019년도 데이터>
df_age_19 <- ds.2019
df_age_19
dim(df_age_19)

df_age_19 <- distinct(df_age_19, 가입자.일련번호, .keep_all = TRUE)
length(df_age_19$가입자.일련번호) # 58084개

df_age_19$연령대 <- NA

matching_rows <- which(df_age_19$연령대코드 %in% age$코드)
df_age_19$연령대[matching_rows] <- age$연령대[match(df_age_19$연령대코드[matching_rows], age$코드)]
df_age_19

df_age_19$연령대 <- factor(df_age_19$연령대, levels = c("0~4세", "5~9세", "10~14세", "15~19세", "20~24세", "25~29세", "30~34세", "35~39세", "40~44세", "45~49세", "50~54세", "55~59세", "60~64세", "65~69세", "70~74세", "75~79세", "80~84세", "85세+"))


## <2020년도 데이터>
df_age_20 <- ds.2020
df_age_20
dim(df_age_20)

df_age_20 <- distinct(df_age_20, 가입자.일련번호, .keep_all = TRUE)
length(df_age_20$가입자.일련번호) # 57776개

df_age_20$연령대 <- NA

matching_rows <- which(df_age_20$연령대코드 %in% age$코드)
df_age_20$연령대[matching_rows] <- age$연령대[match(df_age_20$연령대코드[matching_rows], age$코드)]
df_age_20

df_age_20$연령대 <- factor(df_age_20$연령대, levels = c("0~4세", "5~9세", "10~14세", "15~19세", "20~24세", "25~29세", "30~34세", "35~39세", "40~44세", "45~49세", "50~54세", "55~59세", "60~64세", "65~69세", "70~74세", "75~79세", "80~84세", "85세+"))

# 연령대별 진료자 수 계산
patient_count_19 <- table(df_age_19$연령대)
patient_count_19
patient_count_20 <- table(df_age_20$연령대)
patient_count_20

# 데이터프레임 생성
df_patient_count <- data.frame(
  연령대 = factor(names(patient_count_19), levels = names(patient_count_19)),
  기준년도 = rep(c("2019", "2020"), each = length(patient_count_19)),
  수진자수 = c(as.vector(patient_count_19), as.vector(patient_count_20))
)
df_patient_count

# 선 그래프 그리기
ggplot(df_patient_count, aes(x = 연령대, y = 수진자수, color = 기준년도, group = 기준년도)) +
  geom_line(size = 1) +
  geom_point(size = 6, shape = 19, alpha = 0.5) +
  ggtitle('연도별 수진자 연령대 분포') +
  theme(plot.title = element_text(size = 20, face = 'bold')) +
  labs(x = '연령대', y = '수진자수') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ============================================================================

# 3-2) 코로나19 전후 비교 분석 : 연도별 진료과별 진료횟수 분포
df_medi_19 <- ds.2019
df_medi_19

df_medi_19$진료과 <- NA

df_medi_20 <- ds.2020
df_medi_20

df_medi_20$진료과 <- NA


matching_rows <- which(df_medi_19$진료과목코드 %in% medical.department$코드)
df_medi_19$진료과[matching_rows] <- medical.department$진료과[match(df_medi_19$진료과목코드[matching_rows], medical.department$코드)]
df_medi_19


matching_rows <- which(df_medi_20$진료과목코드 %in% medical.department$코드)
df_medi_20$진료과[matching_rows] <- medical.department$진료과[match(df_medi_20$진료과목코드[matching_rows], medical.department$코드)]
df_medi_20

# 진료과별 진료 횟수 계산
dept_freq_19 <- table(df_medi_19$진료과)
dept_freq_19
dept_freq_20 <- table(df_medi_20$진료과)
dept_freq_20

# 진료과 개수가 동일한 진료과 추출
common_depts <- intersect(names(dept_freq_19), names(dept_freq_20))

# 데이터프레임 생성
df_dept_freq <- data.frame(
  진료과 = factor(common_depts, levels = common_depts),
  기준년도 = rep(c("2019", "2020"), each = length(common_depts)),
  진료횟수 = c(dept_freq_19[common_depts], dept_freq_20[common_depts])
)

# 선 그래프 그리기
ggplot(df_dept_freq, aes(x = 진료과, y = 진료횟수, color = 기준년도, group = 기준년도)) +
  geom_line(size = 1) +
  geom_point(size = 6, shape = 19, alpha = 0.5) +
  ggtitle('연도별 진료과별 진료횟수 분포') +
  theme(plot.title = element_text(size = 20, face = 'bold')) +
  labs(x = '진료과', y = '진료횟수') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
