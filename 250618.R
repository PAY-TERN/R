install.packages("lubridate")
install.packages("readxl")
# 필요한 패키지
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

theme_set(theme_minimal(base_family = "AppleGothic"))

# 엑셀 파일 불러오기
df <- read_excel("../Data/hj_cheongju-pay.xlsx", sheet = "Classification_Results")

# 날짜 형식 변환
df <- df %>%
  mutate(이용일자 = as.Date(substr(이용일자, 1, 10)),  # 시간 제거
         월 = floor_date(이용일자, "month"),
         분기 = paste0(year(이용일자), " Q", quarter(이용일자)))

# 숫자형 변환
df$이용금액 <- as.numeric(df$이용금액)

# 1. 카테고리별 소비 합계
카테고리_소비 <- df %>%
  group_by(카테고리) %>%
  summarise(총이용금액 = sum(이용금액, na.rm = TRUE)) %>%
  arrange(desc(총이용금액))

월별_소비 <- 월별_소비 %>%
  filter(!is.na(카테고리))

# 카테고리별 소비 시각화
ggplot(카테고리_소비, aes(x = reorder(카테고리, 총이용금액), y = 총이용금액)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "카테고리별 소비 금액", x = "카테고리", y = "총 이용 금액") +
  scale_y_continuous(labels = comma) +  # 콤마로 보기 쉽게
  theme_minimal(base_family = "AppleGothic")

# 2. 월별 소비 추이
월별_소비 <- df %>%
  group_by(월) %>%
  summarise(총이용금액 = sum(이용금액, na.rm = TRUE))

월별_소비 <- 월별_소비 %>%
  mutate(월표시 = format(월, "%y.%m"))

월별_소비 <- 월별_소비 %>%
  filter(!is.na(월)) %>%
  mutate(월표시 = format(월, "%y.%m"))

# 월별 소비 시각화
ggplot(월별_소비, aes(x = 월표시, y = 총이용금액, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "월별 소비 추이", x = "월", y = "이용 금액") +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 카테고리별 월별 소비 비중 분석 (비율)
df <- df %>%
  filter(!is.na(카테고리), !is.na(월))

카테고리_월별_비중 <- df %>%
  group_by(월, 카테고리) %>%
  summarise(카테고리_이용금액 = sum(이용금액, na.rm = TRUE)) %>%
  group_by(월) %>%
  mutate(비중 = 카테고리_이용금액 / sum(카테고리_이용금액)) %>%
  ungroup() %>%
  mutate(월표시 = format(월, "%y.%m"))  # 연.월 문자열 생성

# Stacked bar chart
최고비중_카테고리 <- 카테고리_월별_비중 %>%
  group_by(월표시) %>%
  slice_max(order_by = 비중, n = 1) %>%
  ungroup()


ggplot(카테고리_월별_비중, aes(x = 월표시, y = 비중, fill = 카테고리)) +
  geom_bar(stat = "identity") +
  # 카테고리명을 라벨로 표시
  geom_text(data = 최고비중_카테고리,
            aes(label = 카테고리, y = 비중 + 0.03),  # 막대 위 살짝 띄움
            size = 3,
            color = "black",
            family = "AppleGothic") +
  labs(title = "월별 카테고리 소비 비중", x = "월", y = "비중") +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. 분기별 소비 추이
분기별_소비 <- df %>%
  group_by(분기) %>%
  summarise(총이용금액 = sum(이용금액, na.rm = TRUE),
            평균이용금액 = mean(이용금액, na.rm = TRUE)) %>%
  arrange(분기) %>%
  mutate(전분기대비증감률 = round((총이용금액 - lag(총이용금액)) / lag(총이용금액) * 100, 2))

# 분기별 소비 금액 시각화
ggplot(분기별_소비, aes(x = 분기, y = 총이용금액, group = 1)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 3, color = "forestgreen") +
  labs(title = "분기별 소비 추이", x = "분기", y = "총 이용 금액") +
  theme_minimal(base_family = "AppleGothic")

# 전분기 대비 증감률 출력
print(분기별_소비)

