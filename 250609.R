# 250609 월
library(dplyr)


# cheongju-pay data preprocessing
cp = read.csv("../Data/cheongju-pay.csv")
sum(cp)
head(cp)

# 중복 행 확인

# 카테고리 + 상호명 + 주소
duplicated_rows_a = duplicated(cp[, c("카테고리", "상호명", "주소")])
sum(duplicated_rows_a)
# 559개

# 카테고리 + 상호명 + 주소 + 업종
duplicated_rows_b = duplicated(cp[, c("카테고리", "상호명", "주소", "업종")])
sum(duplicated_rows_b)

# 차이 나는 인덱스 추출
diff_index <- which(duplicated_rows_a & !duplicated_rows_b)

# 해당 행 보기
diff_rows <- cp[diff_index, ]
head(diff_rows)

# 중복된 조합
dup_summary <- cp %>%
  group_by(카테고리, 상호명, 주소) %>%
  tally() %>%
  filter(n > 1) %>%
  arrange(desc(n))
dup_summary

# 중복 제거
cp_unique = cp[!duplicated(cp[, c("카테고리", "상호명", "주소")]), ]

# 중복 제거 전후 차이 비교
nrow(cp)
nrow(cp_unique)

table(cp_unique$카테고리)

write.csv(cp_unique, "../Data/cheongju-pay_u.csv", row.names = FALSE, quote = FALSE)

# 전화번호 확인
cp_unique %>%
  filter(grepl("000-0000$", 전화번호)) -> phone_000s

# 확인
head(phone_000s)
nrow(phone_000s)

