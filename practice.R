## DACON2020 EDA
## 주제 : 일자별 사용자, 세션, 신규방문자, 페이지뷰 산출

# load package
package = c('tidyverse','magrittr','lubridate','GGally','showtext','extrafont','gam')
lapply(package, library, character.only = TRUE)
# font load
font_add("NanumBarunGothic", "NanumBarunGothic.ttf")
showtext_auto()

setwd('C:/Users/USER/Desktop/프로그래밍 연습/DACON/2020 DACON CUP GA 데이터/data')
dir()

#########################################
######### 1.Data Preparation ##########
#########################################
train1 = read.csv('train.csv')
train2 = read.csv('2차_train.csv')

train = bind_rows(train1, train2)
train %<>% mutate(DateTime = ymd_hms(DateTime))
dim(train)
str(train)
head(train)

#
login = read.csv('new_login_info.csv')[-1]
login %<>% mutate(c_time = ymd_hm(c_time))
dim(login)
str(login)

# 
competition = read.csv('new_competition_info.csv')[-1]
competition %>% head()
str(competition)

# 
submission = read.csv('new_submission_info.csv')[-1]
head(submission)
submission %<>% mutate(c_time = ymd_hm(c_time))
str(submission)

#
user = read.csv('new_user_info.csv')[-1]
head(user)
user %<>% mutate(c_time = ymd_hm(c_time))
str(user)

## 데이터 가공
train %>% head()
# train데이터 날짜별로 합하기
new_train = train %>% 
  mutate(DateTime = as.Date(DateTime)) %>% 
  group_by(DateTime) %>% 
  summarise_all(sum)

# train데이터 분포확인
ggpairs(new_train[-1],
        diag = list(continuous = wrap('densityDiag', size=1, colour='tomato')),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.8, colour='tomato')))

ggcorr(new_train[-1], nbreaks=10, palette='RdBu', label=TRUE, label_size=5, label_color='white')

#########################################
############## 2.EDA ####################
#########################################
colnames(login) ; colnames(submission); colnames(competition) ; colnames(user)
login %>% head
# 한 사람이 하루에 중복로그인 횟수 확인
login %>% 
  mutate(c_time = as.Date(c_time)) %>% 
  group_by(user_id, c_time) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  na.omit() %>%
  ungroup() %>% 
  select(count) %>% 
  apply(.,2,function(x) quantile(x,probs=c(0.5,0.8,0.9,0.95,0.99,1)))

