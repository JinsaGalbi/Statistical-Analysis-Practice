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
        
#########################################
####### 3.Feature Extraction#############
#########################################

############# 3-1. login ################
login %>% is.na() %>% sum
login %>% 
  apply(1, function(x) any(is.na(x))) %>% 
  sum()
login %>% 
  apply(2, function(x) sum(is.na(x)))
# c_time 결측치는 모두 삭제
login %<>% filter(!login %>%select(c_time) %>%  is.na())

# login_id와 user_id의 차이 존재
## => 같은 user_id가 여러 login_id 가짐
## => 같은 login_id는 하나의 user_id 가짐
login %>%
  group_by(user_id) %>% 
  summarise(count = n_distinct(login_id)) %>% 
  arrange(desc(count))
login %>% filter(user_id==5158)

login %>%
  group_by(login_id) %>% 
  summarise(count = n_distinct(user_id)) %>% 
  arrange(desc(count))

# 날짜별 총 방문횟수(count login_id), 방문자수(n_distinct user_id) 변수 생성
new_login = login %>% 
  mutate(DateTime = as.Date(c_time)) %>% 
  group_by(DateTime) %>% 
  summarise(총방문횟수 = n(),
            방문자수 = n_distinct(user_id)) %>% 
  ungroup()
new_login

############# 3-2. submission ################
submission %>% head
submission %>% select(cpt_id) %>% n_distinct()
submission %>% is.na %>% sum
submission %>% apply(2, function(x) sum(is.na(x)))
# c_time 결측치는 모두 삭제
submission %<>% filter(!is.na(c_time))
submission %>% apply(2, function(x) sum(is.na(x)))


# 한 팀에 모든 구성원이 제출 가능
submission %>% 
  mutate(c_time = as.Date(c_time)) %>% 
  group_by(c_time,team_id) %>% 
  summarise(n_user = n_distinct(user_id)) %>% 
  arrange(desc(n_user))
  
# 총제출횟수(# of sub), 제출한 팀수(n_distinct team), 제출한 사람수(n_distinct user) 변수 생성
new_submission = submission %>% 
  mutate(DateTime = as.Date(c_time)) %>% 
  group_by(DateTime) %>% 
  summarise(총제출횟수 = n(),
            제출팀수 = n_distinct(team_id),
            제출유저수 = n_distinct(user_id)) %>% 
  ungroup()
  
############# 3-3. competition ################
competition %>% head()
competition %<>% mutate(period_start = as.Date(period_start),
                        period_end = as.Date(period_end),)
# 결측치 제거
competition %>%
  apply(1, function(x) sum(is.na(x)))
competition %>%
  apply(2, function(x) sum(is.na(x)))
competition$sponsor = NULL
competition %<>% slice(1:nrow(competition)-1)
competition %>%
  apply(2, function(x) sum(is.na(x)))

# 일자별 개최대회수(가중없음), 참여자수 가중 개최대회 수 
earliest_date = min(competition[[3]])
latest_date = max(competition[[4]])
new_competition = data.frame(
  DateTime = seq.Date(earliest_date, latest_date, by='day'),
  개최대회수 = 0,
  참여가중개최대회수 = 0)
for(i in 1:nrow(new_competition)){
  for(j in 1:nrow(competition)){
    if(new_competition[[1]][i] %in% seq.Date(competition[[3]][j], competition[[4]][j], by='day')){
      new_competition[[2]][i] = new_competition[[2]][i]+1
      new_competition[[3]][i] = new_competition[[3]][i]+competition[['participants']][j]
    }
  }
}

############# 3-4. user ################
user %>% is.na %>% sum
user %>% apply(1, function(x) all(is.na(x))) %>% sum()
user %>% apply(2, function(x) sum(is.na(x)))

new_user = user %>%
  mutate(DateTime = as.Date(c_time)) %>% 
  group_by(DateTime) %>% 
  summarise(아이디생성개수 = n_distinct(id)) %>% 
  ungroup() %>% 
  na.omit()

############# 3-5. joining ################
train = new_train %>% 
  left_join(new_login, by='DateTime') %>% 
  left_join(new_submission, by='DateTime') %>% 
  left_join(new_competition, by='DateTime') %>% 
  left_join(new_user, by='DateTime')

na_idx = train %>% apply(1, function(x) any(is.na(x)))
train %>% filter(na_idx) %>% View()
