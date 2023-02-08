################################################################################
library(tidyverse)
library(magrittr)
library(readxl)
library(data.table)
library(fasttime)
library(lubridate)
library(hms)
library(changepoint)
library(showtext)
library(pacman)
library(caret)
pacman::p_load(progress)
pb <- progress_bar$new(total=100, width=60, clear=F,
                       format = "(:spin) [:bar] :percent IN :elapsed")
################################################################################
setwd('C:/proj/masta-ev/abs_test')
files <- list.files('./')
len <- files[str_detect(files, '.csv')] %>% length()

data <- list()

for (i in 1:len) {
  file <- files[stringr::str_detect(files, '.csv')][i]
  data[[i]] <- fread(paste0('./', file), encoding='UTF-8', skip=4)
}

abs <- rbindlist(data)

names(abs) <- c("TestNo", "검사일", "시작시간", "종료시간", "차종", "종합",
                "전륜1차", "전륜2차", "후륜1차", "후륜2차", "사이드1차", "사이드2차",
                "기타")
################################################################################

abs <- abs[-which(is.na(abs$TestNo)),] # 결측치 처리 

abs$종합 <- ifelse(abs$종합=="OK", "합격", "불합격")
mut <- abs %>%
  select(TestNo, 전륜1차, 전륜2차, 후륜1차, 후륜2차, 사이드1차, 사이드2차, 종합)

mut %<>% mutate_all(funs(str_replace(., "X", "불합격")))
mut %<>% mutate_all(funs(str_replace(., "O", "합격")))
mut %<>% mutate_all(funs(str_replace(., " ", "")))
mut %<>% mutate_all(funs(str_replace(., "`", "합격")))
mut <- replace(mut, mut=='', '합격')
mut[is.na(mut)] <- "합격"

mut$측정횟수 <- 3
len <- nrow(abs)
for (i in 1:len) {
  if(mut$전륜1차[i] != '합격') {
    mut$측정횟수[i] <- mut$측정횟수[i]+1
  }
  else if(mut$후륜1차[i] != '합격') {
    mut$측정횟수[i] <- mut$측정횟수[i]+1
  }
  else if(mut$사이드1차[i] != '합격'){
    mut$측정횟수[i] <- mut$측정횟수[i]+1
  }
}

mut %<>%
  mutate(
    
    재측정부분 = case_when(
      전륜1차 !='합격' ~ '전륜',
      후륜1차 !='합격' ~ '후륜',
      사이드1차 !='합격' ~ '사이드',
      전륜1차 !='합격' & 후륜1차 !='합격' ~ '전륜,후륜',
      전륜1차 !='합격' & 사이드1차 !='합격' ~ '전륜,사이드',
      후륜1차 !='합격' & 사이드1차 !='합격' ~ '후륜,사이드',
      전륜1차 !='합격' & 후륜1차 !='합격'  & 사이드1차 !='합격' ~ '전륜,후륜,사이드',
      !(전륜1차 !='합격' & 후륜1차 !='합격' & 사이드1차 !='합격') ~ ' '),
    
    전륜합격여부 = case_when(
      전륜1차 =='합격' | 전륜2차=='합격' ~ '합격',
      전륜1차 !='합격' & 전륜2차=='합격' ~ '합격',
      전륜1차 !='합격' & 전륜2차!='합격' ~ '불합격'),
    
    후륜합격여부 = case_when(
      후륜1차 =='합격' | 후륜2차=='합격' ~ '합격',
      후륜1차 !='합격' & 후륜2차=='합격' ~ '합격',
      후륜1차 !='합격' & 후륜2차!='합격' ~ '불합격'),
    
    사이드합격여부 = case_when(
      사이드1차 =='합격' | 사이드2차=='합격' ~ '합격',
      사이드1차 !='합격' & 사이드2차=='합격' ~ '합격',
      사이드1차 !='합격' & 사이드2차!='합격' ~ '불합격'),
    
    합격여부 = case_when(
      전륜합격여부=='합격' & 후륜합격여부=='합격' & 사이드합격여부=='합격' & 종합=='합격' ~ '합격',
      !(전륜합격여부=='합격' & 후륜합격여부=='합격' & 사이드합격여부=='합격' & 종합=='합격') ~ '불합격')
  )


측정횟수 <- mut %>% select(측정횟수) %>% pull()
재측정부분 <- mut %>% select(재측정부분) %>% pull()
전륜합격여부 <- mut %>% select(전륜합격여부) %>% pull()
후륜합격여부 <- mut %>% select(후륜합격여부) %>% pull()
사이드합격여부 <- mut %>% select(사이드합격여부) %>% pull()
합격여부 <- mut %>% select(합격여부) %>% pull()

abs %>%
  mutate(측정횟수, 재측정부분,
         전륜합격여부, 후륜합격여부, 사이드합격여부, 합격여부) -> abs_rst

abs_rst$재측정여부 <- ifelse(mut$측정횟수 >= 4, '재측정', '')
abs_rst$측정횟수 <- as.numeric(abs_rst$측정횟수)
abs_rst$TestNo <- as.numeric(abs_rst$TestNo)
abs_rst$starttime <- fastPOSIXct(paste0(abs_rst$검사일, ' ', abs_rst$시작시간))
abs_rst$starttime <- abs_rst$starttime - 9*60*60
abs_rst$endtime <- fastPOSIXct(paste0(abs_rst$검사일, ' ', abs_rst$종료시간))
abs_rst$endtime <- abs_rst$endtime - 9*60*60

abs_rst %>%
  select(TestNo, 차종, 검사일, 시작시간, 종료시간,
         측정횟수, 재측정여부, 재측정부분,
         전륜합격여부, 후륜합격여부, 사이드합격여부, 종합, 합격여부)

################################################################################
setwd('C:/proj/masta-ev/sensor')

files <- list.files('./')
len <- files[str_detect(files, '.csv')] %>% length()
data <- list()

for (i in 1:len) {
  file <- files[stringr::str_detect(files, '.csv')][i]
  data[[i]] <- fread(paste0('./', file), header=FALSE, skip=4, tz='')
}

data <- rbindlist(data)

names(data) <- c('timestamp', 'X', 'Y', 'Z', 'intime', 'isrun')
data$intime <- fastPOSIXct(data$intime)
data$intime <- data$intime - 9*60*60
data %<>% select(-c(timestamp, isrun))
data %<>% select(intime, everything())

################################################################################

iters <- nrow(abs_rst)
abnm_rst <- NULL

for (i in 1:iters) {
  time.interval <- abs_rst$starttime[i] %--% (abs_rst$endtime[i]+60)
  time.idx <- data$intime %within% time.interval
  data_sel <- data[time.idx, ]
  
  X_init <- min(data_sel$X)
  Y_init <- min(data_sel$Y)
  Z_init <- min(data_sel$Z)
  
  data_sel %<>% mutate(X=X-X_init, Y=Y-Y_init, Z=Z-Z_init)
  cols <- data_sel %>% select(-intime)
  col_sel <- data_sel$Z
  
  q_sel <- ( as.numeric(abs_rst$측정횟수[i]) * 2) + 1
  
  v_segneigh <- cpt.var(col_sel, penalty='BIC', method = "SegNeigh", Q = q_sel)
  v_time <- data_sel$intime[cpts(v_segneigh)] 
  interval_len <- length(v_time)/2
  breaking.idx <- NULL 
  cpt.thres <- NULL 
  
  
  for (k in 1:interval_len) {
    assign(paste0('cpt.i_', k),
           v_time[(2*k)-1] %--% v_time[(2*k)]
    )
    assign(paste0('ti_', k, '.idx'),
           which(data_sel$intime %within% get(paste0('cpt.i_',k)))
    )
    breaking.idx[[k]] <- get(paste0('ti_', k, '.idx')) 
    cpt.thres[k] <- max(col_sel[get(paste0('ti_', k, '.idx'))]) 
    
  }
  
  breaking.idx <- unlist(breaking.idx)
  unbreaking.idx <- setdiff(1:length(col_sel), breaking.idx)
  anomaly.idx <- which(col_sel[unbreaking.idx] > min(cpt.thres))
  
  # 결과 저장  
  abnm_rst[i] <- ifelse(length(anomaly.idx) != 0, "이상 없음", "이상 발견")
  
  pb$tick()
  Sys.sleep(0.1)
}
################################################################################

cat(paste0('총 ', length(abnm_rst), '개의 차량 중 ',
           sum(abnm_rst=='이상 발견'), '개 차량이 이상(anomaly)으로 탐지되었습니다.'), '\n',
    '(* 이상탐지비율 =', (table(abnm_rst)['이상 발견'] / sum(table(abnm_rst)) * 100), '%)')

abs_rst %>% select(TestNo, 합격여부) -> rst_match
rst_match$이상탐지결과 <- abnm_rst

# Matching (Abs Test ~ Anomaly Result)
rst_match %<>%
  mutate(매칭결과=case_when(
    (합격여부=='합격' & 이상탐지결과=='이상 없음')~'일치',
    (합격여부=='불합격' & 이상탐지결과=='이상 발견')~'일치',
    (합격여부=='합격' & 이상탐지결과=='이상 발견')~'불일치',
    (합격여부=='불합격' & 이상탐지결과=='이상 없음')~'불일치'
  )
  )

# Matching Accuracy
rst_match
nms <- c('불일치율', '일치율')
vls <- c(prop.table(table(rst_match$매칭결과))['불일치'],
         prop.table(table(rst_match$매칭결과))['일치'])
prp <- paste0(vls*100, '%')
names(prp) <- nms
prp

# Confusion Matrix
rst_match$이상여부 <- ifelse(rst_match$이상탐지결과=="이상 없음", "합격", "불합격")
rst_match$이상여부 <- as.factor(rst_match$이상여부)
rst_match$합격여부 <- as.factor(rst_match$합격여부)
confusionMatrix(rst_match$합격여부, rst_match$이상여부, positive='합격')
################################################################################