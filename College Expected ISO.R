library(caret)
library(scales)
library(tidyverse)
library(dplyr)
library(gam)

cbb_complete <- read.csv("CBB Complete.csv")

cbb_contact <- subset(cbb_complete, !is.na(cbb_complete$ExitSpeed))
cbb_inplay <- subset(cbb_contact, cbb_contact$PitchCall == 'InPlay')
cbb_inplay$EventID <- c(1:nrow(cbb_inplay)) 

cbb_inplay$ba <- 0
cbb_inplay$ba[cbb_inplay$PlayResult == 'Single'] <- 1
cbb_inplay$ba[cbb_inplay$PlayResult == 'Double'] <- 1
cbb_inplay$ba[cbb_inplay$PlayResult == 'Triple'] <- 1
cbb_inplay$ba[cbb_inplay$PlayResult == 'HomeRun'] <- 1

cbb_inplay$slg <- 0
cbb_inplay$slg[cbb_inplay$PlayResult == 'Single'] <- 1
cbb_inplay$slg[cbb_inplay$PlayResult == 'Double'] <- 2
cbb_inplay$slg[cbb_inplay$PlayResult == 'Triple'] <- 3
cbb_inplay$slg[cbb_inplay$PlayResult == 'HomeRun'] <- 4

test_size <- round(nrow(cbb_inplay) * 0.1 ,0)

for(i in 1:10){
  sample_vec <- sample(1:nrow(cbb_inplay), test_size, replace = FALSE)
  sample_frame <- cbb_inplay[sample_vec,]
  cbb_inplay <- cbb_inplay[-sample_vec,]
  assign(paste("test_fold_",i,sep = ''),sample_frame)
} 

cbb_inplay <- rbind(test_fold_1, test_fold_2, test_fold_3, test_fold_4, test_fold_5,
                    test_fold_6, test_fold_7, test_fold_8, test_fold_9, test_fold_10)

cbb_inplay <- subset(cbb_contact, cbb_contact$PitchCall == 'InPlay')
cbb_inplay$EventID <- c(1:nrow(cbb_inplay)) 

cbb_inplay$ba <- 0
cbb_inplay$ba[cbb_inplay$PlayResult == 'Single'] <- 1
cbb_inplay$ba[cbb_inplay$PlayResult == 'Double'] <- 1
cbb_inplay$ba[cbb_inplay$PlayResult == 'Triple'] <- 1
cbb_inplay$ba[cbb_inplay$PlayResult == 'HomeRun'] <- 1

cbb_inplay$slg <- 0
cbb_inplay$slg[cbb_inplay$PlayResult == 'Single'] <- 1
cbb_inplay$slg[cbb_inplay$PlayResult == 'Double'] <- 2
cbb_inplay$slg[cbb_inplay$PlayResult == 'Triple'] <- 3
cbb_inplay$slg[cbb_inplay$PlayResult == 'HomeRun'] <- 4

xBA_error <- c()
xSLG_error <- c()
k_count <- c()

for(k in seq(10,100,5)){
  print(k)
  xba_k <- data.frame()
  xslg_k <- data.frame()
  
  for(x in 1:10){
    inplay_test <- get(paste('test_fold_',x,sep=''))
    inplay_train <- anti_join(cbb_inplay, inplay_test)
    
    xba_frame <- data.frame("ExitVelocity" = as.numeric(inplay_train$ExitSpeed),
                            'LaunchAngle' = as.numeric(inplay_train$Angle))
    xba_test <- data.frame("ExitVelocity" = as.numeric(inplay_test$ExitSpeed),
                           'LaunchAngle' = as.numeric(inplay_test$Angle))
    
    xba_knn <- knnreg(xba_frame, inplay_train$ba, k)
    
    xba_test$xBA <- predict(xba_knn, xba_test)
    
    xslg_frame <- data.frame("ExitVelocity" = as.numeric(inplay_train$ExitSpeed),
                             'LaunchAngle' = as.numeric(inplay_train$Angle))
    xslg_test <- data.frame("ExitVelocity" = as.numeric(inplay_test$ExitSpeed),
                            'LaunchAngle' = as.numeric(inplay_test$Angle))
    
    xslg_knn <- knnreg(xslg_frame, inplay_train$slg, k)
    
    xslg_test$xSLG <- predict(xslg_knn, xslg_test)
    
    xba_test$BA <- inplay_test$ba
    xslg_test$SLG <- inplay_test$slg
    
    xba_k <- rbind(xba_k, xba_test)
    xslg_k <- rbind(xslg_k, xslg_test)
  }
  
  error1 <- sqrt((sum(xba_k$xBA - xba_k$BA))^2)
  error2 <- sqrt((sum(xslg_k$xSLG - xslg_k$SLG))^2)
  
  xBA_error <- c(xBA_error, error1)
  xSLG_error <- c(xSLG_error, error2)
  k_count <- c(k_count, k)
}

error_frame <- data.frame("K" = seq(10,100,5),
                          "xBA" = xBA_error,
                          "xSLG" = xSLG_error)

testerror_plot <- ggplot() + 
  geom_line(data = error_frame, aes(x = K, y = xBA), color = "blue") +
  geom_line(data = error_frame, aes(x = K, y = xSLG), color = "red") +
  labs(x = 'K', y = 'Test Error', title = 'Test Error by K Size')
testerror_plot

error_frame <- error_frame[order(error_frame$xBA),]
error_frame$ba_rank <- c(1:19)
error_frame <- error_frame[order(error_frame$xSLG),]
error_frame$slg_rank <- c(1:19)
error_frame$rank_avg <- (error_frame$ba_rank + error_frame$slg_rank) / 2
error_frame <- error_frame[order(error_frame$rank_avg, error_frame$ba_rank),]
best_k <- error_frame$K[1]

xba_knn <- knnreg(xba_frame, inplay_train$ba, best_k)
xslg_knn <- knnreg(xslg_frame, inplay_train$slg, best_k)

ev_vec <- c(round(cbb_inplay$ExitSpeed,0))
la_vec <- c(round(cbb_inplay$Angle,0))

expected_display <- data.frame('ExitVelocity' = ev_vec,
                               'LaunchAngle' = la_vec)
expected_display <- unique(expected_display)
display_xba <- predict(xba_knn, expected_display)
display_xslg <- predict(xslg_knn, expected_display)
expected_display$xba <- c(display_xba)
expected_display$xslg <- c(display_xslg)

xba_map <- ggplot(data = expected_display, aes(x = ExitVelocity, y = LaunchAngle)) +
  geom_tile(aes(fill = xba)) + 
  scale_fill_gradient2(low = "Blue", mid = "White", high = "Red", midpoint = 0.5, limits = c(0, 1), oob = squish, na.value = "grey50") +
  ggtitle('College xBA')
print(xba_map)

xslg_map <- ggplot(data = expected_display, aes(x = ExitVelocity, y = LaunchAngle)) +
  geom_tile(aes(fill = xslg)) + 
  scale_fill_gradient2(low = "Blue", mid = "White", high = "Red", midpoint = 1, limits = c(0, 2), oob = squish, na.value = "grey50") +
  ggtitle('College xSLG')
print(xslg_map)

player_frame <- data.frame("ExitSpeed" = cbb_inplay$ExitSpeed,
                           "Angle" = cbb_inplay$Angle)

player_xBA <- predict(xba_knn, player_frame)
player_xSLG <- predict(xslg_knn, player_frame)
player_frame$xBA <- player_xBA
player_frame$xSLG <- player_xSLG
player_frame$BA <- cbb_inplay$ba
player_frame$SLG <- cbb_inplay$slg
player_frame$xISO <- player_frame$xSLG - player_frame$xBA
player_frame$PA <- 1
player_frame$Player <- cbb_inplay$Batter
cbb_inplay$Bearing <- as.character(cbb_inplay$Bearing)
player_frame$Direction <- round(as.numeric(cbb_inplay$Bearing),0)

player_averages <- aggregate(cbind(xBA,xSLG,PA)~Player, player_frame, sum)
player_averages$xBA <- player_averages$xBA / player_averages$PA
player_averages$xSLG <- player_averages$xSLG / player_averages$PA
player_averages$xISO <- player_averages$xSLG - player_averages$xBA

player_averages <- subset(player_averages, player_averages$PA >= 30)
player_averages <- player_averages[order(-player_averages$xISO),]

print(head(player_averages, 20))

direction_averages <- aggregate(cbind(xBA,xSLG,PA,BA,SLG)~Direction, player_frame, sum)
direction_averages$xBA <- direction_averages$xBA / direction_averages$PA
direction_averages$xSLG <- direction_averages$xSLG / direction_averages$PA
direction_averages$xISO <- direction_averages$xSLG - direction_averages$xBA
direction_averages$BA <- direction_averages$BA / direction_averages$PA
direction_averages$SLG <- direction_averages$SLG / direction_averages$PA
direction_averages$ba_dif <- direction_averages$xBA - direction_averages$BA
direction_averages$slg_dif <- direction_averages$xSLG - direction_averages$SLG

direction_averages <- subset(direction_averages, direction_averages$Direction >= -45 &
                               direction_averages$Direction <= 45)

direction_line <- ggplot(data = direction_averages, aes(x = Direction, y = ba_dif)) +
  geom_line()
direction_line

direction_line2 <- ggplot(data = direction_averages, aes(x = Direction, y = slg_dif)) +
  geom_line()
direction_line2

ba_adj <- gam(ba_dif~ns(Direction,7)+ ns(Direction,9), data = direction_averages)
summary(ba_adj)

slg_adj <- gam(slg_dif~ns(Direction,6) + ns(Direction,9), data = direction_averages)
summary(slg_adj)

predict_frame <- data.frame("Direction" = as.numeric(as.character(cbb_inplay$Bearing)))
player_frame$ba2 <- predict(ba_adj, predict_frame)
player_frame$ba2 <- ifelse(player_frame$Direction >= -45 & player_frame$Direction <= 45,
                           player_frame$ba2, 0)

predict_frame <- data.frame("Direction" = as.numeric(as.character(cbb_inplay$Bearing)))
player_frame$slg2 <- predict(slg_adj, predict_frame)
player_frame$slg2 <- ifelse(player_frame$Direction >= -45 & player_frame$Direction <= 45,
                           player_frame$slg2, 0)

player_frame$xBA <- player_frame$xBA - player_frame$ba2
player_frame$xSLG <- player_frame$xSLG - player_frame$slg2

direction_averages <- aggregate(cbind(xBA,xSLG,PA,BA,SLG)~Direction, player_frame, sum)
direction_averages$xBA <- direction_averages$xBA / direction_averages$PA
direction_averages$xSLG <- direction_averages$xSLG / direction_averages$PA
direction_averages$xISO <- direction_averages$xSLG - direction_averages$xBA
direction_averages$BA <- direction_averages$BA / direction_averages$PA
direction_averages$SLG <- direction_averages$SLG / direction_averages$PA
direction_averages$ba_dif <- direction_averages$xBA - direction_averages$BA
direction_averages$slg_dif <- direction_averages$xSLG - direction_averages$SLG

direction_averages <- subset(direction_averages, direction_averages$Direction >= -45 &
                               direction_averages$Direction <= 45)

direction_line <- ggplot(data = direction_averages, aes(x = Direction, y = ba_dif)) +
  geom_line()
direction_line

direction_line2 <- ggplot(data = direction_averages, aes(x = Direction, y = slg_dif)) +
  geom_line()
direction_line2

player_averages <- aggregate(cbind(xBA,xSLG,PA)~Player, player_frame, sum)
player_averages$xBA <- player_averages$xBA / player_averages$PA
player_averages$xSLG <- player_averages$xSLG / player_averages$PA
player_averages$xISO <- player_averages$xSLG - player_averages$xBA

player_averages <- subset(player_averages, player_averages$PA >= 30)
player_averages <- player_averages[order(-player_averages$xISO),]

print(head(player_averages, 20))

