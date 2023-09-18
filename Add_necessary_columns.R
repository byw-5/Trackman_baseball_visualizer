library(tidyverse)
library(randomForest)

combined_df <- read_csv("") #Change path to your raw trackman-baseball data

combined_df <- combined_df %>% 
  mutate(PitchType = case_when(
    TaggedPitchType == "Fastball" ~ "FF",
    TaggedPitchType == "Slider" ~ "SL",
    TaggedPitchType == "Curveball" ~ "CR",
    TaggedPitchType == "ChangeUp" ~ "CH",
    TaggedPitchType == "Splitter" ~ "SP",
    TaggedPitchType == "Cutter" ~ "CT",
    TaggedPitchType == "Sinker" ~ "ST",
    TaggedPitchType == "TwoSeamFastBall"  ~ "ST",
    TaggedPitchType == "OneSeamFastBall"  ~ "ST",
    TaggedPitchType == "Undefined"  ~ "FF",
    TaggedPitchType == "FourSeamFastBall"  ~ "FF",
    TaggedPitchType == "Other"  ~ "FF",
    .default = "FF"
  ))


data3 <- combined_df %>% 
  filter((is.na(ExitSpeed) != T | is.na(ExitSpeed) != T) & PitchCall == "InPlay") %>% 
  mutate(woba = case_when(PlayResult == "Single" ~ 0.9,
                          PlayResult == "Double" ~ 1.25,
                          PlayResult == "Triple" ~ 1.6,
                          PlayResult == "HomeRun" ~ 2,
                          PlayResult != "Single" | PlayResult != "Double" | PlayResult != "Triple" | PlayResult != "HomeRun" ~ 0 
                            )) %>% 
  select(ExitSpeed,Angle)

data4 <- combined_df %>% 
  filter((is.na(ExitSpeed) != T | is.na(ExitSpeed) != T) & PitchCall == "InPlay")

data5 <- combined_df %>% 
  filter((is.na(ExitSpeed) == T | is.na(ExitSpeed) == T) | PitchCall != "InPlay")

load("xwoba.RData")

data3$xwoba <-  predict(rf_classifier, newdata = data3)

data4$xwoba <-  predict(rf_classifier, newdata = data3)

data5 <- data5 %>% 
  mutate(xwoba = 0)

colnames(data5)

combined_df <- rbind(data4,data5)


combined_df <- combined_df %>% 
  mutate(Result = case_when(.default = "Non-hit",
                            PlayResult == "Single"|PlayResult == "Double"|PlayResult == "Triple"|PlayResult == "HomeRun" ~ PlayResult))


combined_df <- combined_df %>% 
  mutate(swing = ifelse(PitchCall == "FoulBall"|PitchCall == "InPlay"|PitchCall == "StrikeSwinging",1,"")) %>% 
  mutate(whiff = ifelse(PitchCall == "StrikeSwinging",1,"")) 


combined_df <- combined_df %>% 
  mutate(zone = case_when(
    PlateLocSide < -0.3157 & PlateLocSide >= -0.947 &  PlateLocHeight < 3.6 & PlateLocHeight >= 2.9  ~ 1,
    PlateLocSide < 0.3157 & PlateLocSide >=  -0.3157 &  PlateLocHeight < 3.6 & PlateLocHeight >= 2.9 ~ 2,
    PlateLocSide < 0.947 & PlateLocSide >=  0.3157 &  PlateLocHeight < 3.6 & PlateLocHeight >= 2.9  ~ 3,
    PlateLocSide < -0.3157 & PlateLocSide >= -0.947 &  PlateLocHeight < 2.9 & PlateLocHeight >= 2.2  ~ 4,
    PlateLocSide < 0.3157 & PlateLocSide >=  -0.3157 &  PlateLocHeight < 2.9 & PlateLocHeight >= 2.2   ~ 5,
    PlateLocSide < 0.947 & PlateLocSide >=  0.3157 &  PlateLocHeight < 2.9 & PlateLocHeight >= 2.2 ~ 6,
    PlateLocSide < -0.3157 & PlateLocSide >= -0.947 &  PlateLocHeight < 2.2 & PlateLocHeight >= 1.5  ~ 7,
    PlateLocSide < 0.3157 & PlateLocSide >=  -0.3157 &  PlateLocHeight < 2.2 & PlateLocHeight >= 1.5 ~ 8,
    PlateLocSide < 0.947 & PlateLocSide >=  0.3157 &  PlateLocHeight < 2.2 & PlateLocHeight >= 1.5   ~ 9,
    PlateLocSide < 0.449 & PlateLocSide >=  -0.449 &  PlateLocHeight < 4 & PlateLocHeight >= 3.6   ~ 12,
    PlateLocSide < -0.947 & PlateLocSide >= -1.347 &  PlateLocHeight < 3.167 & PlateLocHeight >= 2.067  ~ 14,
    PlateLocSide < 1.347 & PlateLocSide >= 0.947 &  PlateLocHeight < 3.167 & PlateLocHeight >= 2.067 ~ 16,
    PlateLocSide < 0.449 & PlateLocSide >=  -0.449 &  PlateLocHeight < 1.5 & PlateLocHeight >= 1.1   ~ 18,
    (PlateLocSide < -0.449 & PlateLocSide >=  -0.947 &  PlateLocHeight < 4 & PlateLocHeight >= 3.6) | (PlateLocSide < -0.947 & PlateLocSide >=  -1.347 &  PlateLocHeight < 4 & PlateLocHeight >= 3.167)   ~ 11,
    (PlateLocSide < 1.347 & PlateLocSide >=  0.449 &  PlateLocHeight < 4 & PlateLocHeight >= 3.6) | (PlateLocSide < 1.347 & PlateLocSide >=  0.947 &  PlateLocHeight < 4 & PlateLocHeight >= 3.167)   ~ 13,
    (PlateLocSide < -0.947 & PlateLocSide >=  -1.347 &  PlateLocHeight < 2.067 & PlateLocHeight >= 1.1) | (PlateLocSide < -0.449 & PlateLocSide >=  -0.947 &  PlateLocHeight < 1.5 & PlateLocHeight >= 1.1)   ~ 17,
    (PlateLocSide < 0.947 & PlateLocSide >= 0.449  &  PlateLocHeight < 1.5 & PlateLocHeight >= 1.1) | (PlateLocSide < 1.347 & PlateLocSide >=  0.947 &  PlateLocHeight < 2.067 & PlateLocHeight >= 1.1)   ~ 19,
    (PlateLocSide < 0 & PlateLocHeight >= 4) | (PlateLocSide < -1.347 & PlateLocHeight >= 2.55) ~ 21,
    (PlateLocSide >= 0 & PlateLocHeight >= 4) | (PlateLocSide >= 1.347 & PlateLocHeight >= 2.55) ~ 23,
    (PlateLocSide < 0 & PlateLocHeight < 1.1) | (PlateLocSide < -1.347 & PlateLocHeight < 2.55) ~ 27,
    (PlateLocSide >= 0 & PlateLocHeight < 1.1) | (PlateLocSide >= 1.347 & PlateLocHeight < 2.55) ~ 29
  ))

combined_df <- combined_df %>% 
  mutate(K = ifelse(KorBB == "Strikeout",1,0)) %>% 
  mutate(BB = ifelse(KorBB == "Walk",1,0)) %>% 
  mutate(HBP = ifelse(PitchCall == "HitByPitch",1,0)) %>%
  mutate(Single = ifelse(PlayResult == "Single",1,0)) %>% 
  mutate(Double = ifelse(PlayResult == "Double",1,0)) %>% 
  mutate(Triple = ifelse(PlayResult == "Triple",1,0)) %>%
  mutate(HomeRun = ifelse(PlayResult == "HomeRun",1,0)) %>%  
  mutate(swing = ifelse(PitchCall == "FoulBall"|PitchCall == "InPlay"|PitchCall == "StrikeSwinging",1,"")) %>% 
  mutate(whiff = ifelse(PitchCall == "StrikeSwinging",1,"")) %>% 
  mutate(hit = ifelse(PlayResult == "Single"|PlayResult == "Double"|PlayResult == "Triple"|PlayResult == "HomeRun",1,0)) %>%
  mutate(BIP = ifelse(PlayResult == "Sacrifice"|PlayResult == "Out"|PlayResult == "FieldersChoice"|PlayResult == "Error"|hit == 1,1,0)) %>% 
  mutate(AB = ifelse(PlayResult == "Out"|hit == 1|K == 1| PlayResult == "FieldersChoice"| PlayResult == "Error",1,0)) %>% 
  mutate(PA = ifelse(AB == 1|PlayResult == "Sacrifice"|PitchCall == "HitByPitch"|BB == 1,1,0)) %>% 
  mutate(woba = case_when(
    PA == 1 & hit != 1 & BB != 1 & HBP != 1 ~ 0,
    BB == 1 | HBP == 1 ~ 0.7,
    Single == 1 ~ 0.9,
    Double == 1 ~ 1.25,
    Triple == 1 ~ 1.6,
    HomeRun == 1 ~ 2,
    .default = 0
  )) %>% 
  mutate(xwoba = ifelse(BB == 1 | HBP == 1, 0.7, xwoba), xwoba = ifelse(xwoba < 0 , 0 , xwoba))

combined_df <- combined_df %>% 
  mutate(landingX = sin((Bearing*pi)/180)*Distance) %>% 
  mutate(landingY = cos((Bearing*pi)/180)*Distance) %>% 
  mutate(landingZone = case_when(
    Bearing < -27 ~ 1,
    Bearing < -9  & Bearing >= -27 ~ 2,
    Bearing <  9  & Bearing >=  -9 ~ 3,
    Bearing <  27 & Bearing >=   9 ~ 4,
    Bearing >= 27 ~ 5,
  )) %>% 
  mutate(Plate = case_when(
    BatterSide == "Left" & PlateLocSide < 0 ~ "Inside",
    BatterSide == "Left" & PlateLocSide >= 0 ~ "Outside",
    BatterSide == "Right" & PlateLocSide < 0 ~ "Outside",
    BatterSide == "Right" & PlateLocSide >= 0 ~ "Inside"
  )) 


write.csv(combined_df,"example_game_data.csv", row.names = F)
