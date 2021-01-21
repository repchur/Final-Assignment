library(readr)

training <- read.csv("S:/Users/repchur/Machine Learning Course/Week 4/pml-training.csv")

library(caret)
library(randomForest)

training$classe.f <- as.factor(training$classe)

set.seed(1988)

rftrain <- randomForest(classe.f ~ roll_belt+pitch_belt+yaw_belt+total_accel_belt+gyros_belt_x+gyros_belt_z+gyros_belt_y+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm+gyros_forearm_x+gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z, data=training)
rftrain
#OOB error is 0.26% - has predicted very well - now I will attempt to tune the model before I run it on the training data
varImp(rftrain) #lowest is around 55 - going to keep everything for now

ctrl <- trainControl(method="repeatedcv", number=10, repeats=10)
rfgrid <- expand.grid(.mtry=c(3, 7, 9, 11, 14))

set.seed(1988)
m_rf <- train(classe.f ~ roll_belt+pitch_belt+yaw_belt+total_accel_belt+gyros_belt_x+gyros_belt_z+gyros_belt_y+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm+gyros_forearm_x+gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z, data=training, method="rf", metric="Kappa", trControl=ctrl, tuneGrid=rfgrid)

#mtry = 9
set.seed(1988)
rftrainfinal <- randomForest(classe.f ~ roll_belt+pitch_belt+yaw_belt+total_accel_belt+gyros_belt_x+gyros_belt_z+gyros_belt_y+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm+gyros_forearm_x+gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z, data=training, mtry=9)
rftrainfinal

#OOB error=0.27% 

#reading in the test data

testing <- read.csv("S:/Users/repchur/Machine Learning Course/Week 4/pml-testing.csv")

#getting prediction

pred <- predict(rftrainfinal, testing)
pred




