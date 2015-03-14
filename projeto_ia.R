#Legenda dos dados

#1.Sample code number            id number
#2. Clump Thickness               1 - 10
#3. Uniformity of Cell Size       1 - 10
#4. Uniformity of Cell Shape      1 - 10
#5. Marginal Adhesion             1 - 10
#6. Single Epithelial Cell Size   1 - 10
#7. Bare Nuclei                   1 - 10
#8. Bland Chromatin               1 - 10
#9. Normal Nucleoli               1 - 10
#10. Mitoses                      1 - 10
#11. Class:                       2(benígno) ou 4(maligno)

library(ggplot2)
library("gmodels")
library("C50")
library("rpart")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

setwd("C:/Users/Thiago/Documents/UFCG/Projeto - IA")
data <- read.csv("data/breast-cancer-wisconsin_2.csv",sep=",")
data <- na.omit(data)
colnames(data) = c ("sample_code","thickness", "uniformity_size",
                                       "uniformity_shape", "marginal_adhesion",
                                       "single_epithelial_size", "bare_nuclei",
                                       "bland_chromatin", "normal_nucleoli", "mitoses",
                                       "class")

View(data)
plot(data$thickness)
plot(data$uniformity_size)
plot(data$uniformity_shap)
plot(data$marginal_adhesion)
plot(data$single_epithelial_size)
plot(data$bare_nuclei)
plot(data$bland_chromatin)
plot(data$normal_nucleoli)
plot(data$mitoses)

plot(sort(data$thickness))
plot(sort(data$uniformity_size))
plot(sort(data$uniformity_shape))
plot(sort(data$marginal_adhesion))
plot(sort(data$single_epithelial_size))
plot(data$bare_nuclei)
plot(sort(data$bland_chromatin))
plot(sort(data$normal_nucleoli))
plot(sort(data$mitoses))

cor(data$class,data$uniformity_size)
cor(data$class,data$uniformity_shape)
cor(data$class,data$marginal_adhesion)
cor(data$class,data$single_epithelial_size)
cor(data$class,data$bland_chromatin)
cor(data$class,data$normal_nucleoli)
cor(data$class,data$mitoses)
cor(data$class,data$thickness)

fit <- rpart(class ~ bland_chromatin + uniformity_shape + uniformity_size, data=data, method="class")
fancyRpartPlot(fit)
prediction <- predict(fit, data, type = "class")

CrossTable(data$class, prediction, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE, dnn = c('actual class', 'predicted class'))


fit2 <- rpart(class ~ marginal_adhesion +
               bland_chromatin + 
               uniformity_shape + 
               uniformity_size +
               single_epithelial_size+
               bland_chromatin +
               normal_nucleoli +
               mitoses +
               thickness
             , data=data, method="class")

fancyRpartPlot(fit2)

prediction2 <- predict(fit2, data, type = "class")

CrossTable(data$class, prediction2, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE, dnn = c('actual class', 'predicted class'))

fit3 = randomForest(factor(class) ~ marginal_adhesion +
          bland_chromatin + 
          uniformity_shape + 
          uniformity_size +
          single_epithelial_size+
          bland_chromatin +
          normal_nucleoli +
          mitoses +
          thickness
        , data=data, method="class", ntree = 10)

prediction3 <- predict(fit3, data, type = "class")

CrossTable(data$class, prediction3, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE, dnn = c('actual class', 'predicted class'))
