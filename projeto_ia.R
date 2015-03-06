library(ggplot2)
library("gmodels")
library("C50")
library("rpart")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

breast_cancer_wisconsin = read.csv("C:/Users/Leticia/Documents/IA/projeto/breast-cancer-wisconsin_2.csv", head= F)
colnames(breast_cancer_wisconsin) = c ("sample_code","thickness", "uniformity_size",
                                       "uniformity_shape", "marginal_adhesion",
                                       "single_epithelial_size", "bare_nuclei",
                                       "bland_chromatin", "normal_nucleoli", "mitoses",
                                       "class")
breast_cancer_wisconsin$class = factor(breast_cancer_wisconsin$class)

#########################
# Embaralhando os dados #
#########################

set.seed(12345)
breast_cancer_wisconsin_rand = breast_cancer_wisconsin[order(runif(699)),]

##################################
# Criando partições Treino/Teste #
##################################

breast_cancer_wisconsin_train = breast_cancer_wisconsin_rand[1:629,]
breast_cancer_wisconsin_test = breast_cancer_wisconsin_rand[630:699,]
prop.table(table(breast_cancer_wisconsin_train$class))
prop.table(table(breast_cancer_wisconsin_test$class))

######################
# Treinando a árvore #
######################

breast_cancer_wisconsin_model = C5.0(breast_cancer_wisconsin_train[-11], breast_cancer_wisconsin_train$class)
breast_cancer_wisconsin_model
summary(breast_cancer_wisconsin_model)

################################
# Aplicando nos dados de teste #
################################

breast_cancer_wisconsin_pred = predict(breast_cancer_wisconsin_model, breast_cancer_wisconsin_test)
CrossTable(breast_cancer_wisconsin_test$class, breast_cancer_wisconsin_pred, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE, dnn = c('actual class', 'predicted class'))

########################################
# Treinando com floresta de 10 árvores #
########################################

breast_cancer_wisconsin_model_boost10 = C5.0(breast_cancer_wisconsin_train[-11], breast_cancer_wisconsin_train$class,
                                             trials = 10)
breast_cancer_wisconsin_model_boost10
summary(breast_cancer_wisconsin_model_boost10)

breast_cancer_wisconsin_boost10_pred = predict(breast_cancer_wisconsin_model_boost10, breast_cancer_wisconsin_test)
CrossTable(breast_cancer_wisconsin_test$class, breast_cancer_wisconsin_boost10_pred, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c('actual class', 'predicted class'))

########################
# Usando custo de erro #
########################

error_cost = matrix(c(0, 1, 4, 0), nrow = 2)
breast_cancer_wisconsin_cost <- C5.0(breast_cancer_wisconsin_train[-11], breast_cancer_wisconsin_train$class,
                                     costs = error_cost)
breast_cancer_wisconsin_cost_pred <- predict(breast_cancer_wisconsin_cost, breast_cancer_wisconsin_test)
CrossTable(breast_cancer_wisconsin_test$class, breast_cancer_wisconsin_cost_pred, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE, dnn = c('actual class', 'predicted class'))


wdbc = read.csv("C:/Users/Leticia/Documents/IA/projeto/wdbc_2.csv", head= F)
colnames(wdbc) = c("ID","diagnosis", "radious", "texture", "perimeter", "area",
                   "smoothness", "compactness", "concavity", "concave_points", "symmetry",
                   "fractal_dimension","feature1","feature2","feature3","feature4","feature5",
                   "feature6","feature7","feature8","feature9","feature10","feature11",
                   "feature12","feature13","feature14","feature15", "feature16","feature17",
                   "feature18","feature19","feature20")

wpbc = read.csv("C:/Users/Leticia/Documents/IA/projeto/wpbc_2.csv", head= F)
colnames(wpbc) = c("ID","outcome","time", "radious", "texture", "perimeter", "area",
                   "smoothness", "compactness", "concavity", "concave_points", "symmetry",
                   "fractal_dimension","feature1","feature2","feature3","feature4","feature5",
                   "feature6","feature7","feature8","feature9","feature10","feature11",
                   "feature12","feature13","feature14","feature15", "feature16","feature17",
                   "feature18","feature19","feature20","feature21","feature22")

