#install.packages("neuralnet")
lybrary(neuralnet)

path <- choose.files()
dataset <- read.csv(path, as.is = TRUE)

dataset[,1] <- as.numeric(dataset[,1])
dataset[,2] <- as.numeric(dataset[,2])
dataset[,3] <- as.numeric(dataset[,3])
dataset[,4] <- as.numeric(dataset[,4])
dataset[,5] <- as.numeric(dataset[,5])
dataset[,10] <- as.numeric(dataset[,10])
dataset[,11] <- as.numeric(dataset[,11])
dataset[,12] <- as.numeric(dataset[,12])
dataset[,13] <- as.numeric(dataset[,13])
dataset[,14] <- as.numeric(dataset[,14])
dataset[,15] <- as.numeric(dataset[,15])
dataset[,16] <- as.numeric(dataset[,16])
dataset[,17] <- as.numeric(dataset[,17])
dataset[,18] <- as.numeric(dataset[,18])

tmp <- dataset[dataset[,'class']== "notckd",]

tmp[,4] <-tmp[1,4]
tmp[,5] <-tmp[1,5]
tmp[,6] <-tmp[1,6]
tmp[,7] <-tmp[1,7]
tmp[,8] <-tmp[1,8]
tmp[,9] <-tmp[1,9]
tmp[,19] <-tmp[1,19]
tmp[,20] <-tmp[1,20]
tmp[,21] <-tmp[1,21]
tmp[,22] <-tmp[1,22]
tmp[,23] <-tmp[1,23]
tmp[,24] <-tmp[1,24]
dataset[dataset[,'class']== "notckd",] <- tmp 


coeficiente_perceptrivo <-function(data, col){
    for(i in 1:dim(data)[1]){
        if(data[i,col] == "no" || data[i,col] == "good" || data[i,col] == "notpresent"|| data[i,col] == "normal"){
            data[i,col] <- "0"
        }
        if(data[i,col] == "yes" || data[i,col] == "poor" || data[i,col] == "present"|| data[i,col] == "abnormal"){
                data[i,col] <- "1"
        }
    }
    data[,col] <- as.numeric(data[,col])
    return (data)
}

dataset <- coeficiente_perceptrivo(dataset,6)
dataset <- coeficiente_perceptrivo(dataset,7)
dataset <- coeficiente_perceptrivo(dataset,8)
dataset <- coeficiente_perceptrivo(dataset,9)
dataset <- coeficiente_perceptrivo(dataset,19)
dataset <- coeficiente_perceptrivo(dataset,20)
dataset <- coeficiente_perceptrivo(dataset,21)
dataset <- coeficiente_perceptrivo(dataset,22)
dataset <- coeficiente_perceptrivo(dataset,23)
dataset <- coeficiente_perceptrivo(dataset,24)

range <- sample(1:dim(dataset)[1],250)
train <- dataset[range,]
test <e- dataset[-range,]

nn <- neuralnet(class~names(dataset)[names(dataset)!="class"], data = dataset[,], hidden=3, act.fct= "logistic", linear.output=FALSE)
nn <- neuralnet(class~age+bp+sg+al+su+rbc+pc+pcc+ba+bgr+bu+sc+sop+pot+hemo+pcv+wc+rc+htn+dm+cat+appet+pe+ane, data = dataset[,], hidden=3, act.fct= "logistic", linear.output=FALSE)
