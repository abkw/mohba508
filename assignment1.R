name<- "Mohammed Bakheet"
liuid<- "mohba508"
library(markmyassignment)
lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)

#The first task (my_num_vector)
my_num_vector<- function(){
return(c(log(11,base=10),cos(pi/5), exp(pi/3), (1173%%7)/19))
}
#---------------------------------------------------------------------------------------------
#The second task (filter_my_vector)
filter_my_vector<-function(x,leq){
    y=x
    z=leq
    for(i in 1:z){
    if (y[i]>=z)
    y[i]=NA
}
return (y)
}
#The third task (dot prod(a, b))
#---------------------------------------------------------------------------------------------

dot_prod <- function(a,b){
    x <- a%*%b
    return(x[1])
}

#The fourth task (approx e(N))
#---------------------------------------------------------------------------------------------
approx_e <- function(N){
    result=0
    for (i in 0:N){
        result = result + (1/factorial(i))
    }
return (result)
}

approx_e_2 <- function(N){
    result=0
    for (i in 0:N){
        result = result + (1/factorial(i))
        a<-strsplit(as.character(result),"\\.")
        a2<-unlist(a)
        a3<-nchar(a2[2])
        if(as.numeric(a3)==14)
            result = 44
            else
            result = 33
    }

return (result)
}
#The fifth task (my magic matrix())
#---------------------------------------------------------------------------------------------
my_magic_matrix <- function(){
    x<- matrix(c(4,3,8,9,5,1,2,7,6),nrow=3,ncol=3)
    return(x)
    }
    #The sixth task (calculate elements(A))
    #---------------------------------------------------------------------------------------------
    calculate_elements <- function(A){
        return (dim(A)[1]*dim(A)[2])
}
#The seventh task (row to zero(A, i))
#---------------------------------------------------------------------------------------------
row_to_zero<-function(A, i){
    A[i,]=0
    return(A)
    }
    #Task number 8 (add elements to matrix(A, x, i, j))
    add_elements_to_matrix<-function(A,x,i,j){
    A[i,j]=A[i,j]+x
    return (A)
}
#Task number 9 (my magic list())
#---------------------------------------------------------------------------------------------
my_magic_list<-function(){
    return(list("info"="my own list",my_num_vector(),my_magic_matrix()))
}
#Task number 10 (change info(x, text))
#---------------------------------------------------------------------------------------------
change_info=function(x, text){
    x$"info"=text
    return(x)
}
#Task number 11 (add note(x, note))
#------------------------------------------------------------------------------------------
add_note<-function(x,note){
    x[["note"]]<-note
    return (x)
}
#Task number 12 (sum numeric parts(x))
#------------------------------------------------------------------------------------------
sum_numeric_parts<-function(x){
    sum=0
    for (i in 1:length(x)){
        if (is.numeric(x[[i]])){
            for (n in 1:length(x[[i]])){
              sum=sum+x[[i]][[n]]
            }  
        }
    }
    return (sum)
}
#Task number 13 (my data.frame())
#------------------------------------------------------------------------------------------
my_data.frame <- function(){
    df <- data.frame(
    id = 1:3,
    name = c("John","Lisa","Azra"),
    income = c(7.30,0.00,15.21),
    rich = c(FALSE,FALSE,TRUE)
)
return(df)
}
#Task number 14(sort head(df, var.name, n))
#------------------------------------------------------------------------------------------
sort_head<-function(df, var.name, n){
   newFrame <- data.frame(Sepal.Length=double(), Sepal.Width=double(), Petal.Length=double(), Petal.Width=double(), Species=character())
    for (j in c(1:n)){
        y <- max(df[var.name], na.rm = TRUE)
        z <- which(df[var.name] == y)
        k <- df[z,]
        newFrame <- rbind(newFrame,k)
        df <- df[-c(z),] 
       
}
    return(newFrame[1:n,])
}
#The task number 15 (add median variable(df, j))
#-------------------------------------------------------------------------------------------
add_median_variable<-function(df, j){
    theMedian<-median(df[,j])
    for (i in 1:length(df[,j])){
        if (df[i,j]>theMedian){    
            df$compared_to_median[i]="Greater"
        }
        else if(df[i,j]< theMedian){
            df$compared_to_median[i]="Smaller"
        }
            else 
            df$compared_to_median[i]="Median"
    }

    return (df)
}
#Task number 16 (analyze columns(df, j))
#-------------------------------------------------------------------------------------------
analyze_columns=function(df, j){
    firstName <- c(mean(df[,j[1]]),median(df[,j[1]]),sd(df[,j[1]]))
    secondName <-c(mean(df[,j[2]]),median(df[,j[2]]),sd(df[,j[2]]))
    correlation_matrix<-cor(df[j])
    names(firstName)<-c("mean","median","sd")
    names(secondName)<-c("mean","median","sd")
    result <- list(firstName,secondName,correlation_matrix)
    names(result)<-c(names(df[j[1]]),c(names(df[j[2]])),"correlation_matrix")
   
    return(result)
}
#The end!

