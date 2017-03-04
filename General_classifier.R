

setwd("/home/hilabs/auto_detect_column/")

source("util/isCategoricalColumn.R")
source("util/isIDColumn.R")
source("util/isDateColumn.R")
source("util/isSequenceColumn.R")
source("util/isNumericColumn.R")
source("level_classifier/level2_classifier.R")
source("util/isQuantityColumn.R")
source("data_generation/numeric_data_generation.R")
source("data_generation/categorical_lookup_table.R")


#Input arguments from config file
config_args <- read.csv("config/config.csv", stringsAsFactors = FALSE)

size_of_samples <- as.numeric(config_args[1,2])
no_of_samples <- as.numeric(config_args[2,2])
match_ptg <- as.numeric(config_args[3,2])
tr_te <- as.character(config_args[4,2])


library(RMySQL)
connection <- dbConnect(MySQL(), user="root", password="Aanya@12345",dbname="syndata2008_2010", host="166.62.80.136")
level.0.classification <- dbGetQuery(connection, "select * from level_0_classification order by priority_order")


if(tr_te == "tr"){
  load("data_train.RData")
}else{
  load("data_test.RData")
}

col_names<-colnames(data)
original_colnames<-colnames(data)
no_of_samples<-10
size_of_samples<-0.01
df<-data.frame(matrix(0,nrow=no_of_samples*size_of_samples*(dim(data))[1]))
samp_col<-c()


for(i in 1:ncol(data)){
  
  data[,i] <- gsub(" ","",data[,i])
  data[,i] <- gsub("'","",data[,i])
  
}

data1<-data
data1<-apply(data1, 2, function(x) gsub("^$|^ $", NA, x))
data1<- as.data.frame(data1)


size = floor(size_of_samples*length(data1[,1]))
if(no_of_samples*size!=dim(df)[1]){
  diff=abs((no_of_samples*size)-dim(df)[1])
  df<-data.frame(df[1:(dim(df)[1]-diff),])
}

resample<-function(i){
  #  data1[,i]<-na.omit(data1[,i])
  samp_col<-append(samp_col,replicate(no_of_samples,sample(data1[,i], size = floor(size_of_samples*length(data1[,i])), replace = T)))
  # df<-cbind(df,samp_col)
  #print(i)
  # print(dim(df))
  return(samp_col)
}
for(col in 1:ncol(data1)){
  
  df<-cbind(df,resample(col))
  
}

df<-df[,-1]
colnames(df)<-colnames(data)
data<-df
rm(df)


call_by_order<-function(col_name){
  for(x in 1:dim(level.0.classification)[1]){
    
    threshold<-level.0.classification[which((level.0.classification)[2]=='Categorical'),6]
    threshold_for_date<-level.0.classification[which((level.0.classification)[2]=='Date'),6]
    threshold_for_id<-level.0.classification[which((level.0.classification)[2]=='ID'),6]
    threshold_for_seq<-level.0.classification[which((level.0.classification)[2]=='Sequence'),6]
    
    
    if(eval(parse(text=level.0.classification[x,'function_call']))==TRUE){
      colnames(data)[which(colnames(data)==colnames(data)[col_name])]<<-paste(colnames(data)[col_name],level.0.classification[x,2],sep='.')
      break}
  }
}

sapply(1:length(col_names) , call_by_order)

colnames(data1)<-colnames(data)
#colnames(data1)<-original_colnames

#level1_types<-c('Numeric','Categorical','ID','Sequence','Date','Quantity')
#for(col in 1:length(col_names)){
# for(j in 1:length(level1_types)){
#  if((grepl(level1_types[j],colnames(data)[col]))==TRUE){
#col_type<-scan(text=   # use scan to separate after insertion of commas
#      gsub("\\]", "],",   # put commas in after "]"'s
#          gsub(".\\[", ",[",  colnames(data)[col])) ,  # add commas before "[" unless at first position
#  what="", sep=".")
#}
# }
# column_type<-col_type[2]
#colnames(data1)[col]<-paste(colnames(data1)[col],column_type,sep='.')
#}

#data1<-apply(data1, 2, function(x) gsub("^$|^ $", NA, x))
#data1<- as.data.frame(data1)
for(i in 1:ncol(data1)){
  data1[,i]<- as.character(data1[,i])
}

for(i in 1:ncol(data1)){
  coltype<-scan(text=   # use scan to separate after insertion of commas
                  gsub("\\]", "],",   # put commas in after "]"'s
                       gsub(".\\[", ",[",  colnames(data1)[i])) ,  # add commas before "[" unless at first position
                what="", sep=".")
  
  col_type<-coltype[2]
  threshold<-0.7
  eval(boolean_isFrequent_CountColumn(i, col_type,threshold))
  #print(col_type)
}
if(tr_te=="tr")
  write.csv(data1,"temporary/train_data_identified.csv")
else write.csv(data1,"temporary/test_data_identified.csv")


source("data_generation/generic_data_generation.R")
