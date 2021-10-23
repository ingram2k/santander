

raw_data_freq<-raw_data

variable<-names(raw_data)

variable<-variable[3:202]

for(i in 1:length(variable)) {

udata<-raw_data %>% select (var=variable[i]) %>% group_by(var) %>% summarise(count=n())

colnames(udata)

colnames(udata)[1]<-paste0(variable[i])

colnames(udata)[2]<-paste0(variable[i],"_freq")

raw_data_freq<-merge(raw_data_freq,udata,by = paste0(variable[i]) )


}

test_data<-fread("C:/Users/phan3/OneDrive/Desktop/data/santander/test.csv")



test_data_freq<-test_data

variable<-names(test_data)

variable<-variable[2:201]

for(i in 1:length(variable)) {
  
  udata<-test_data %>% select (var=variable[i]) %>% group_by(var) %>% summarise(count=n())
  
  colnames(udata)
  
  colnames(udata)[1]<-paste0(variable[i])
  
  colnames(udata)[2]<-paste0(variable[i],"_freq")
  
  test_data_freq<-merge(test_data_freq,udata,by = paste0(variable[i]) )
  
  
}
