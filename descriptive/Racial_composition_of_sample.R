#=========================================================================#
#
#                     Racial Composition for Robert Wood Johnson Foundation
#
#       by JP Dube, 9-27-2023
#
#=========================================================================#


#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+

if (Sys.info()[4]=="JDUBE-LT"){
  wd = "C:/Users/jdube/Dropbox/Servus/Portland"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")


#---------+---------+---------+---------+---------+---------+
# Load Census Tract Summaries
#---------+---------+---------+---------+---------+---------+
df=read.csv("data/forTransUnion/summary_stats.csv")

total = df$Single_Family+df$Multi_Family
Black = total*df$Black/100
Hispanic = total*df$Hispanic/100

Black_single = df$Single_Family*df$Black/100
Hispanic_single = df$Single_Family*df$Hispanic/100

populationlist = populationlist1 = NULL
tractlist = sort(unique(df$Tract))
for(ii in tractlist){
  index = df$Tract==ii
  if( df$Single_Family[index]>0 & is.na(df$Black[index])!=1){
    poptemp = matrix(4,df$Single_Family[index],2)
    poptemp[,1] = ii
    poptemp[1:round(df$Single_Family*df$Black/100)[index],2] = 1
    poptemp[(round(df$Single_Family*df$Black/100)+1)[index]:(round(df$Single_Family*df$Black/100)[index]+round(df$Single_Family*df$Hispanic/100)[index]),2] = 2
    poptemp[(round(df$Single_Family*df$Black/100)[index]+round(df$Single_Family*df$Hispanic/100)[index]+1):(round(df$Single_Family*df$Black/100)[index]+round(df$Single_Family*df$Hispanic/100)[index]+round(df$Single_Family*df$White/100)[index]),2] = 3
    populationlist = rbind(populationlist,poptemp)
    
    poptemp1 = kronecker( matrix(c(df$Tract[index],df$Income[index]),nrow=1),matrix(1,df$Single_Family[index],1))
    populationlist1 = rbind(populationlist1,poptemp1)
  }
}


#---------+---------+---------+---------+---------+---------+
# Estimate Racial Composition of 20,000 random sample
#---------+---------+---------+---------+---------+---------+

# Bootstrap draws from the Census Tracts
composition = matrix(0,100,4)
for(rr in 1:100){
  composition[rr,] = table(populationlist[sample(1:nrow(populationlist),20000,replace=FALSE),2])
}

#summarize Racial composition of a 20K random sample
apply(composition,2,mean)

#Income distribution (using average Census Tract means)
quantile(populationlist1[sample(1:nrow(populationlist1),20000,replace=FALSE),2],prob = c(.25,.5,.75),na.rm=T)
