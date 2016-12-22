income=read.csv("C:/Users/arjhu_000/Desktop/Assignment 1/income_tr.csv")
setwd("C:/Users/arjhu_000/Desktop/Assignment 1")
k=5
#income=income[1:100,]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

final=data.frame()

# imputing the mean for the attributes 'age' and 'hours_per_week'.
# imputing the mode for the 2 ratio attributes 'capital_gain' and 'capital_loss'
# since its almost always zero. Hence taking the mean for those would not be the right thing.
# Imputing the Mode for all the nominal and ordinal attributes

for(i in 1:nrow(income))
{
  for( j in 2:(ncol(income)-1))
  {
    if(is.na(income[i,j]) || income[i,j]==" ?"){
      
      if(colnames(income)[j]== "age" || colnames(income)[j]== "hour_per_week"){
        income[i,j] = mean(income[,j])
        
      }else{
        income[i,j] = as.character( Mode(income[,j]) )
      }
      
    }
  }
}

#min max normalization for the 4 ratio attributes

maxage = as.numeric(max(income$age))
minage = as.numeric(min(income$age))
diffage = maxage - minage

maxgain = as.numeric(max(income$capital_gain))
mingain = as.numeric(min(income$capital_gain))
diffgain = maxgain - mingain 

maxloss = as.numeric(max(income$capital_loss))
minloss = as.numeric(min(income$capital_loss))
diffloss = maxloss - minloss 

maxhour = as.numeric(max(income$hour_per_week))
minhour = as.numeric(min(income$hour_per_week))
diffhour = maxhour - minhour 

for(i in 1:nrow(income))
{
  income[i,"age"] = (income[i,"age"]-minage) / diffage
  
  income[i,"capital_gain"] = (income[i,"capital_gain"]-mingain) / diffgain
  
  income[i,"capital_loss"] = (income[i,"capital_loss"]-minloss) / diffloss
  
  income[i,"hour_per_week"] = (income[i,"hour_per_week"]-minhour) / diffhour
}

for (i in 1:nrow(income))
{
  distance=data.frame()
  for(j in 1:nrow(income) )
  {
    #prevent comparison of i-th row with itself
    if(i!=j)
    {
      
      #caclualte distance for nominal data
      
      if(income$workclass[i] == income$workclass[j]) workclassdiff = 0
      else workclassdiff = 1
      if(income$marital_status[i] == income$marital_status[j]) marital_statusdiff = 0
      else marital_statusdiff = 1
      
      if(income$occupation[i] == income$occupation[j]) occupationdiff = 0
      else occupationdiff = 1
      
      if(income$relationship[i] == income$relationship[j]) relationshipdiff = 0
      else relationshipdiff = 1
      
      if(income$race[i] == income$race[j]) racediff = 0
      else racediff = 1
      
      if(income$gender[i] == income$gender[j]) genderdiff = 0
      else genderdiff = 1
      
      if(income$native_country[i] == income$native_country[j]) native_countrydiff = 0
      else native_countrydiff = 1
      
      ndist=(workclassdiff+marital_statusdiff+occupationdiff+
               relationshipdiff+racediff+genderdiff+native_countrydiff)/7
      
      #Calculate Manhattan Distance for ratio data
      rdist = 
        abs(income$age[i]-income$age[j]) + 
        abs(income$capital_gain[i]-income$capital_gain[j]) +
        abs(income$capital_loss[i]-income$capital_loss[j]) +
        abs(income$hour_per_week[i]-income$hour_per_week[j])

      
      #Calculate distance for ordinal data
      odist=abs(income$education_cat[i]-income$education_cat[j])/15
      
      #Final distance is the average of the ordinal, nominal and ratio distances
      dist=(ndist+rdist+odist)/3
      
      #Write this distance to the distance matrix(data frame)
      distance[j,1] = income[i,1]
      distance[j,2]=income[j,1]
      distance[j,3]=dist
    }
  }
  #sort the distance matrix by distance
  distance = distance[order(distance[,3]),]
  
  #pick the top k rows with least distances and write it to the final data frame in required format
  distance=distance[1:k,]
  final[i,1]=distance[1,1]
  count=2
  for (s in 1:k)
  {
    for (t in 2:3)
    {
      final[i,count]=distance[s,t]
      count=count+1  
    }
  }
}

#rename the columns of the final data frame
colnames(final)[1]="Transaction ID"
count=2
for (c in 1:k)
{
  
  colnames(final)[count]=paste("ID", as.character(c)) 
  count=count+1
  colnames(final)[count]=paste("Prox", as.character(c))
  count=count+1
}

write.csv(final,"result_income_manhat.csv")
