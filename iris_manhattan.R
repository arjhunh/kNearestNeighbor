data=read.csv("C:/Users/arjhu_000/Desktop/Assignment 1/iris.csv")
setwd("C:/Users/arjhu_000/Desktop/Assignment 1")
k=5

final=data.frame()
new=data[,1:4] #data frame without the class attribute

#calculating the manhattan distance
for(i in 1:nrow(new))
{
  distance = data.frame()
  for(j in 1:nrow(new))
  {
    if(i!=j)
    {
      dist=0
      
      for (n in 1:ncol(new))
      {
        dist = dist + abs(new[i,n]-new[j,n])  
      }
      #Write this distance to the distance matrix(data frame)
      distance[j,1] = i
      distance[j,2]=j
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

write.csv(final,"result_iris_manhat.csv")
