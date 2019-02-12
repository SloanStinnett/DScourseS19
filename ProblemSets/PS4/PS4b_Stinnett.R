df1<-as.data.frame(iris)
df<-createDataFrame(iris)
class(df1)
class(df)
head(select(df,df$Sepal_length,df$Species),n=6)
head(filter(df,df$Sepal_length>5.5),n=6)
# if try either of these on df1 you are given an error relating to the fact that they are not compatible with the dataframe class objects.
head(select(df,df$Sepal_length,df$Species),filter(df,df$Sepal_length>5.5),n=6)
head(summarize(groupBy(df, df$Species),
               mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))
df2<-head(summarize(groupBy(df, df$Species),
                    mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))
head(arrange(df2, asc(df2$Species)))
