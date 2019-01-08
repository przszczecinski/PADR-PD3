

setwd("C:/Users/Przemek/Desktop/Przemek/PW/PADR/PD3/2. Prepared Data/Politics")

Posts.politics.XML <- xmlParse("Posts.xml")
Posts.politics <- XML:::xmlAttrsToDataFrame(getNodeSet(Posts.politics.XML, path='//row'))
Users.politics.XML <- xmlParse("Users.xml")
Users.politics <- XML:::xmlAttrsToDataFrame(getNodeSet(Users.politics.XML, path='//row'))
Votes.politics.XML <- xmlParse("Votes.xml")
Votes.politics <- XML:::xmlAttrsToDataFrame(getNodeSet(Votes.politics.XML, path='//row'))


# some of votes in Votes.christianity are assign to posts, that are not in Posts.christianity
result <- Posts.politics %>% 
          select(Id, Body, Title, Score, PostTypeId, Tags) %>% 
          left_join(Votes.politics %>% select(PostId, VoteTypeId), 
                    by=c("Id"="PostId")) 

# picking posts voted for as'Offensive'
result2 <- result %>% filter(VoteTypeId==12)
          
levels(result)

nrow(result2)

head(result2)


re <- Votes.christianity %>% filter((PostId %in% Posts.christianity$Id)==FALSE)

re

nrow(result) + nrow(re) 


Posts.christianity %>% filter(Id==5)
