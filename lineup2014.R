###### Settings
library(XML)
library(stringr)

###### URLs
url<-paste0("http://stats-for-the-nba.appspot.com/ratings/2013.html")
len<-length(url)

###### Reading data
tbl <- readHTMLTable(url[1])[[1]]
tbl <- tbl[,-6]
colnames(tbl) <- c("Player","Offense/100","Defense/100","Off+Deff/200","Poss")

lineup.2014 <- read.csv("~/Desktop/Papers/2014lineups.csv")
lineup.2014 <- lineup.2014[order(-lineup.2014$MP),] #order by minutes played
#############################################################

player.names <- NULL
split <- NULL
dts <- NULL
k <- matrix(NA,nrow=1,ncol=51)
colnames(k) <- colnames(player.final)

for (i in 1:nrow(lineup.2014)) { 
  row <- NULL
  player.names<-as.character(lineup.2014[i,2])
  split<-strsplit(player.names,split="|",fixed=TRUE)
  
  for (j in 1:5){ 
    mid <- NULL
    v <- NULL
    v <- split[[1]][j]
    v <- strsplit(v,split=".",fixed=TRUE)
    first <- str_replace_all(string=v[[1]][1], pattern=" ", repl="")
    str_replace_all(string=as.character(player.final[,1]), pattern=" ", repl="")
    last <- str_replace_all(string=v[[1]][2], pattern=" ", repl="")
    index <- intersect(
      grep(pattern = first, x = str_replace_all(
        string=as.character(player.final[,1]), pattern=" ", repl=""),
        ignore.case = T
      ),
      grep(pattern = last, x = str_replace_all(
        string=as.character(player.final[,1]), pattern=" ", repl=""),
        ignore.case = T
      )
    )
    l <- as.character(player.final[index,4]) == as.character(lineup.2014[i,3])
    if (length(index)>0&&j==1){
      first.row <- player.final[index[1],]
      row <- first.row
      } else if (length(index)>0&&j!=1){
        mid <- player.final[index[1],]
        row <- cbind(row,mid,row.names = NULL)
        } else{
          row<-cbind(row,k,row.names = NULL)
          }
    }
  row<-row[1,]
  if (i==1) dts<-row
  else dts<-rbind(dts,row)
}

final.dts <- dts %>% 
  filter(complete.cases(.))

lineup.2014 <- lineup.2014 %>% 
  filter(complete.cases(.))
  
