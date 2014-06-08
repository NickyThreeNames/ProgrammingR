agecount <- function(age=NULL){
  homicides <- readLines("./homicides.txt")
  agelist<-regexpr(" +[0-9]+ +years +old",homicides)
  r<-regmatches(homicides, agelist)
  s<-gsub(" years old","",r)
  u<-table(as.integer(gsub(" ","",s)))
  u2 <- as.data.frame(u, rownames=FALSE)
  u3 <- (u2$Freq[u2$Var1 == age])
  if(length(u3) == 0) {
    return(0)
  } else {
    return(u3)
  }
}