rm(list=ls())

Data <- OSCARtoCSV("testDB.xml")

write.csv(Data,"testDB.csv")

OSCARtoCSV <- function(xmlfile){
  
  library(XML)
  xtree <- xmlTreeParse(xmlfile,useInternal=TRUE)
  root <- xmlRoot(xtree)
  
  DateTime <- strsplit(xpathSApply(root,"//DateTime",xmlValue),"T")
  
  IDData <- xpathSApply(root,"//IDData",xmlValue)
  
  Data <- strsplit(xpathSApply(root,"//Data",xmlValue),'\n')
  
  DataFrame <- data.frame(
    "IDData"=as.character("Empty"),
    "Date"=as.character("Empty"),
    "Time"=as.character("Empty"),
    "Comment"=as.character("Empty"),
    "CommentSub1"=as.character("Empty"),
    "CommentSub2"=as.character("Empty"),
    "CommentSub3"=as.character("Empty"),
    "Wavelength"=0,
    "Absorbtion"=0,
    stringsAsFactors=FALSE
  )
  
  for(i in 1:xmlSize(root)){
    for(j in 2:length(Data[[i]])){
      
      DataFrame <- rbind(DataFrame,c(
        as.character(IDData[i]),
        as.character(DateTime[[i]][1]),
        as.character(DateTime[[i]][2]),
        xmlValue(xmlChildren(xmlChildren(root)[[i]])[c("Comment")][[1]]),
        xmlValue(xmlChildren(xmlChildren(root)[[i]])[c("CommentSub1")][[1]]),
        xmlValue(xmlChildren(xmlChildren(root)[[i]])[c("CommentSub2")][[1]]),
        xmlValue(xmlChildren(xmlChildren(root)[[i]])[c("CommentSub3")][[1]]),
        as.numeric(strsplit(Data[[i]][j]," ")[[1]][2]),
        as.numeric(strsplit(Data[[i]][j]," ")[[1]][3])
      ))
    }
    cat(paste(c(i,"out of",xmlSize(root),IDData[i],"\n")))
  }
  DataFrame <- DataFrame[-1,]
  row.names(DataFrame) <- NULL
  return(DataFrame)
}

