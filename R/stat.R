#count the number of occurrences of a substring within a string
options(encoding="utf-8")
str_count <- function(x, pattern, sep=""){
  unlist(lapply(
    strsplit(x, sep),
    function(z) na.omit(length(grep(pattern, z)))
  ))
}
#check list current, if it doesn't exist in the list then add this element
add_unique<-function(list,value) {
  ifelse (value %in% list, return(list) , return(c(list,value)) )
}
#show all file un the result
xfile <- function(sep=":")
{
  tryCatch(
  {
  conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
  data <- readLines(conf$result$file)
  lst <- c();
  #read line to line
  for(i in 1:length(data))
  {
    if(nchar(data[i]) > 0)
    {
      v <- unlist(strsplit(data[i], sep))[1]
      lst <- add_unique(lst,v)
    }
  }
  return(lst)
  },
  error=function(cond) {
    message("There are problems in paths, please use command xconfig() for verifying your parameters!")
    return(NA)
  },
  warning=function(cond) {
    message("There are problems in paths, please use command xconfig() for verifying your parameters!")
    return(NULL) 
  },
  finally={
    rm(list=ls())
  })
}
#draw a graph: histogram
xhist <- function(e="")
{
  tryCatch(
  {
  #create a data frame
  conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
  lst_f <- xfile(sep=":")
  value <- c()
  date <- c()
  for(i in 1:length(lst_f))
  {
    value <- c(value,0)
    date <- c(date,0)
  }
  d <- data.frame(file=lst_f,date=date,value_date=value,visible=value)
  data <- readLines(conf$result$file)
  #check entity
  reg = ":\\$:"
  for(i in 1:length(data))
  {
    #get name of file
    f <- unlist(strsplit(data[i],":"))[1]
    #find all on data
    if(e == "")
    {
      #update date
      #format dd.mm.yyyyy
      reg_date1 <- ":([[:digit:]]{2}).([[:digit:]]{2}).([[:digit:]]{4}):" 
      if(grepl(pattern = reg_date1, x = data[i]))
      {
        date <- str_extract(data[i],reg_date1)
        year <- sub(pattern = reg_date1, replacement = "\\3", x = date)
        month <- sub(pattern = reg_date1, replacement = "\\2", x = date)
        d[d$file == f,2] <- paste(month,year,sep=".")
        d[d$file == f,3] <-  as.numeric(paste(year,month,sep=""))
      }
      #format mm.yyyyy
      reg_date1 <- ":([[:digit:]]{2}).([[:digit:]]{4}):" 
      if(grepl(pattern = reg_date1, x = data[i]))
      {
        date <- str_extract(data[i],reg_date1)
        year <- sub(pattern = reg_date1, replacement = "\\2", x = date)
        month <- sub(pattern = reg_date1, replacement = "\\1", x = date)
        d[d$file == f,2] <- paste(month,year,sep=".")
        d[d$file == f,3] <-  as.numeric(paste(year,month,sep=""))
      }
      if(!is.na(f))
      {
        d[d$file == f,4] <- 1  
      }
    }
    else
    {
      #format dd.mm.yyyyy
      reg_date1 <- ":([[:digit:]]{2}).([[:digit:]]{2}).([[:digit:]]{4}):" 
      if(grepl(pattern = reg_date1, x = data[i]))
      {
        date <- str_extract(data[i],reg_date1)
        year <- sub(pattern = reg_date1, replacement = "\\3", x = date)
        month <- sub(pattern = reg_date1, replacement = "\\2", x = date)
        d[d$file == f,2] <- paste(month,year,sep=".")
        d[d$file == f,3] <-  as.numeric(paste(year,month,sep=""))
      }
      #format mm.yyyyy
      reg_date1 <- ":([[:digit:]]{2}).([[:digit:]]{4}):" 
      if(grepl(pattern = reg_date1, x = data[i]))
      {
        date <- str_extract(data[i],reg_date1)
        year <- sub(pattern = reg_date1, replacement = "\\2", x = date)
        month <- sub(pattern = reg_date1, replacement = "\\1", x = date)
        d[d$file == f,2] <- paste(month,year,sep=".")
        d[d$file == f,3] <-  as.numeric(paste(year,month,sep=""))
      } 
     
      #lst <- c(lst,str_count(x = tolower(data[i]), pattern = reg1 , sep="\n"))
      #check 0 or 1 at the end of relation
      if(grepl(pattern = ":1$", x = e) || grepl(pattern = ":0$", x = e))
      {
        reg_entity = paste(":",e,sep="")
      }
      else
      {
        reg_entity = paste(":",e,":",sep="")
      }
      
      count <- str_count(x = tolower(data[i]), pattern = reg_entity , sep="\n")
      if(count > 0)
      {
        #update value in the 
        d[d$file == f,4] <-  count
      } 
    }
  }
  if(e=="")
  {
    ylabel = "All documents" 
  }
  else
  {
    ylabel = paste("Documents contain the key:",e,sep = " ")
  }
  if(length(d[!is.na(d$value_date) & (d$value_date > 0) & (d$visible > 0),3])>0)
  {
    par(las = 0)#load default
    par(mfrow=c(1,1))#1 row, 1 column
    hist(d[!is.na(d$value_date) & (d$value_date > 0) & (d$visible > 0),3], breaks = 12, col="blue",labels=TRUE,xaxt="n",border = "pink",main="Histogram of bulletin: date",xlab="Date",ylab= ylabel)    
    axis(side=1, at = d$value_date, labels= d$date)  
  }
  else
  {
    print("No data available")
  }
  #return a data frame for users check
  return(d)
  },
  error=function(cond) {
    message("Parameters are incorrect or there are problems in paths, please check your parameters!")
    return(NA)
  },
  warning=function(cond) {
    message("Parameters are incorrect or there are problems in paths, please check your parameters!")
    return(NULL) 
  },
  finally={
    rm(list=ls())
  })
}
#draw graph: plot
xplot <- function(e1="",e2="",t="")
{
  tryCatch(
  {
  #create a data frame
  conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
  lst_f <- xfile(sep=":")
  init <- c()
  init_pos <- c()
  for(i in 1:length(lst_f))
  {
    init <- c(init,0)
    init_pos <- c(init_pos,1)
  }
  #create a table for stocking data
  d <- data.frame(file=lst_f,date=init,value_date=init,visible=init_pos)
  data <- readLines(conf$result$file)
  #check entity e1
  #check entity
  reg = ":\\$:"
  reg_req = "";
  if((e1 == "") && (e2 == ""))
  {
    stop("Entity e1 or e2 must have a vulue")
  }
  if(length(e1) > 1)
  {
    stop("Entity e1 has only 0 or 1 parametre")
  }
  if((e1 != "") && (e2 == ""))
  {
    #find only a field e1
    reg_req = paste(":",e1,":",sep="")
    d[,e1] <- init
  }
  if((e1 != "") && (e2 != ""))
  {
    for(j in 1:length(e2))
    {
      d[,paste(e1,"-",e2[j],sep="")] <- init
    }
  }
  if((e1 == "") && (e2 != ""))
  {
    for(j in 1:length(e2))
    {
      d[,e2[j]] <- init
    }
  }
  for(i in 1:length(data))
  {
    #get name of file
    f <- unlist(strsplit(data[i],":"))[1]
    #fill data to data frame
    #add value of year
    #format dd.mm.yyyy
    reg_date1 <- ":([[:digit:]]{2}).([[:digit:]]{2}).([[:digit:]]{4}):" 
    if(grepl(pattern = reg_date1, x = data[i]))
    {
      date <- str_extract(data[i],reg_date1)
      year <- sub(pattern = reg_date1, replacement = "\\3", x = date)
      month <- sub(pattern = reg_date1, replacement = "\\2", x = date)
      d[d$file == f,2] <- paste(month,year,sep=".")
      d[d$file == f,3] <-  as.numeric(paste(year,month,sep=""))
    }
    #format mm.yyyyy
    reg_date1 <- ":([[:digit:]]{2}).([[:digit:]]{4}):" 
    if(grepl(pattern = reg_date1, x = data[i]))
    {
      date <- str_extract(data[i],reg_date1)
      year <- sub(pattern = reg_date1, replacement = "\\2", x = date)
      month <- sub(pattern = reg_date1, replacement = "\\1", x = date)
      d[d$file == f,2] <- paste(month,year,sep=".")
      d[d$file == f,3] <-  as.numeric(paste(year,month,sep=""))
    }
    #add value of entity
    if((e1 != "") && (e2 == "")) 
    {
      #find only a field e1
      if(grepl(pattern=reg,x=data[i]))
      {
        count <- str_count(x = tolower(data[i]), pattern = tolower(reg_req) , sep="\n")
        if(count > 0)
        {
          #update value in the 
          d[d$file == f,5] <-   count
        } 
      }
    }
    else if((e1 == "") && (e2 != ""))
    {
      if(grepl(pattern=reg,x=data[i]))
      {
        for(j in 1:length(e2))
        {
          reg_req = paste(":",e2[j],":",sep="")
          count <- str_count(x = tolower(data[i]), pattern = tolower(reg_req) , sep="\n")
          if(count > 0)
          {
            #update value in the 
            d[d$file == f,j+4] <-   count
          }
        }
      }
    }
    else
    {
      #more than deux entities
      for(j in 1:length(e2))
      {
        reg_req = paste(":",e1,":",e2[j],":",sep="")
        count <- str_count(x = tolower(data[i]), pattern = tolower(reg_req) , sep="\n")
        if(count > 0)
        {
          #update value in the 
          d[d$file == f,j+4] <-  count #d[d$file == f,j+4] + count
        } 
      }
    }
  }
  #filte the data following the time
  #if time
  if(any(t != ""))
  {
    reg_date = "([[:digit:]]{2}).([[:digit:]]{4})"
    if(length(t) == 1) #a precise day 
    { 
      if(grepl(pattern = reg_date, x = t))
      {
        month1 <- sub(pattern = reg_date, replacement = "\\1", x = t[1])
        year1 <- sub(pattern = reg_date, replacement = "\\2", x = t[1])
        date1 = as.numeric(paste(year1,month1,sep=""))
        if(!is.na(date1))
        {
          d[d$value_date != date1,4] <-  0 
        }
      }
      else
      {
        stop("Format of year (mm.yyyy) isn't valid, please check again!")
      }
    }
    if(length(t) == 2)
    {
      if((grepl(pattern=reg_date, x=t[1],perl=FALSE)) && (grepl(pattern=reg_date, x=t[2],perl=FALSE)))
      {
        month1 <- sub(pattern = reg_date, replacement = "\\1", x = t[1])
        year1 <- sub(pattern = reg_date, replacement = "\\2", x = t[1])
        date1 = as.numeric(paste(year1,month1,sep=""))
        month2 <- sub(pattern = reg_date, replacement = "\\1", x = t[2])
        year2 <- sub(pattern = reg_date, replacement = "\\2", x = t[2])
        date2 = as.numeric(paste(year2,month2,sep=""))
        if(!is.na(date1) & !is.na(date2))
        {
          d[d$value_date < date1 | d$value_date > date2 ,4] <-  0
        }
      }
      else
      {
        stop("Format of year (mm.yyyy) isn't valid, please check again!")
      }
    }
    if(length(t) > 2)#interval of date
    {
      stop("value of date isn't valid, there are two choise: a date (ex 02.2010) or interval of date (02.2010,02.2011) , please check again!")
    }
  }
  #draw graphe
  par(las = 0)#load default
  par(mfcol = c(ncol(d)-3,1),mar = c(0.5, 4.0, 0.5, 0.5), oma=c(1, 1, 4, 2))
  test = d[d$value_date > 0 & d$visible == 1,]
  test <- test[order(test$value_date),]
  print(test)
  label_h = 0
  for(i in 5:ncol(d))
  {
    if(nrow(test) > 0)
    {
      plot(test[,i], axes = TRUE, col = ifelse(test[test$value_date > 0,i] > 0, "red","purple"), xaxt="n", ylim=c(0,1), xlab = "",cex=2.0, ylab = colnames(test)[i] ,pch=15 , lty=5)
      if(label_h == 0)
      {
        axis(3, at= 1:length(test[test$value_date>0,3]),labels=test$date,col = "violet", las = 2,col.axis = "blue",cex.lab=0.7,cex=0.7,cex.axis=0.7)
        label_h <- 1
      }
    }
    else
    {
      print("No data available")
    }
  } 
  if(nrow(test) > 0)
  {
    axis(1, at= 1:length(test[test$value_date>0,1]),labels=test$file, col = "violet", las = 2,col.axis = "blue",cex.lab=0.7,cex=0.7,cex.axis=0.7)
    #title("Comparison of every entity in documents")
    #legend("bottomleft", inset=.05, title="Visible",
    #       c("0","1"), fill=terrain.colors(3), horiz=TRUE)
  }
  return(d)
  },
  error=function(cond) {
    message("Error: Parameters are incorrect or there are problems in paths, please check your parameters!")
  },
  warning=function(cond) {
    message("Warning: Parameters are incorrect or there are problems in paths, please check your parameters!")
  },
  finally={
    rm(list=ls())
  })
}
#draw a graph: venn diagram
xvenn <- function(e)
{
  tryCatch(
  {
    if(length(e) < 2)
    {
      stop("This value must be a vector that its length is greater than 2")
    }
    else
    {
      conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
      lst_f <- xfile(sep=":")
      init <- c()
      for(i in 1:length(lst_f))
      {
        init <- c(init,0)
      }
      #create a table for stocking data
      d <- data.frame(file=lst_f)
      for(i in 1:length(e))
      {
        d[,e[i]] <- init
      }
      #read result file
      data <- readLines(conf$result$file)
      for(i in 1:length(data))
      {
        #get name of file
        f <- unlist(strsplit(data[i],":"))[1]
        for(j in 1:length(e))
        {
          reg_entity = paste(":",e[j],":",sep="")
          count <- str_count(x = tolower(data[i]), pattern = reg_entity , sep="\n")
          if(count > 0)
          {
            #update value in the 
            d[d$file == f,j+1] <-  count
          }  
        }
      }
      #Data Synthesis
      x <- as.matrix(d[,2:ncol(d)])
      nprobes <- nrow(x)
      ncontrasts <- ncol(x)
      names <- colnames(x)
      labels <- c()
      if(is.null(names)) names <- paste("Group",1:ncontrasts)
      noutcomes <- 2^ncontrasts
      outcomes <- matrix(0,noutcomes,ncontrasts)
      colnames(outcomes) <- names
      for (j in 1:ncontrasts)
        outcomes[,j] <- rep(0:1,times=2^(j-1),each=2^(ncontrasts-j))
      for(j in 1:ncontrasts)
        outcomes[,j] <- rep(0:1,times=2^(j-1),each=2^(ncontrasts-j))
      for(i in 1:noutcomes)
      {
        name <- ""
        for(j in 1:ncontrasts)
        {
          if(outcomes[i,j] == 1)
          {
            if(name == "")
              name <- names[j]
            else
              name <- paste(name,"&",names[j],sep="")
          }
        }
        if(name != "")
          labels <- c(labels,name)
      } 
      xlist <- list()
      for (i in 1:ncontrasts) xlist[[i]] <- factor(x[,ncontrasts-i+1],levels=c(0,1))
      counts <- as.vector(table(xlist))
      counts <- counts[-1]
      names(counts) <- labels
      #create venn diagram
      vd <- venneuler(counts)
      plot(vd,main="Compare values of entities can apprear simultaneously in bulletins")
      return(counts)
    }
  },
  error=function(cond) {
    message("Parameters are incorrect or there are problems in paths, please check your parameters!")
  },
  warning=function(cond) {
    message("Parameters are incorrect or there are problems in paths, please check your parameters!")
  },
  finally={
    rm(list=ls())
  })
}
