#this is a function main which it will call the main script
options("encoding" = "UTF-8")
xparse <- function()
{
    path    = paste(.libPaths()[1], "x.ent/Perl", sep='/')
    command = paste("perl -I \"",path, "\" \"",path, "/", "Main.pl\"", sep='')
    print(command)
    print("Please, wait ....")
    test <- try(system(command, intern=TRUE,wait=TRUE))
    #rm(list=ls())
    gc(verbose=T) #garbage collection to free up memory
    print(test)
    print("Let use functions for viewing the results: xshow(...), xhist(...), xplot(...)....")
}
xentity <- function()
{
  conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
  lst_tag <- c();
  #dico
  if(length(conf$dico$tag) > 0)
  {
    for(i in 1:length(conf$dico$tag))
    {
      lst_tag <-add_unique(lst_tag,conf$dico$tag[i])
    }
  }
  #unitex
  if(length(conf$unitex$result$tag) > 0)
  {
    for(i in 1:length(conf$unitex$result$tag))
    {
      lst_tag <-add_unique(lst_tag,conf$unitex$result$tag[i])
    }
  }
  return(lst_tag)
}
xrelation<- function()
{
  conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
  lst_tag <- c();
  #relation
  if(length(conf$relation$link) > 0)
  {
    for(i in 1:length(conf$relation$link))
    {
      lst_tag <-add_unique(lst_tag,conf$relation$link[i])
    }
  }
  return(lst_tag)
}
xshow <- function(cols="",sort="a")
{
  tryCatch(
  {
    #create a data frame
    conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
    path = conf$result$file;
    lst_f <- xfile(sep=":")
    #get all tags in the file config
    lst_tag <- xentity();
    lst_tag <-add_unique(lst_tag,xrelation())
    dta <- data.frame()
    reg_ent = ":\\$:"
    reg_rel = ":\\$\\$:"
    if(length(cols) == 1)#if 1 argument or argument default
    {
      if(nchar(trim(cols)) == 0)#argument default, get all data
      {
        dta <- data.frame(file=lst_f)
        for(i in 1:length(lst_tag))
        {
          dta[,lst_tag[i]] <- "N/A" 
        }
        text <- readLines(path)
        for(i in 1:length(text))
        {
          #get name of file
          f <- unlist(strsplit(text[i],":"))[1]
          #find entity
          if(grepl(pattern=reg_ent,x=text[i]))#entity
          {
            data_ele <- ""
            eles <- unlist(strsplit(text[i],":"))
            if(length(eles) == 4)
            {
              data_ele <- eles[4]
            }
            else if(length(eles) >= 5)
            {
              data_ele <- eles[4]
              for(j in 5:length(eles))
              {
                data_ele <- paste(data_ele,eles[j],sep="; ")
              } 
            }
            dta[dta$file == f,eles[2]] <- data_ele
          }
          #relation
          if(grepl(pattern=reg_rel,x=text[i]))
          {
            eles <- unlist(strsplit(text[i],reg_rel))
            col = sub(pattern = paste(f,":",sep=""), replacement = "",x = eles[1])
            col = gsub(":$", "",col, perl=TRUE)#delete ":" at the end of sentence
            if(col %in% names(dta))
            {
              if(dta[dta$file == f,col] == "N/A")
              {
                dta[dta$file == f,col] <- eles[2] 
              }
              else
              {
                dta[dta$file == f,col] <- paste(dta[dta$file == f,col],eles[2],sep =";")  
              }
            }
          }
        }
      }
      else#get a column from user
      {
        if(cols %in% lst_tag)#only a column
        {
            lst <- c()
            #data frame, local parameter, use as hash
            dt <- data.frame(value=character(0),freq=integer(0),stringsAsFactors=FALSE)#store values of column and frequency found
            text <- readLines(path)
            for(i in 1:length(text))
            {
              #get name of file
              f_name <- unlist(strsplit(text[i],":"))[1]
              #creat a regular expression ex=> filename:p:$:
              reg = paste(f_name,":",cols,reg_ent,sep="")
              if(grepl(pattern=reg,x=text[i]))#entity
              {
                #get value
                eles <- unlist(strsplit(text[i],":"))
                for(j in 4:length(eles))
                {
                  #lst <- add_unique(lst,eles[j])
                  if(length(dt$value[dt$value %in% eles[j]]) > 0)
                  {
                    dt[dt$value == eles[j],2] = dt[dt$value == eles[j],2] + 1;
                  }
                  else#add new value
                  {
                    dt <- rbind(dt,data.frame(value=eles[j],freq=1))
                  }
                } 
                next
              }
              #creat a regular expression ex=> filename:p:$$:
              reg = paste(f_name,":",cols,reg_rel,sep="")
              if(grepl(pattern=reg,x=text[i]))#relation
              {              
                #replace le result file:p:s:$$:e1:e2:1
                result = gsub(reg, "",text[i], perl=TRUE)
                #result = gsub(":$", "",result, perl=TRUE)
                #lst <- add_unique(lst,result)
                if(length(dt$value[dt$value %in% result]) > 0)
                {
                  dt[dt$value == result,2] = dt[dt$value == result,2] + 1;
                }
                else#add new value
                {
                  dt <- rbind(dt,data.frame(value=result,freq=1))
                }
              }
            }
            #convert to vector
            value <- as.vector(dt$value)
            freq <- as.vector(dt$freq)
            dt <- data.frame(value=value,freq=freq)
            if(sort == "a")
            {
              dt <- dt[order(dt$value,-dt$freq),]
            }
            else
            {
              dt <- dt[order(-dt$freq,dt$value),]
            }
            lst <- dt$value
            frq <- dt$freq
            dta <- data.frame(value=lst,freq=frq)#global parameter
            if(nrow(dta)>0)
            {
              dta$freq = formatC(dta$freq,digits=0,format="f") 
            }
        }
        else
        {
          print("The tag isn't valid!")
        }
      }
    }
    else #a vecter paramete
    {
      dta <- data.frame(file=lst_f)
      for(i in 1:length(cols))
      {
        dta[,cols[i]] <- "N/A" 
      }
      text <- readLines(path)
      for(i in 1:length(text))
      {
        #get name of file
        f <- unlist(strsplit(text[i],":"))[1]
        for(j in 1:length(cols))
        {
          #entity
          reg = paste(f,":",cols[j],reg_ent,sep="")
          #find all
          if(grepl(pattern=reg,x=text[i]))#entity
          {
            data_ele <- ""
            eles <- unlist(strsplit(text[i],":"))
            if(length(eles) == 4)
            {
              data_ele <- eles[4]
            }
            else if(length(eles) >= 5)
            {
              data_ele <- eles[4]
              for(j in 5:length(eles))
              {
                data_ele <- paste(data_ele,eles[j],sep="; ")
              } 
            }
            dta[dta$file == f,eles[2]] <- data_ele
          }
          #relation
          reg = paste(f,":",cols[j],reg_rel,sep="")
          if(grepl(pattern=reg,x=text[i]))#relation
          {
            result = gsub(reg, "",text[i], perl=TRUE)
            if(cols[j] %in% names(dta))
            {
              if(dta[dta$file == f,cols[j]] == "N/A")
              {
                dta[dta$file == f,cols[j]] <- result 
              }
              else
              {
                dta[dta$file == f,cols[j]] <- paste(dta[dta$file == f,cols[j]],result,sep =";")  
              }
            }
          }
        }
      }
    }
    path = paste(.libPaths()[1],"x.ent/www/output.html",sep="/")
    html= print(xtable(dta),"html",file=path)
    html = gsub(";", "<br/>",html, perl=TRUE)
    html = paste("<meta http-equiv=Content-Type content=text/html; charset=utf-8>",html,sep="")
    write(html, file=path)
    browseURL(path)
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


