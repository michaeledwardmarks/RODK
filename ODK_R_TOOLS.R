##################################################################################
#function to get a new copy of ODK briefcase named odkbriefcase.jar
odk.briefcase.update<-function(folderforsave = ".")
{
  download.file(
      url = "https://opendatakit.org/download/4476/",
      destfile = paste(folderforsave, "/odkbriefcase.jar",sep = ""), mode = "wb")
}

##################################################################################
#function to allow quote like text as in perl's qw function
qw<-function(x,pattern="\n+") unlist(strsplit(x,pattern))
##################################################################################
#function to get an answer to a question interactively
grab.answer<-function(question)
{answer <- as.character(readline(paste(question," : ",sep="")))}
##################################################################################
#function to set the ODK URL and user name
odk.briefcase.settings.online<-function(connection.type="online")
{
URL<-grab.answer("URL")
USERNAME<-grab.answer("Username")
return<-c(URL,USERNAME)
}
##################################################################################
#function to pull data from server defined in URL

odk.briefcase.pull<-
  function(
    aggregate.url=NULL,            #arg  -url   <url>
    odk.directory=NULL,           #arg         <path/to/dir>
    odk.username="admin",         #arg  -u     <username>
    form.id=NULL,                 #arg  -id    <form_id>
    export.dir=".",               #arg  -ed    <path/to/dir>
    storage.dir=".",              #arg  -sd    <path/to/dir>
    export.start.date=NULL,       #arg  -start <yyyy/MM/dd>
    export.end.date=NULL,         #arg -end   <yyyy/MM/dd>
    export.filename=NULL,         #arg  -f     <filename.csv>
    pem_file=NULL,                #arg -pf    <path/to/file.pem>
    exclude.media.export=FALSE,   #flag -em
    overwrite.csv.export=TRUE,   #flag -oc
    update.odk.briefcase=FALSE
            ){

    message("installing packages if needed")

    #check if getPass is installed and install it if not
  message("checking for new packages")
      list.of.packages <- c("getPass")

    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages))
      {
      install.packages(new.packages)
      message (paste("Installing ",new.packages))
    }
    message("loading getPass library")
    library(getPass)

    #check if update.odk.briefcase flag was active (default) and update odk briefcase
   message("Checking to see if Briefcase should be updated.")
      if(update.odk.briefcase==T)
      {
      message("Updating ODK Briefcase")
        download.file(url = "https://opendatakit.org/download/4476/",
          destfile = paste( "./odkbriefcase.jar",sep = ""), mode = "wb")
      }

    #check that only online or offline is being used
    if (!is.null(aggregate.url) & !is.null(odk.directory))
      {
      message("Please only specify one of aggregate.url and odk.directory")
      }


    #add flags for command line call
    if(!is.null(aggregate.url)){aggregate.url<-paste("-url ", aggregate.url, sep="")}            #arg  -url   <url>
    if(!is.null(odk.directory)){odk.directory<-paste("-od ", odk.directory, sep="")}
    odk.username<-paste("-u ", odk.username, sep="")
    form.id<-paste("-id ", form.id, sep="")
    export.dir<-paste("-ed ", export.dir, sep="")
    storage.dir<-paste("-sd ", storage.dir, sep="")
    if(!is.null(export.start.date)){export.start.date<-paste("-start ", export.start.date, sep="")}
    if(!is.null(export.end.date)){export.end.date<-paste("-end ", export.end.date, sep="")}
    if(!is.null(export.filename)){export.filename<-paste("-f ", export.filename, sep="")}
    if(!is.null(pem_file)){pem_file<-paste("-pf ", pem_file, sep="")}
    if(exclude.media.export==T){exclude.media.export<-"-em"}else{exclude.media.export<-""}
    if(overwrite.csv.export==T){overwrite.csv.export<-"-oc"}else{overwrite.csv.export<-""}

    #generate command for system call to odk briefcase

    password<- getPass(msg = "Enter Password for server")
    password<- paste("-p ",password,sep="")
    command<-paste("java -jar odkbriefcase.jar ",
                   aggregate.url,
                   odk.directory,
                   odk.username,
                   form.id,
                   export.dir,
                   export.start.date,
                   export.end.date,
                   export.filename,
                   storage.dir,
                   pem_file,
                   exclude.media.export,
                   overwrite.csv.export,
                   password,
                   sep=" ")
    message(command)

    system(command)
}


#example
odk.briefcase.pull(aggregate.url = "https://roberts-beta-001.odk.lshtm.ac.uk/roberts-beta-001",odk.username = "admin",form.id = "ISNTD",export.dir = "MS1",storage.dir = "MS1_Storage",export.filename = "2018.04.23.MS1.csv",exclude.media.export = T,overwrite.csv.export = T,update.odk.briefcase = T)












##################################################################################
#simple call to rmd

odk.convert.csv<-function(csv.file,special.field=NULL,form.id=NULL,formatoutput="pdf_document")

{
# pull in the csv data
odk.data<-read.csv(csv.file,header = T,stringsAsFactors = F)

#check if anytime is installed and install it if not
list.of.packages <- c("anytime","rmarkdown")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(anytime)
library(rmarkdown)

# enter loop for each row of the csv file
for(i in 1:dim(odk.data)[1])
{
  message(gsub(paste(odk.data[i,special.field],"_",form.id,"_", odk.data$SubmissionDate[i],".txt",sep=""),pattern = " |:|//",replacement = "."))
  nametouse<-gsub(paste(odk.data[i,which(colnames(odk.data)==special.field)],"_",form.id,"_", odk.data$SubmissionDate[i],".Rmd",sep=""),pattern = " |:|//",replacement = ".")
  nametousepdf<-gsub(paste(odk.data[i,which(colnames(odk.data)==special.field)],"_",form.id,"_", odk.data$SubmissionDate[i],".pdf",sep=""),pattern = " |:|//",replacement = ".")
  nametousetitle<-gsub(paste(odk.data[i,which(colnames(odk.data)==special.field)],"_",form.id,"_", odk.data$SubmissionDate[i],sep=""),pattern = " |:|//",replacement = ".")

  write.table(file = nametouse,x = "---",row.names = F,append = T,quote = F,col.names = F)
  write.table(file = nametouse,x = paste("title: ",nametousetitle,sep=""),row.names = F,append = T,quote = F,col.names = F)
  write.table(file = nametouse,x = paste("output: ",formatoutput,sep=""),row.names = F,append = T,quote = F,col.names = F)
  write.table(file = nametouse,x = "name: test",row.names = F,append = T,quote = F,col.names = F)
  write.table(file = nametouse,x = "---",row.names = F,append = T,quote = F,col.names = F)



   for(j in 1:dim(odk.data)[2])
  {




    write.table((paste("**",names(odk.data)[j],"**  ",sep="")),row.names = F,col.names = F,file = nametouse,append = T,quote=F)

    write.table(paste(odk.data[i,j],"  \n",sep=""),row.names = F,col.names = F,file = nametouse,append = T,quote=F)
  }
  write.table(file = nametouse,x = paste("  report created: ",timestamp(),"  ",sep=""),row.names = F,append = T,quote = F,col.names = F)
  write.table(file = nametouse,x = "  ",row.names = F,append = T,quote = F,col.names = F)
  write.table(file = nametouse,x = "![rstudio](blue-250.png)",append = T,row.names = F,col.names = F,quote = F)
  render(nametouse, pdf_document(),params=list(title=nametousetitle,reportdate=Sys.Date()-1))
  file.remove(nametouse) #cleanup
}
}


##################################################################################

#example

odk.convert.csv(csv.file = "MS1/2018_04_23_MS1.csv",special.field = "first_name",form.id = "ISNTD")

a
































# function to convert CSV data in to a series of Podk.data files
odk.convert.csv<-function(format="podk.data_document",source.row,csv.file,special.field=NULL,form.id=NULL,path.to.podk.datamaker.rmd=".")

{
require
odk.data<-read.csv(csv.file,header = T)
odk.data<-odk.data[source.row,]

#check if anytime is installed and install it if not
list.of.packages <- c("anytime")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(anytime)

message(gsub(paste(odk.data[i,which(colnames(odk.data)==special.field)],"_",form.id,"_", odk.data$SubmissionDate[i],".txt",sep=""),pattern = " |:|//",replacement = "."))
nametouse<-gsub(paste(odk.data[i,which(colnames(odk.data)==special.field)],"_",form.id,"_", odk.data$SubmissionDate[i],".txt",sep=""),pattern = " |:|//",replacement = ".")

part1<-paste(
"```{r header, echo=FALSE}\n
pres_title <- ",paste(odk.data[i,which(colnames(odk.data)==special.field)],"_",form.id," : ", odk.data$SubmissionDate[i],sep=""),
"\npres_date <- ", paste('Report generated on ',Sys.Date()),"\n\n\n\n",sep="")

part2<-qw("

```
pres_title <- `This title`
title: `r pres_title`
author: `r pres_author`
date: `r pres_date`
output: podk.data_document
classoption: landscape
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r, printoutdata, echo=FALSE}

\n\n\n
```
")


part3<-qw(as.character(list(odk.data[i,])),pattern = ",")
```{r, printoutdata, echo=FALSE}

for(i in 1:dim(a)[2])
{
  message(names(a)[i])
  message(paste("\t",a[[i]]))
}

```
write.table(file = nametouse,x = part1,row.names = F,col.names = F,quote = T)
write.table(file = nametouse,x = part2,row.names = F,col.names = F,quote = T,append = T)
write.table(file = nametouse,x = part3,row.names = F,col.names = F,quote = T,append = T)
}














rmarkdown::render(input = "report.Rmd",
                  output_file = "report.html",
                  params = list(wc=getWordcloud(),
                                table=getTab(),
                                pie=getPie()))












  #send a file for the current record to tmp
  write.table(odk.data[i,],file = "tmp.txt",quote = F,sep = "\t",col.names = T,row.names = F)
  #convert datetimes so that reports sort appropriately by date of submission

  odk.data$SubmissionDate<-anytime(odk.data$SubmissionDate)












  #run RMD script to create report in podk.data, pulling in any jpgs
  rmarkdown::render(input = "podk.datamaker.Rmd",
                    output_format = format,
                    output_file = gsub(pattern = " |:",replacement = "_",x = paste(special.field,"_",form.id,"_", odk.data$SubmissionDate[i],".podk.data", sep='')),
                    output_dir = "reports",
                    )

}

}


fileConn<-file("output.txt")

writeLines(
rmdtext<-
paste(
"
```{r header, echo=FALSE}
pres_title <- ",
paste(special.field,"_",form.id,"_", odk.data$SubmissionDate[i]),
"\npres_date <- ",
paste("Report generated on ",Sys.Date()),
"```"
,sep="")
)
close(fileConn)

---
  title: `r pres_title`
author: `r pres_author`
date: `r pres_date`
output: podk.data_document
classoption: landscape
---

  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, printoutdata, echo=FALSE}

for(i in 1:dim(a)[2])
{
  message(names(a)[i])
  message(paste("\t",a[[i]]))
}
"
```

```{r , images, results='asis', echo=FALSE}
jpgs<-a[1,grep("jpeg|jpg",x = a[1,])]
if(length(jpgs>0))
{
  for(i in 1:length(jpgs))
  {jpgs[i]<-paste(getwd(),"/TMIH_Example/",jpgs[i],sep="")}
  res <- paste0("![](", jpgs, ")<br><br>")
  cat(res)
}
```





###add command to save a stata file

