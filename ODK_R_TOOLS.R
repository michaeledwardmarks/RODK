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
            )
        {

    #check if getPass is installed and install it if not
    list.of.packages <- c("getPass")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    require(getPass)

    #check if update.odk.briefcase flag was active (default) and update odk briefcase
    if(update.odk.briefcase==T)
      {
      message("Updating ODK Briefcase")
      odk.briefcase.update()
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
odk.briefcase.pull(aggregate.url = "https://fiebress2.odk.lshtm.ac.uk/fiebress2/",odk.username = "admin",form.id = "meds.survey",export.dir = "MS1",storage.dir = "MS1_Storage",export.filename = "2018.04.23.MS1.csv",exclude.media.export = T,overwrite.csv.export = T,update.odk.briefcase = F)

##################################################################################
#simple call to rmd

odk.convert.csv<-function(format="pdf_document",csv.file,special.field=NULL,form.id=NULL,path.to.pdfmaker.rmd=".")

{
# pull in the csv data
odk.data<-read.csv(csv.file,header = T)

# enter loop for each row of the csv file
for(i in 1:dim(odk.data)[1])
{
  #send a file for the current record to tmp
  write.table(odk.data[i,],file = "tmp.txt",quote = F,sep = "\t",col.names = T,row.names = F)
  #convert datetimes so that reports sort appropriately by date of submission

  odk.data$SubmissionDate<-anytime(odk.data$SubmissionDate)
  nametouse<-gsub(paste(odk.data[i,which(colnames(odk.data)==special.field)],"_",form.id,"_", odk.data$SubmissionDate[i],".pdf",sep=""),pattern = " |:|//",replacement = ".")

  #run RMD script to create report in pdf, pulling in any jpgs
  rmarkdown::render(input = "pdfmaker.Rmd",
                    output_format = "pdf_document",
                    output_file = nametouse,
                    output_dir = "reports")

}
}

##################################################################################

#example
odk.convert.csv(format = "pdf_document",csv.file = "MS1/2018_04_23_MS1.csv",form.id = "medicines.survey",special.field = "participant.id")


































# function to convert CSV data in to a series of PDF files
odk.convert.csv<-function(format="pdf_document",source.row,csv.file,special.field=NULL,form.id=NULL,path.to.pdfmaker.rmd=".")

{
require
df<-read.csv(csv.file,header = T)
df<-df[source.row,]

#check if anytime is installed and install it if not
list.of.packages <- c("anytime")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(anytime)

message(gsub(paste(df[i,which(colnames(df)==special.field)],"_",form.id,"_", df$SubmissionDate[i],".txt",sep=""),pattern = " |:|//",replacement = "."))
nametouse<-gsub(paste(df[i,which(colnames(df)==special.field)],"_",form.id,"_", df$SubmissionDate[i],".txt",sep=""),pattern = " |:|//",replacement = ".")

part1<-paste(
"```{r header, echo=FALSE}\n
pres_title <- ",paste(df[i,which(colnames(df)==special.field)],"_",form.id," : ", df$SubmissionDate[i],sep=""),
"\npres_date <- ", paste('Report generated on ',Sys.Date()),"\n\n\n\n",sep="")

part2<-qw("

```

---
title: `r pres_title`
author: `r pres_author`
date: `r pres_date`
output: pdf_document
classoption: landscape
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r, printoutdata, echo=FALSE}

\n\n\n
```
")


part3<-qw(as.character(list(df[i,])),pattern = ",")
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



























  #send a file for the current record to tmp
  write.table(df[i,],file = "tmp.txt",quote = F,sep = "\t",col.names = T,row.names = F)
  #convert datetimes so that reports sort appropriately by date of submission

  df$SubmissionDate<-anytime(df$SubmissionDate)












  #run RMD script to create report in pdf, pulling in any jpgs
  rmarkdown::render(input = "pdfmaker.Rmd",
                    output_format = format,
                    output_file = gsub(pattern = " |:",replacement = "_",x = paste(special.field,"_",form.id,"_", df$SubmissionDate[i],".pdf", sep='')),
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
paste(special.field,"_",form.id,"_", df$SubmissionDate[i]),
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
output: pdf_document
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

