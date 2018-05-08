#########################################################################################
# ODK hardcopy in PDF v 1.0
#########################################################################################
#
# Step one of this script is a call to ODK briefcase via the command line, which pulls all
# records and media.
#
# Step two is to output individual records to a temp file, then bind their data with
# any images to make a pdf document.
#
# This work is licensed under Creative Commons Attribution-ShareAlike 3.0 Unported License
# http://creativecommons.org/licenses/by-sa/3.0/"
#########################################################################################
# SETUP
#########################################################################################
# Enter form IDs in double quotes separated by commas

form_id<-c("Medicines_Survey_1","Photos","social science audio","Social Science contact summary","Social Science general observations")


#########################################################################################


#########################################################################################

# Requirements
#

# This script needs latex and pandoc (external installations, use homebrew on mac)
#

#Download and install these
# https://pandoc.org/installing.html
# https://www.latex-project.org/get/


# this tests if the required R packages are installed. If not, it will install them.
# also loads packages

packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
packages(getPass)
packages(knitr)
packages(readr)
packages(dplyr)
packages(anytime)

########################################################################################
# call to ODK briefcase
# use full path including first subdirectory
# ie. https://projectx.odk.lshtm.ac.uk/projectx/
# any passwords with special characters will need escape characters
# i.e. password doggy!prunes12 needs to be specified as doggy\\!prunes12
# obviously shouldn't hardcode passwords. Better to call from the console using a readline() command but I am lazy
# Also need to specify the targets for input and output. Here TMIH_example
# I also renamed the jar file for ODK briefcase because I hate whitespace

#get the url for the server

serverurl<-"https://fiebress2.odk.lshtm.ac.uk/fiebress2/"
password<-getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE)
formname<-
command<- paste('java -jar ODK_Briefcase.jar -url ', serverurl,' -p ' password ' -u admin -id ', formname, ' -ed ./',formname, ' -sd ./',formname, ' -f', formname,'_CSV.csv',sep="")
system(command)

# pull in the csv data
df<-read.csv("TMIH_Example/TMIH_Example.csv",header = T)

# enter loop for each row of the csv file
for(i in 1:dim(df)[1])
{
  #send a file for the current record to tmp
  write.table(df[i,],file = "tmp.txt",quote = F,sep = "\t",col.names = T,row.names = F)
  #convert datetimes so that reports sort appropriately by date of submission

  df$SubmissionDate<-anytime(df$SubmissionDate)

  #run RMD script to create report in pdf, pulling in any jpgs
  rmarkdown::render(input = "pdfmaker.Rmd",
                    output_format = "pdf_document",
                    output_file = gsub(pattern = " |:",replacement = "_",x = paste(df$SubmissionDate[i], "_YOUR_FORM_NAME_HERE.pdf", sep='')),
                    output_dir = "reports")

}

#This work is licensed under Creative Commons Attribution-ShareAlike 3.0 Unported License
#http://creativecommons.org/licenses/by-sa/3.0/"

#Nuff respect to VP Nagraj [twitter @vpnagraj] from whose scripts I reused some of the control structure for calling RMD from another R script
#See http://nagraj.net/notes/multiple-rmarkdown-reports/



