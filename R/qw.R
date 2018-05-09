##################################################################################
#function to allow quote like text as in perl's qw function
qw<-function(x,pattern="\n+") unlist(strsplit(x,pattern))
