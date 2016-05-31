library(shiny)
library(pwr)
library(samplesize)
#F-test two tail power
pwr=function(var1,var2,n1,n2,a1){
	ratio=var1/var2
	df1=n1-1
	df2=n2-1
	f1=qf(1-a1/2,df1,df2,lower.tail=F)
	f2=qf(a1/2,df1,df2,lower.tail=F)
	beta=pf(ratio^(-1)*f1,df1,df2,lower.tail=F)-pf(ratio^(-1)*f2,df1,df2,lower.tail=F)
	power=1-beta
	return(power)
}

#F-test two tail required sample size
fsample=function(a,pwr,ratio,ar){
	for (n in 2:1000){
		df1=n-1
		df2=round(ar*df1)
		if(df2 < 1) df2=1
		f1=qf(1-a/2,df1,df2,lower.tail=F)
		f2=qf(a/2,df1,df2,lower.tail=F)
		beta=pf(ratio^(-1)*f1,df1,df2,lower.tail=F)-pf(ratio^(-1)*f2,df1,df2,lower.tail=F)
		power=1-beta
		if (round(power,3) >= pwr) break
	}
	n1=n
	n2=df2+1
	return(list(n1,n2))
}

#calculate effective size d
d=function(mu1,mu2,var1,var2,n1,n2){
	d=(mu1-mu2)/sqrt(((n1-1)*var1+(n2-1)*var2)/(n1+n2-2))
}
#output
shinyServer(function(input, output) {
        output$text1 <- renderUI({
            txt1<-paste("Using the Contractor's Risk,",paste(tags$i(HTML("&alpha;")),",",sep=""),"of",input$a1,"with the sample size given, the risk of the State DOT not detecting a difference between the variances of State DOT's and Contractor's sample data when one exists,",paste(tags$i(HTML("&beta;")),",",sep=""),"is",paste(tags$code(1-round(pwr(input$var1,input$var2,input$n1,input$n2,as.numeric(input$a1)),4)),".",sep=""),"Thus the power of the test, (1-",HTML(paste(tags$i(HTML("&beta;")),")",sep="")),"is",paste(tags$code(round(pwr(input$var1,input$var2,input$n1,input$n2,as.numeric(input$a1)),4)),".",sep=""),"
            		   This indicates that there is",tags$code(sprintf("%1.2f%%",100*round(pwr(input$var1,input$var2,input$n1,input$n2,as.numeric(input$a1)),4))),"probability of detecting a difference in the variances.")
            txt2<-paste(tags$i(HTML("&beta;" )),"=", 1-round(pwr(input$var1,input$var2,input$n1,input$n2,as.numeric(input$a1)),4))
            txt3<-paste("The power of test (1 -",HTML(paste(tags$i(HTML("&beta;")),")",sep="")),"is", round(pwr(input$var1,input$var2,input$n1,input$n2,as.numeric(input$a1)),4))
            HTML(paste(txt1,txt2,txt3,sep="<br><br>"))
        })
        output$text2<- renderUI({
        	txt1<-paste("Using the Contractor's Risk,",paste(tags$i(HTML("&alpha;")),",",sep=""),"of",input$a1,"with the sample size given, the risk of the State DOT not detecting a difference between the means of State DOT and Contractor's sample data when one exists ",tags$i(HTML("&beta;")),"is",paste(tags$code(1-round(pwr.t2n.test(input$n1,input$n2,d=d(input$mu1,input$mu2,input$var1,input$var2,input$n1,input$n2),as.numeric(input$a1))$power,4)),".",sep=""),"Thus the power of the test, (1-",HTML(paste(tags$i(HTML("&beta;")),")",sep="")),"is",paste(tags$code(round(pwr.t2n.test(input$n1,input$n2,d=d(input$mu1,input$mu2,input$var1,input$var2,input$n1,input$n2),as.numeric(input$a1))$power,4)),".",sep="")
        		  ,"This indicates that there is",tags$code(sprintf("%1.2f%%",100*(round(pwr.t2n.test(input$n1,input$n2,d=d(input$mu1,input$mu2,input$var1,input$var2,input$n1,input$n2),as.numeric(input$a1))$power,4)))),"probability of detecting a difference in the means."
        		  )
        	txt2<- paste(tags$i(HTML("&beta;" )),"=",1-round(pwr.t2n.test(input$n1,input$n2,d=d(input$mu1,input$mu2,input$var1,input$var2,input$n1,input$n2),as.numeric(input$a1))$power,4))
        	txt3<- paste("The power of test (1 -",HTML(paste(tags$i(HTML("&beta;")),")",sep="")),"is", round(pwr.t2n.test(input$n1,input$n2,d=d(input$mu1,input$mu2,input$var1,input$var2,input$n1,input$n2),as.numeric(input$a1))$power,4))
        	HTML(paste(txt1,txt2,txt3,sep="<br><br>"))
        })
        output$text3 <- renderUI({
        	str <- paste("With the inputs:")
        	str1<- paste(tags$i(HTML("&alpha;")),"=",as.numeric(input$a1))
        	str2<- paste("The ratio of standard deviation λ=",round(sqrt(input$var1/input$var2),4))
        	str3<- paste("The ratio of variance =",round(input$var1/input$var2,4))
        	str4<- paste(paste("n",tags$sub("State."),sep=""),"=", input$n1)
        	str5<- paste(paste("n",tags$sub("Contr."),sep=""),"=",input$n2)
        	str6<- paste(tags$i(HTML("&beta;" )),"=", 1-round(pwr(input$var1,input$var2,input$n1,input$n2,as.numeric(input$a1)),4))
        	str7<-paste("The power of test (1 -",HTML(paste(tags$i(HTML("&beta;")),")",sep="")),"is", round(pwr(input$var1,input$var2,input$n1,input$n2,as.numeric(input$a1)),4))
        		
        	HTML(paste(str,str1,str2,str3,str4,str5,str6,str7,sep="<br>"))
        })
        output$text4 <- renderUI({
        	str <- paste("With the inputs:")
        	str1<- paste(tags$i(HTML("&alpha;")),"=",as.numeric(input$a1))
        	str2<- paste("The allowable difference ",paste(tags$i(HTML("d")),"*",sep=""),"=",round(d(input$mu1,input$mu2,input$var1,input$var2,input$n1,input$n2),4))
        	str3<- paste(paste("n",tags$sub("State."),sep=""),"=", input$n1)
        	str4<- paste(paste("n",tags$sub("Contr."),sep=""),"=",input$n2)
        	str5<- paste(tags$i(HTML("&beta;" )),"=",1-round(pwr.t2n.test(input$n1,input$n2,d=d(input$mu1,input$mu2,input$var1,input$var2,input$n1,input$n2),as.numeric(input$a1))$power,4))
        	str6<- paste("The power of test (1 -",HTML(paste(tags$i(HTML("&beta;")),")",sep="")),"is", round(pwr.t2n.test(input$n1,input$n2,d=d(input$mu1,input$mu2,input$var1,input$var2,input$n1,input$n2),as.numeric(input$a1))$power,4))
        	HTML(paste(str,str1,str2,str3,str4,str5,str6,sep="<br>"))
        })
        output$text5<- renderUI({
        	if (input$ar != 1){
        		str <-paste("To achieve a power of test value of at least",input$pwr,"of being able to detect a difference between the State DOT”s and Contractor’s variances:")
        		str1 <- paste("The required sample size for State DOT data is",tags$code(fsample(as.numeric(input$a2),input$pwr,input$var3/input$var4,input$ar)[1]))
				str2 <- paste("The required sample size for Contractor data is",tags$code(fsample(as.numeric(input$a2),input$pwr,input$var3/input$var4,input$ar)[2]))
				HTML(paste(str,str1,str2,sep="<br>"))
        	}
        	else {str <-paste("To achieve the selectd power value of at least",input$pwr,"to detect a difference in variances between the two data sets
        					using equal sample sized, the required sample size for each would be",tags$code(fsample(as.numeric(input$a2),input$pwr,input$var3/input$var4,input$ar)[1]))
				HTML(paste(str))
				}
        })
       
        output$text6 <- renderUI({
        	if (input$ar != 1) {
        	str <- paste("To achieve a power of at least",input$pwr,"to detect a difference between the State DOT”s and Contractor’s means:")
        	str1 <- paste("The required sample size for the State DOT data is",tags$code(n.ttest(power=input$pwr,alpha=as.numeric(input$a2),mean.diff=input$mu3-input$mu4,
        																sd1=sqrt((input$n3-1)*input$var3+(input$n4-1)*input$var4/(input$n3+input$n4-2)),k=input$ar,fraction="unbalanced")[[2]]))
        	str2 <- paste("The required sample size for the Contractor data is",tags$code(n.ttest(power=input$pwr,alpha=as.numeric(input$a2),mean.diff=input$mu3-input$mu4,
        																sd1=sqrt((input$n3-1)*input$var3+(input$n4-1)*input$var4/(input$n3+input$n4-2)),k=input$ar,fraction="unbalanced")[[3]]))
			HTML(paste(str,str1,str2,sep="<br>"))   
        	}
        	else {str <- paste("To achieve the selectd power value of at least",input$pwr,"to detect a difference in means between the two data sets
        					using equal sample sized, the required sample size for each would be",tags$code(n.ttest(power=input$pwr,alpha=as.numeric(input$a2),mean.diff=input$mu3-input$mu4,
        																sd1=sqrt((input$n3-1)*input$var3+(input$n4-1)*input$var4/(input$n3+input$n4-2)),k=input$ar,fraction="unbalanced")[[2]]))
        	HTML(paste(str))
        	}
        })
        
        output$text7 <- renderUI({
        	str <- paste("With the inputs:")
        	str1<- paste(tags$i(HTML("&alpha;")),"=",as.numeric(input$a2))
        	str2<- paste("The ratio of standard deviation λ=",round(sqrt(input$var3/input$var4),4))
        	str3<- paste("The ratio of variance =",round(input$var3/input$var4,4))
        	str4<- paste("Expected Power of test =", input$pwr)
        	str5<- paste("Allocation ratio",paste("n",tags$sub("Contr."),"/n",tags$sub("State."),sep=""),"=",input$ar)
        	str6<- paste("The optimized sample size of",paste("n",tags$sub("State."),sep=""),"=", fsample(as.numeric(input$a2),input$pwr,input$var3/input$var4,input$ar)[1])
        	str7<- paste("The optimized sample size of",paste("n",tags$sub("Contr."),sep=""),"=",fsample(as.numeric(input$a2),input$pwr,input$var3/input$var4,input$ar)[2])
        	HTML(paste(str,str1,str2,str3,str4,str5,str6,str7,sep="<br>"))
        })
        output$text8 <- renderUI({
        	str <- paste("With the inputs:")
        	str1<- paste(tags$i(HTML("&alpha;")),"=",as.numeric(input$a2))
        	str2<- paste("Expected Power of test =", input$pwr)
        	str3<- paste("Allocation ratio",paste("n",tags$sub("Contr."),"/n",tags$sub("State."),sep=""),"=",input$ar)
        	str4<- paste("The optimized sample size of",paste("n",tags$sub("Contr."),sep=""),"=",n.ttest(power=input$pwr,alpha=as.numeric(input$a2),mean.diff=input$mu3-input$mu4,
        																	sd1=sqrt(((input$n3-1)*input$var3+(input$n4-1)*input$var4)/(input$n3+input$n4-2)),k=input$ar,fraction="unbalanced")[[2]])
        	str5<- paste("The optimized sample size of",paste("n",tags$sub("State."),sep=""),"=",n.ttest(power=input$pwr,alpha=as.numeric(input$a2),mean.diff=input$mu3-input$mu4,
        																	sd1=sqrt(((input$n3-1)*input$var3+(input$n4-1)*input$var4)/(input$n3+input$n4-2)),k=input$ar,fraction="unbalanced")[[3]])
        	HTML(paste(str,str1,str2,str3,str4,str5,sep="<br>"))
        })
})