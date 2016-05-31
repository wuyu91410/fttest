library(shiny)
# Note: this app now is default for two tails test
shinyUI(
	fluidPage(
		theme="bootstrap.css",			  
		br(),
		img(src = "FHWA.jpg",height=48,width=200),
		titlePanel("Determination of State DOT Risk and Optimum Sample Size "),
		br(),
		br(),
		navbarPage("",
				   tabPanel("Introduction",
				   		 h3(p("This software analysis includes four menus. In the simplest terms, one determines the State DOT’s Risk given the sample size; 
				   		    and the second determines the sample size given the State DOT’s Risk.Two other menus - Glossary defines several terms that apply to this program and Equations provides the equations used in this program."
				   		    ),
				   		 	tags$ul(
				   		 		tags$li(p("The \"State DOT's Risk\" menu determines the State DOT’s Risk (Type II Risk or",tags$i(HTML("&beta;")),"Risk) based on the Contractor’s Risk (Type I Risk or",tags$i(HTML("&alpha;")),"Risk) 
				   		 				and the sample size n.",
				   		 				tags$ul(
												tags$li(p("Under the \"State DOT's Risk\" tab are three tabs, \"Summary\",", HTML(paste(tags$i("\"F-"),"test\", and",sep="")),HTML(paste(tags$i("\"t-"),"test\".",sep="")),"The summary tab provides the result of both the ", tags$i("F-"),"and ", tags$i("t-"),"test analyses.
				   		 								The ", tags$i("F-"),"and", tags$i("t-"),"test tabs provide the details of the analysis of each of these statistical tests."
				   		 								)),
				   		 						tags$li(p("The State DOT's Risk,", HTML(paste(tags$i(HTML("&beta;")),",")),"is the probability of not detecting a difference between the State's and Contractor's data sets when one exists. It is desirable that this risk be as low as practical. The
				   		 								State DOT's Risk can be adjusted by changing, either or both the Contractor's Risk,",tags$i(HTML("&alpha;")),",and sample size, n."
				   		 								))
				   		 						)
				   		 				)),
				   		 		tags$li(p("The \"Optimum Sample Size\" menu determines the Optimum Sample Size for both the", tags$i("F-"),"and",tags$i("t-"),"tests based on the Contractor’s Risk and the desired power of test value. 
				   		 				The user has the option of using the same sample size for both the State DOT and the Contractor or different sample sizes. It is important to recognize that the sample size required 
				   		 				for the selected risk value may be larger than anticipated.",
				   		 				tags$ul(
												tags$li(p("Under the \"Optimum Sample Size\" tab are three tabs, \"Summary\", ", tags$i("\"F-"),"test\", and ", tags$i("\"t-"),"test\". The summary tab provides the result of 
				   		 								both the", tags$i("F-"),"and ", tags$i("t-"),"test analyses. The ", HTML(paste(tags$i("\"F-"),"test\" and",sep="")),HTML(paste(tags$i("\"t-"),"test\"",sep="")),"tabs provide the details of the analysis of each of these statistical tests"
				   		 								)),
				   		 						tags$li(p("The \"power of test\" (1-", HTML(paste(tags$i(HTML("&beta;")),")",sep="")),"is the probability of detecting a difference between the State's and Contractor's data sets when one exists."
				   		 								)),
				   		 						tags$li(p("The Optimum Sample Size can be adjusted by changing either or both, the Contractor's Risk,", HTML(paste(tags$i(HTML("&alpha;")),",",sep="")),"and \"power of test\" value. It is desirable to select a \"power of test\" value as high as practical."
				   		 								)),
				   		 						tags$li(p("It is important to recognize that the sample size, n, required for the selected \"power of test \" may be larger than anticipated.")
				   		 						))
				   		 				))
				   		 		))
				   		 		),
				   tabPanel("State DOT's Risk",
				   		 h3(p(
							"In this menu, the State DOT’s risk (Type II Risk,",HTML(paste(tags$i(HTML("&beta;")),")",sep="")),"is determined. This is an important analysis because with typical sample sizes, this risk is higher than specification writers are often aware. This risk is the probability of not
				   		    detecting a difference between the State DOT's and Contractor's data sets when one exists. (Please see more details in the \"Guidance\" tab)"
				   		    )),
				   		 sidebarLayout(
				   		 	sidebarPanel(
				   		 		h4("State DOT's Data"),
				   		 		numericInput("mu1","Sample Mean",2.1,step=0.01),
				   		 		numericInput("var1","Sample Variance",8.23,step=0.01),
				   		 		numericInput("n1","Sample size",10,min=2), 
				   		 		hr(),
				   		 		h4("Contractor's Data"),
				   		 		numericInput("mu2","Sample Mean",4.2,step=0.01),
				   		 		numericInput("var2","Sample Variance",2.69,step=0.01),
				   		 		numericInput("n2","Sample size",4,min=2),
				   		 		hr(),
				   		 		selectInput("a1",HTML(paste("Contractor's Risk,",tags$i(HTML("&alpha;")))),selected=0.025,c(0.01,0.025,0.05,0.1)),
				   		 		submitButton(HTML(paste("Calculate State DOT's Risk,",tags$i(HTML("&beta;")))))
				   		 	),
				   		 	mainPanel(
				   		 		tabsetPanel(
				   		 			tabPanel("Summary",
				   		 					 tags$ol(
				   		 					 	h3(tags$li("Under",tags$i("F"),"test")),
				   		 					 	h4(htmlOutput("text1"),style="color:#555555"),
				   		 					 	tags$hr(),
				   		 					 	h3(tags$li("Under",tags$i("t"),"test")),
				   		 					 	h4(htmlOutput("text2"),style="color:#555555")
				   		 					 )
				   		 			),
				   		 			tabPanel(HTML(paste(tags$i("F"),"test")),
				   		 					 h4(htmlOutput("text3"),style="color:#555555")
				   		 			),
				   		 			tabPanel(HTML(paste(tags$i("t"),"test")),
				   		 					 h4(htmlOutput("text4"),style="color:#555555"),
				   		 					 h6("*The allowable difference",tags$i("d"),"is the difference between the State DOT’s and Contractor’s means divided by their pooled standard deviation.",style="color:#555555")
				   		 			),
				   		 			tabPanel("Guidance",
				   		 					 h4(
				   		 					 	tags$ul(tags$li(p("The State DOT's Risk is based on the Contractor's Risk and a selected sample size, n. A low value of", tags$i(HTML("&beta;")),"is desirable.")),
				   		 					 			tags$li(p("It is important to know the interrelationship among",tags$i(HTML("&alpha;,")),tags$i(HTML("&beta;,")),"and sample size, n. A well-written acceptance plan attempts to provide a balance between",tags$i(HTML("&alpha;")),"and",tags$i(HTML("&beta;.")),"The FHWA 
				   		 					 					suggested value for an",tags$i(HTML("&alpha;")),"risk ranges from 0.01 (1.0%) to 0.05 (5.0%)*, If a low",tags$i(HTML("&alpha;")),"value is uesed, it may be difficult to obtain a relatively low value for",tags$i(HTML("&beta;")),"unless a large sample size is used. In the event the State DOT Risk is considered too high, and the desired balance of
				   		 					 					the risks between the Contractor and the State DOT is not obtained, it is not realistic to increase the Contractor's risk,",tags$i(HTML("&alpha;,")),"to more than about 5%. If the ",tags$i(HTML("&alpha;")),"risk is limited to an upper value of 5% and the analysis indicates the
				   		 					 					State DOT's risk,",tags$i(HTML("&beta;,")),"is too high, the only alternative is to increase the sample size.")),
				   		 					 			tags$li(p("As mentioned above, and based on experience, a subjective acceptable range of Contractor's risk,",tags$i(HTML("&alpha;,")),"has been found to be 1.0% (0.01) to 5.0% (0.05). The range is even more subjective for the State DOT's risk,",tags$i(HTML("&beta;,")),"than for the Contractor's risk as many agencies have not defined \"unacceptable product\". Because of the above mentioned relationship
				   		 					 						among ",tags$i(HTML("&alpha;,")),tags$i(HTML("&beta;,")),"and sample size, n, using a fixed",tags$i(HTML("&alpha;")),"makes",tags$i(HTML("&beta;")),"directly related to n. The question becomes, how certain does the State DOT want to be that they are not verifying Contractor's data erroneously? A suggested range of",tags$i(HTML("&beta;")),"risks, based on experience, is in the table below"))
				   		 					 			),
				   		 					 	tags$table(tags$tr(
				   		 					 					tags$td("Low",tags$i(HTML("&beta;")),"risk"),tags$td("Medium",tags$i(HTML("&beta;")),"risk"),tags$td("Higher than desirable",tags$i(HTML("&beta;")),"risk")),
				   		 					 			   tags$tr(
				   		 					 			   	tags$td(h5("Less than 40% (0.40)")),tags$td(h5("50% (0.50) to 60% (0.60)")),tags$td(h5("Greater than 60% (0.60)")))
				   		 					 			   ,border="1",width="100%",height="150",style="text-align:center")
				   		 					 	)
				   		 			)
				   		 		)
				   		 	)
				   		 )),
				   tabPanel("Optimum Sample Size",
				   		 h3(p("In this menu, the optimum sample size needed for a given power of test value is calculated based on the State DOT’s and 
				   		 			  Contractor’s statistical parameters and the Contractor's Risk,",HTML(paste(tags$i(HTML("&alpha;")),".",sep="")),"The menu allows different sample sizes for the State DOT’s and Contractor’s 
				   		 			  data; this is called the “Allocation ratio”,",HTML(paste("n",tags$sub("Contr."),sep=""),"/",paste("n",tags$sub("State."),".",sep=""))
				   		 	 )),
				   		 sidebarLayout(
				   		 	sidebarPanel(
				   		 		h4("State DOT's Data"),
				   		 		numericInput("mu3","Sample Mean",13.2),
				   		 		numericInput("var3","Sample Variance",1.22),
				   		 		numericInput("n3","Sample size",10),
				   		 		hr(),
				   		 		h4("Contractor's Data"),
				   		 		numericInput("mu4","Sample Mean",14.3),
				   		 		numericInput("var4","Sample Variance",1.76),
				   		 		numericInput("n4","Sample size",4),
				   		 		hr(),
				   		 		numericInput("pwr",HTML(paste("Expected power of test (1 -",tags$i(HTML("&beta;)")))),0.9,min=0,max=1,step=0.01), 
				   		 		numericInput("ar",HTML(paste("n",tags$sub("Contr."),sep=""),"/",paste("n",tags$sub("State."),sep="")),1,step=0.1),
				   		 		selectInput("a2",HTML(paste("Contractor's Risk,",tags$i(HTML("&alpha;")))),selected=0.025,c(0.01,0.025,0.05,0.1)),
				   		 		submitButton("Calculate Optimum Sample Size")
				   		 	),
				   		 	mainPanel(
				   		 		tabsetPanel(
				   		 			tabPanel("Summary",
				   		 					 tags$ol(
				   		 					 	h3(tags$li("Under",tags$i("F") ,"test")),
				   		 					 	h4(htmlOutput("text5"),style="color:#555555"),
				   		 					 	tags$hr(),
				   		 					 	h3(tags$li("Under",tags$i("t"),"test")),
				   		 					 	h4(htmlOutput("text6"),style="color:#555555")
				   		 					 )
				   		 			),
				   		 			tabPanel(HTML(paste(tags$i("F"),"test")),
				   		 					 h4(htmlOutput("text7"),style="color:#555555")),
				   		 			tabPanel(HTML(paste(tags$i("t"),"test")),
				   		 					 h4(htmlOutput("text8"),style="color:#555555")),
				   		 			tabPanel("Guidance",
				   		 					 h4(p("If the required sample sizes are considered to be too large to be practical, there are some adjustments that can be made to the inputs."),
				   		 					 tags$ul(tags$li("The Contractor’s Risk can be increased, or the power of test value can be decreased. A third choice may be the option to delay a decision concerning verification until the desired sample sizes are reached."),
				   		 					 		tags$li("Often the",HTML(paste(tags$i("F-"),"test",sep="")),"will require larger sample sizes than the",HTML(paste(tags$i("t-"),"test",sep="")),"for validation. This is an indication that the variability between the two sets of data is statistically 
				   		 					 				different. If the results of the",HTML(paste(tags$i("t-"),"test",sep="")),"are acceptable, an agency may chose to ignore the results of the",HTML(paste(tags$i("F-"),"test.",sep="")),"If this is done, it is suggested an investigation into the 
				   		 					 				sources of variability of the two sets of data be done. One of the first steps that has found to be useful is to use the Paired",HTML(paste(tags$i("t-"),"test",sep="")),"in an attempt to find the difference.")
				   		 					 		)
				   		 					 ))
				   		 		)
				   		 	)
				   		 )),
				   tabPanel("Glossary",
				   		 h4(
				   		 		tags$p(tags$b("Risks:"),"The probability or chance of making the wrong decision in a statistical acceptance plan or test for verification. Risks are strongly influenced by the sample size, n."),
				   		 		
				   		 		tags$p(tags$b("Alpha (",tags$i(HTML("&alpha;")),") risk:"),"Also called Contractor's risk, risk of a Type I, or alpha error. Related to verification, this is the risk to the Contractor of having statistical data judged not to come from the same population as the State DOT statistical data when it actually does. This risk is often set at a range 0.01 - 0.05 suggested by FHWA."),
				   		 		
				   		 		tags$p(tags$b("Beta (",(tags$i(HTML("&beta;"))),") risk:"),"Also called State DOT's risk, risk of a Type II, or beta error. Related to verification, this is the risk to the State DOT of having statistical data judged to come from the same population as the Contractor’s statistical data from it when actually does not. This risk should be kept as low as practical."),
				   		 		tags$p(tags$b("Interrelationship among ",HTML(paste(tags$i(HTML("&alpha;")),",",sep=""),paste(tags$i(HTML("&beta;")),",",sep="")),"and sample size, n:"),"A well-written acceptance plan attempts to provide a balance between α and β. If the FHWA suggested value range of 0.01 - 0.05 is used, it may be difficult to obtain a relatively low value for β unless a large sample size is used."),
				   		 		tags$p(tags$b("Power of test ( 1-",(tags$i(HTML("&beta;"))),"):"),"This is the probability of being able to detect a difference between the Contractor’s statistical data and the State DOT’s data when one exits. It is desirable that the power of the test be as high as practical.")
				   		 	
				   		 )),
				   tabPanel("Equations",
				   		 h2("Equations",align="center"),
				   		 withMathJax(),# \\(...\\) represents inlinemath, \\[...\\] or $$...$$ represents displaymath
				   		 tags$ul(
				   		 	tags$li(h4(
				   		 		p(tags$b(tags$i("F-"),"test:")),
				   		 		p("$$F=s^2_{c}/s^2_{a}\\text{ or }s^2_{a}/s^2_{c}$$"),
				   		 		p("Where:"),
								p("\\(s^2_{c}=\\text{variance of Contractor's data}\\)",style="text-indent:50px"),
				   		 		p("\\(s^2_{a}=\\text{variance of State DOT's data}\\)",style="text-indent:50px")
								 )),
				   		 	tags$li(h4(
				   		 		p(tags$b(tags$i("t-"),"test:")),
				   		 		p("$$t=\\frac{\\left|\\bar{X}_{c}-\\bar{X}_{a} \\right|}{\\sqrt{\\frac{s^2_{p}}{n_{c}}+\\frac{s^2_{p}}{n_{a}}}}$$"),
				   		 		p("Where:"),
				   		 		p("\\(\\bar{X}_{c}=\\text{mean of Contractor's data}\\)",style="text-indent:50px"),
				   		 		p("\\(\\bar{X}_{a}=\\text{mean of State DOT's data}\\)",style="text-indent:50px"),
				   		 		p("\\(s^2_{p}=\\text{pooled variance}\\)",style="text-indent:50px"),
				   		 		p("\\(n_{c}=\\text{degrees of freedom of Contractor’s data}\\)",style="text-indent:50px"),
				   		 		p("\\(n_{a}=\\text{degrees of freedom of State DOT’s data}\\)",style="text-indent:50px")
				   		 		  )),
							tags$li(h4(
									   p(tags$b("Pooled variance:")),
									   p("$$s^2_{p}=\\frac{s^2_{c}(n_{c}-1)+s^2_{a}(n_{a}-1)}{n_{c}+n_{a}-2}$$"),
									   p("Where:"),
									   p("\\(s^2_{p}=\\text{pooled variance}\\)",style="text-indent:50px"),
									   p("\\(s^2_{c}=\\text{variance of Contractor's data}\\)",style="text-indent:50px"),
									   p("\\(s^2_{a}=\\text{variacen of State DOT's data}\\)",style="text-indent:50px"),
									   p("\\(n_{c}=\\text{degrees of freedom of Contractor's data}\\)",style="text-indent:50px"),
									   p("\\(n_{a}=\\text{degrees of freedom of State DOT's data}\\)",style="text-indent:50px")
									   )),
							tags$li(h4(
								p(tags$b("Allowable difference, d:")),
								p("$$d=\\frac{\\mu_{c}-\\mu_{a}}{s_{d}}$$"),
								p("Where:"),
								p("\\(d=\\text{allowable difference between two means}\\)",style="text-indent:50px"),
								p("\\(\\mu_{c}=\\text{mean of Contractor's data}\\)",style="text-indent:50px"),
								p("\\(\\mu_{a}=\\text{mean of State DOT's data}\\)",style="text-indent:50px"),
								p("\\(s_{d}=\\text{standard deviation of the difference}\\)",style="text-indent:50px")
							))
								)
							),
				   tabPanel("About")
				   		 )
				   )
)