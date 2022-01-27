library(shiny)
library(shinyBS)
library(shape)

# Triunghi
tri.gen <- function(distanta){
  	
		
		len <- 20005
		
		matrice <- matrix(NA,ncol=3,nrow=3)
		
    #generare puncte rosii
		matrice[1,] <- c(1,0,0)
		matrice[2,] <- c(2,0.5,sqrt(3)/2)
		matrice[3,] <- c(3,1,0)

    # varf contine toate punctele random
    varf <- runif(len)
		varf[which(varf>2/3)]<- 3
		varf[which(1/3<varf & varf<=2/3)]<- 2
		varf[which(varf<=1/3)]<- 1

		
		coord <- matrix(NA,ncol=2,nrow=(len+1))
		colnames(coord)<-c("x","y")
		
    #generare punct random
		coord[1,] <- c(runif(1),runif(1)*sqrt(3)/2)
		
		for (i in 1:len){
		  linie <- i+1
      varf_ales <- which(matrice[,1]==varf[i])
      x <- matrice[varf_ales,2]
      y <- matrice[varf_ales,3]
      x.new <- distanta*x + (1-distanta)*coord[i,1]
      y.new <- distanta*y + (1-distanta)*coord[i,2]
	    coord[linie,]<-c(x.new,y.new)
	    x <- x.new
	    y <- y.new
		}

    return(list(matrice,varf,coord))
}

# Square
sqr.gen <- function(distanta){
  
    
		len <- 20005
		

		matrice <- matrix(NA,ncol=3,nrow=8)
		
		matrice[1,] <- c(1,0.0,0.0)
		matrice[2,] <- c(2,0.5,0.0)
		matrice[3,] <- c(3,1.0,0.0)
  	matrice[4,] <- c(4,1.0,0.5)
  	matrice[5,] <- c(5,1.0,1.0)
  	matrice[6,] <- c(6,0.5,1.0)
  	matrice[7,] <- c(7,0.0,1.0)
  	matrice[8,] <- c(8,0.0,0.5)


    varf <- runif(len)
		varf[which(varf>7/8)]<- 8
  	varf[which(6/8<varf & varf<=7/8)]<- 7
  	varf[which(5/8<varf & varf<=6/8)]<- 6
  	varf[which(4/8<varf & varf<=5/8)]<- 5
  	varf[which(3/8<varf & varf<=4/8)]<- 4
  	varf[which(2/8<varf & varf<=3/8)]<- 3
		varf[which(1/8<varf & varf<=2/8)]<- 2
		varf[which(varf<=1/8)]<- 1
		
		coord <- matrix(NA,ncol=2,nrow=(len+1))
		colnames(coord)<-c("x","y")
		

		coord[1,] <- c(runif(1),runif(1))
		
  	for (i in 1:len){
		  linie <- i+1
      varf_ales <- which(matrice[,1]==varf[i])
      x <- matrice[varf_ales,2]
      y <- matrice[varf_ales,3]
      x.new <- (distanta)*x + (1-distanta)*coord[i,1]
	    y.new <- (distanta)*y + (1-distanta)*coord[i,2]
	    coord[linie,]<-c(x.new,y.new)
		}
    
    return(list(matrice,varf,coord))
}

# Pentagon
pent.gen <- function(distanta){
  
    
  	len <- 20005
		

		matrice <- matrix(NA,ncol=3,nrow=5)
    
    c1 <- 0.25*(sqrt(5)-1)
    c2 <- 0.25*(sqrt(5)+1)
    s1 <- 0.25*(sqrt(10+2*sqrt(5)))
    s2 <- 0.25*(sqrt(10-2*sqrt(5)))
		
		matrice[1,] <- c(1,0,1)
		matrice[2,] <- c(2,s1,c1)
		matrice[3,] <- c(3,s2,-c2)
  	matrice[4,] <- c(4,-s2,-c2)
  	matrice[5,] <- c(5,-s1,c1)


    varf <- runif(len)
		varf[which(varf>4/5)]<- 5
  	varf[which(3/5<varf & varf<=4/5)]<- 4
  	varf[which(2/5<varf & varf<=3/5)]<- 3
		varf[which(1/5<varf & varf<=2/5)]<- 2
		varf[which(varf<=1/5)]<- 1
		
		coord <- matrix(NA,ncol=2,nrow=(len+1))
		colnames(coord)<-c("x","y")
		

		coord[1,] <- c(runif(1,-s1,s1),runif(1,-c2,1))
		
  	for (i in 1:len){
		  linie <- i+1
      varf_ales <- which(matrice[,1]==varf[i])
      x <- matrice[varf_ales,2]
      y <- matrice[varf_ales,3]
      x.new <- (distanta)*x + (1-distanta)*coord[i,1]
	    y.new <- (distanta)*y + (1-distanta)*coord[i,2]
	    coord[linie,]<-c(x.new,y.new)
		}
    
    return(list(matrice,varf,coord))
}

# Hexagon
hex.gen <- function(distanta){
  
    
    len <- 20005
		

		matrice <- matrix(NA,ncol=3,nrow=6)

    alpha <- 0.5
    beta  <- sqrt(3)/2
		
		matrice[1,] <- c(1,0,2*alpha)
		matrice[2,] <- c(2,beta,alpha)
		matrice[3,] <- c(3,beta,-alpha)
  	matrice[4,] <- c(4,0,-2*alpha)
  	matrice[5,] <- c(5,-beta,-alpha)
    matrice[6,] <- c(6,-beta,alpha)


    varf <- runif(len)
		varf[which(varf>5/6)]<- 6
    varf[which(4/6<varf & varf<=5/6)]<- 5
  	varf[which(3/6<varf & varf<=4/6)]<- 4
  	varf[which(2/6<varf & varf<=3/6)]<- 3
		varf[which(1/6<varf & varf<=2/6)]<- 2
		varf[which(varf<=1/6)]<- 1
		
		coord <- matrix(NA,ncol=2,nrow=(len+1))
		colnames(coord)<-c("x","y")
		
		coord[1,] <- c(runif(1,-beta,beta),runif(1,-2*alpha,2*alpha))
		
  	for (i in 1:len){
		  linie <- i+1
      varf_ales <- which(matrice[,1]==varf[i])
      x <- matrice[varf_ales,2]
      y <- matrice[varf_ales,3]
      x.new <- (distanta)*x + (1-distanta)*coord[i,1]
	    y.new <- (distanta)*y + (1-distanta)*coord[i,2]
	    coord[linie,]<-c(x.new,y.new)
		}
    
    return(list(matrice,varf,coord))
}


shinyServer(function(input, output, session) {

# puncte adaugate dupa fiecare iteratie
output$my.app <- renderUI({
  input$shape
    sliderInput(inputId = "init",
      "Numar de puncte afisate (n):",
      min = 1,
      max = 20000,
      step = input$skipped.points,
      value = 1,
      animate=animationOptions(interval = input$time.between))
  })

  #buton random
  updateButton(session, inputId = "gen", style = "primary", size = "default", disabled = FALSE)
  

  #generare 
  all.list <- reactive({
    if (input$shape == "tri") {
      return(tri.gen(input$dist.tri*(input$gen>-1)))
      }

    if (input$shape == "sqr") {
      return(sqr.gen(input$dist.sqr*(input$gen>-1)))
      }

    if (input$shape == "pent") {
      return(pent.gen(input$dist.pent*(input$gen>-1)))
      }

    if (input$shape == "hex") {
      return(hex.gen(input$dist.hex*(input$gen>-1)))
      }
    })  
  


# Chaos Game


  output$app.start <- renderPlot({
    
    matrice <- all.list()[[1]]
    varf <- all.list()[[2]]
    coord <- all.list()[[3]]

    # Triunghi 
    if (input$shape == "tri") {
      #pozitionare pucte
      par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(0,1),ylim=c(0,sqrt(3)/2),col=0, yaxt="n", xaxt="n", xlab="", ylab="", bty="n")
        
      if (!is.null(input$init))
        if (input$init==1) 
        {
          points(coord[1,1],coord[1,2],pch=20,cex=3,col="black")  
        }        
    }

    # Patrat
    if (input$shape == "sqr") {
        #pozitionare pucte
        par(mar=c(0.5,0.5,0.5,0.5))
        plot(0,0,xlim=c(0,1),ylim=c(0,1),col=0, yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
      
      if (!is.null(input$init))
        if (input$init==1) 
          points(coord[1,1],coord[1,2],pch=20,cex=3,col="black")  
    }
    
    # Pentagon
  if (input$shape == "pent") {
    
    c1 <- 0.25*(sqrt(5)-1)
    c2 <- 0.25*(sqrt(5)+1)
    s1 <- 0.25*(sqrt(10+2*sqrt(5)))
    s2 <- 0.25*(sqrt(10-2*sqrt(5)))

      par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(-s1,s1),ylim=c(-c2,1),col=0, yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    if (!is.null(input$init))
      if (input$init==1) 
        points(coord[1,1],coord[1,2],pch=20,cex=3,col="black")  
      

  }

    # Hexagon
  if (input$shape == "hex") {
    
      alpha <- 0.5
      beta  <- sqrt(3)/2

      par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(-beta,beta),ylim=c(-2*alpha,2*alpha),col=0, yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
      if (!is.null(input$init))
        if (input$init==1) 
          points(coord[1,1],coord[1,2],pch=20,cex=3,col="black")  
        

  }


  #modificare puncte
  if (!is.null(input$init))
    if (input$init != 1) {

      #puncte precedente micsorate
      points(coord[1:input$init-1,1],coord[1:input$init-1,2],pch=20,cex=1,col="black")  

      if(input$skipped.points == 1)
      {
        #punct plecare
        points(coord[input$init-1,1],coord[input$init-1,2],pch=20,cex=2.75,col="black")  
      
        #punct curent de mijloc 
        points(coord[input$init,1],coord[input$init,2],pch=20,cex=2.75,col="green")  
      }
            
      #coordonate punct
      v.x <- matrice[matrice[,1]==varf[input$init-1],2]
      v.y <- matrice[matrice[,1]==varf[input$init-1],3]


      if(input$skipped.points == 1)
      {
        #contur punct rosu ales
        points(v.x,v.y,pch=1,cex=2.5,lwd=2,col="black")
      }
    }
    
    #puncte rosii
    points(matrice[,2],matrice[,3],pch=20,cex=2,col="red")

  })

  
  
})

