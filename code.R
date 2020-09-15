#Collaborators: Zoe, Michelle 
#Collaborators: Zoe, Michelle 

#1. 

library(plotrix)
#The magnitude of the determinant is the area of the image of the unit square.
#Start by creating a matrix M.
v1 <- c(2,1); v2 <- c(2,3); M <-cbind(v1,v2); M; det(M) #determinant of M is 4
alpha <-acos((v2%*%v1)/(sqrt(v2%*%v2)*sqrt(v1%*%v1)))
#Set up the screen with narrow margins and aspect ratio 1:1
pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,5), ylim = c(-1,5), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

arrows(0,0, 1, 0, col = "green")   #first standard basis vector
arrows(0,0, v1[1], v1[2], col = "blue") #after applying M
text(2, .5, expression(v1), cex=1)
arrows(0,0, 0, 1, col = "green")   #second standard basis vector
arrows(0,0, v2[1], v2[2], col = "red") #after applying M
text(1.8, 3.1, expression(v2), cex=1)
arrows(v1[1], v1[2], v1[1]+v2[1], v1[2]+v2[2], col = "blue")
arrows(v2[1], v2[2], v1[1]+v2[1], v1[2]+v2[2], col = "red")
draw.arc(-.2,.3,.8,angle2=alpha)
text(0.30,0.3,expression('14.2°'),cex=1)
arrows(1,0, 1, 1, col = "green")  
arrows(0,1, 1, 1, col = "green") 
text(5, 5, expression('positive angle = positive determinant'), cex=1)
text(5, 4, expression('determinant = 4'), cex=1)


v_1 <- c(2,3); v_2 <- c(2,1); M <-cbind(v_1,v_2); M; det(M) #determinant of M is 4
alpha <- acos((v_2%*%v_1)/(sqrt(v_2%*%v_2)*sqrt(v_1%*%v_1)))
#Set up the screen with narrow margins and aspect ratio 1:1


pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,5), ylim = c(-1,5), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

arrows(0,0, 1, 0, col = "green")   #first standard basis vector
arrows(0,0, v_1[1], v_1[2], col = "blue") #after applying M
text(1.8, 3.1, expression(v_1), cex=1)
arrows(0,0, 0, 1, col = "green")   #second standard basis vector
arrows(0,0, v_2[1], v_2[2], col = "red") #after applying M
text(2, .5, expression(v2), cex=1)
arrows(v_1[1], v_1[2], v_1[1]+v_2[1], v_1[2]+v_2[2], col = "blue")
arrows(v_2[1], v_2[2], v_1[1]+v_2[1], v_1[2]+v_2[2], col = "red")
draw.arc(-.2,.3,.8,angle2=alpha)
text(0.30,0.3,expression('-14.2°'),cex=1)
arrows(1,0, 1, 1, col = "green")
arrows(0,1, 1, 1, col = "green")
text(5, 5, expression('negative angle = negative determinant'), cex=1)
text(5, 4, expression('determinant = -4'), cex=1)
angle(v_1, v_2)



v01 <- c(2,1); v02 <- c(2,1); M <-cbind(v01,v02); M; det(M) #determinant of M is 4
alpha <-acos((v02%*%v01)/(sqrt(v02%*%v02)*sqrt(v01%*%v01)))
#Set up the screen with narrow margins and aspect ratio 1:1
pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,5), ylim = c(-1,5), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

arrows(0,0, 1, 0, col = "green")   #first standard basis vector
arrows(0,0, v01[1], v01[2], col = "blue") #after applying M
arrows(0,0, 0, 1, col = "green")   #second standard basis vector
arrows(0,0, v02[1], v02[2], col = "red") #after applying M
arrows(v01[1], v01[2], v01[1]+v02[1], v01[2]+v02[2], col = "blue")
arrows(v02[1], v02[2], v01[1]+v02[1], v01[2]+v02[2], col = "red")
text(2, .7, expression(v01), cex=1)
text(2, 1.2, expression(v02), cex=1)
draw.arc(-.2,.3,.8,angle2=alpha)
text(0.30,0.3,expression('0°'),cex=1)
arrows(1,0, 1, 1, col = "green")
arrows(0,1, 1, 1, col = "green")
text(5, 5, expression('0° angle = 0 determinant'), cex=1)
text(5, 4, expression('determinant = 0'), cex=1)



#Problem 2
plot(NULL, xlim = c(-3,3), ylim = c(-3,3), xlab = "", ylab= "",axes = FALSE)
I1 <- c(-2,2); I2 <- c(-2,-2); I3 <- c(2,2); I4 <- c(2,-2)
points(c(I1[1],I2[1],I3[1],I4[1]),c(I1[2],I2[2],I3[2],I4[2]))
text(I1[1]-0.2,I1[2]+0.2,"The Middle Kingdom"); text(I2[1]-0.2,I2[2]-0.2,"Tibet");
text(I3[1]+0.2,I3[2]+0.2,"Shanghai"); text(I4[1]+0.2,I4[2]-0.2,"Hunan");
#From island 1 there are trains to islands 2, 3, 4
arrows(I1[1],I1[2],I2[1],I2[2]); arrows(I1[1],I1[2],I3[1],I3[2]); arrows(I1[1],I1[2],I4[1],I4[2])
c1 <- c(0,1,1,1)
#From island 2 there are trains to island 1 and 3 , but not to 4.
arrows(I2[1],I2[2],I1[1],I1[2]);arrows(I2[1],I2[2],I3[1],I3[2])
c2 <- c(1,0,1,0)
#From island 3 there are trains to islands 1 and 4, but not to 2
arrows(I3[1],I3[2],I1[1],I1[2]); arrows(I3[1],I3[2],I4[1],I4[2])
c3 <- c(1,0,0,1)
#From island 4 there are boats to island 1 and 2, but not to 3
arrows(I4[1],I4[2],I1[1],I1[2]);arrows(I4[1],I4[2],I2[1],I2[2])
c4 <- c(1,1,0,0)

#Here is the "transition matrix" for this graph.
A <- cbind(c1,c2,c3,c4); A

B <- A %*% A; B  

C <- B %*% B; C

#There are 7 4-step monorails from Tibet to the Middle Kingdom
