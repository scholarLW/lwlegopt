Box_pos=function(x_n=4,y_n=4,line_length=0.9,jap=0.1,alpha=60,beta=30,data2=NULL,type=FALSE,top=FALSE){
  data=array(NA,dim=c(2*x_n,2*y_n,2))
  k1=tan(-pi*alpha/180)
  k2=tan(pi*beta/180)
  if(isTRUE(type)){
   if(!is.null(data2)){
     dimm1=dim(data2)
     if(isTRUE(top)){
       pos_start=data2[16,24,]
       for(i in 1:(2*x_n)){
         if(i%%2==1){
           sum=0.6+(i-1)/2*line_length+(i-1)/2*jap
           data[i,2*y_n,]=c(pos_start[1]+sum*cos(pi*alpha/180),pos_start[2]-sum*sin(pi*alpha/180))
         }else{
           sum1=0.6+i/2*line_length+(i/2-1)*jap
           data[i,2*y_n,]=c(pos_start[1]+sum1*cos(pi*alpha/180),pos_start[2]-sum1*sin(pi*alpha/180))        
         }
       }      
       data3=array(NA,dim=c(1,2*y_n,2))
       pos_start1=data2[1,24,]
       for(i in 1:(2*y_n)){
         if(i%%2==1){
           sum=(i-1)/2*(line_length+jap)
           data3[1,2*y_n-i+1,]=c(pos_start1[1]-sum*cos(pi*beta/180),pos_start1[2]-sum*sin(pi*beta/180))
         }else{
           sum1=i/2*line_length+(i/2-1)*jap
           data3[1,2*y_n-i+1,]=c(pos_start1[1]-sum1*cos(pi*beta/180),pos_start1[2]-sum1*sin(pi*beta/180))        
         }
       } 
       for(j in (2*y_n-1):1){    
         data[1,j,]=posfun(data[1,2*y_n,],data3[1,j,],k1,k2) 
       }          
       for(j in (2*y_n-1):1){
         for(i in 2:(2*x_n)){
           data[i,j,]=posfun(data[i,j+1,],data[i-1,j,],k1,k2) 
         }
       }
     }else{   
      pos_start=data2[16,1,] 
      for(i in 1:(2*x_n)){
       if(i%%2==1){
#        sum=pos_start[1]/cos(pi*alpha/180)+0.6+(i-1)/2*line_length+(i+1)/2*jap
        sum=pos_start[1]/cos(pi*alpha/180)+0.6+(i-1)/2*line_length+(i-1)/2*jap
        data[i,1,]=c(sum*cos(pi*alpha/180),-sum*sin(pi*alpha/180))
       }else{
#        sum1=pos_start[1]/cos(pi*alpha/180)+0.6+i/2*line_length+i/2*jap
        sum1=pos_start[1]/cos(pi*alpha/180)+0.6+i/2*line_length+(i/2-1)*jap
        data[i,1,]=c(sum1*cos(pi*alpha/180),-sum1*sin(pi*alpha/180))        
       }
     }
     data3=array(NA,dim=c(1,2*y_n,2))
     for(i in 1:(2*y_n)){
      if(i%%2==1){
        sum=(i-1)/2*(line_length+jap)
        data3[1,i,]=c(sum*cos(pi*beta/180),sum*sin(pi*beta/180))
      }else{
        sum1=i/2*line_length+(i/2-1)*jap
        data3[1,i,]=c(sum1*cos(pi*beta/180),sum1*sin(pi*beta/180))        
      }
     } 
     for(j in 2:(2*y_n)){    
        data[1,j,]=posfun(data[1,1,],data3[1,j,],k1,k2) 
     }          
     for(j in 2:(2*y_n)){
      for(i in 2:(2*x_n)){
        data[i,j,]=posfun(data[i,j-1,],data[i-1,j,],k1,k2) 
      }
     } 
    }
   }else{
     for(i in 1:(2*x_n)){
       if(i%%2==1){
         sum=(i-1)/2*(line_length+jap)
         data[i,1,]=c(sum*cos(pi*alpha/180),-sum*sin(pi*alpha/180))
       }else{
         sum1=i/2*line_length+(i/2-1)*jap
         data[i,1,]=c(sum1*cos(pi*alpha/180),-sum1*sin(pi*alpha/180))        
       }
     }  
     for(i in 1:(2*y_n)){
       if(i%%2==1){
         sum=(i-1)/2*(line_length+jap)
         data[1,i,]=c(sum*cos(pi*beta/180),sum*sin(pi*beta/180))
       }else{
         sum1=i/2*line_length+(i/2-1)*jap
         data[1,i,]=c(sum1*cos(pi*beta/180),sum1*sin(pi*beta/180))        
       }
     } 
     for(j in 2:(2*y_n)){
       for(i in 2:(2*x_n)){
         data[i,j,]=posfun(data[i,j-1,],data[i-1,j,],k1,k2) 
       }
     }
   }      
  }else{
     for(i in 1:(2*x_n)){
       if(i%%2==1){
         sum=(i-1)/2*(line_length+jap)
         data[i,1,]=c(sum*cos(pi*alpha/180),-sum*sin(pi*alpha/180))
       }else{
         sum1=i/2*line_length+(i/2-1)*jap
         data[i,1,]=c(sum1*cos(pi*alpha/180),-sum1*sin(pi*alpha/180))        
       }
     }  
     for(i in 1:(2*y_n)){
       if(i%%2==1){
         sum=(i-1)/2*(line_length+jap)
         data[1,i,]=c(sum*cos(pi*beta/180),sum*sin(pi*beta/180))
       }else{
         sum1=i/2*line_length+(i/2-1)*jap
         data[1,i,]=c(sum1*cos(pi*beta/180),sum1*sin(pi*beta/180))        
       }
     } 
    for(j in 2:(2*y_n)){
         for(i in 2:(2*x_n)){
           data[i,j,]=posfun(data[i,j-1,],data[i-1,j,],k1,k2) 
         }
    }
  }
  return(data)
}
posfun=function(p1,p2,k1,k2){
  kpos=NULL
  x0=(p2[2]-p1[2]+k2*p1[1]-k1*p2[1])/(k2-k1)
  y0=p2[2]-k1*p2[1]+k1*x0
  kpos=c(x0,y0)
# cat("kpos",kpos,"\n")
  return(kpos)
}
posfun2=function(p1,length,k,axis=NULL){
  if(!is.null(axis)){
    pos=NULL
    y0=p1[2]
    x0=p1[1]
    b=y0-k*x0
    A=1+k^2
    B=2*k*b-2*x0-2*y0*k
    C=x0^2+b^2-2*y0*b+y0^2-length^2
    if(is.na(A)|!is.finite(A)|is.null(A)){
      A=1
    }
    if(is.na(B)|!is.finite(B)|is.null(B)){
      B=0
    }  
    if(is.na(C)|!is.finite(C)|is.null(C)){
      C=0
    }  
    if(B^2-4*A*C<0){
      stop("Equations have no solution!")
    }
    ksqrt=sqrt(B^2-4*A*C)
    x1=c((-B-ksqrt)/(2*A),(-B+ksqrt)/(2*A))
    if(axis=='x'){
      ix=x1<=x0
      pos[1]=x1[ix]
    }
    if(axis=='y'){
      ix=x1>=x0
      pos[1]=x1[ix]     
    }
    if(axis=='z'){
      ix=x1<=x0
      pos[1]=x1[ix]     
    }  
    pos[2]=k*pos[1]+b
    return(pos)
  }
}
pos_get=function(A=NULL,B=NULL,C=NULL,D=NULL,z_high,theta){
  if(!is.null(A)){A1=c(A[1]-z_high*tan(pi*theta/180),A[2]+z_high)}else{A1=NULL}
  if(!is.null(B)){B1=c(B[1]-z_high*tan(pi*theta/180),B[2]+z_high)}else{B1=NULL}
  if(!is.null(C)){C1=c(C[1]-z_high*tan(pi*theta/180),C[2]+z_high)}else{C1=NULL}
  if(!is.null(D)){D1=c(D[1]-z_high*tan(pi*theta/180),D[2]+z_high)}else{D1=NULL}
  return(list('A1'=A1,'B1'=B1,'C1'=C1,'D1'=D1))
}
Box_plot=function(data=NULL,z_high=NULL,col=NULL,type=FALSE,method=NULL,theta=0,border=border){
  if(!is.null(data)){
    dimm=dim(data)
    if(!is.null(z_high)){ 
      num=seq(1,7,by=2)
      num2=seq(9,15,by=2)
      num1=seq(23,1,by=-2)
      if(is.null(method)){
        polyplot(num1,num,data,z_high,col,theta,border=border) 
        polyplot(num1,num2,data,z_high,col,theta,border=border)        
      }else{
         polyplot(num1,num,data,z_high,col,method,theta,border=border) 
         polyplot(num1,num2,data,z_high,col,method,theta,border=border)
      }
    }else{
      if(!isTRUE(type)){
         num=seq(1,15,by=2)
         num1=seq(1,23,by=2)
         for(j in num1){
           for(i in num){
             A=data[i,j,]
             B=data[i+1,j,]
             D=data[i,j+1,]
             C=data[i+1,j+1,]
             segments(A[1],A[2],B[1],B[2])
             segments(A[1],A[2],D[1],D[2])
             segments(C[1],C[2],B[1],B[2])
             segments(C[1],C[2],D[1],D[2])
           }
         } 
      }else{
        num=c(1,3,5,7)
        for(j in num){
          for(i in num){
            A=data[i,j,]
            B=data[i+1,j,]
            D=data[i,j+1,]
            C=data[i+1,j+1,]
            pos_p=pos_get(A,B,C,D,z_high=0.3,theta)
            A1=pos_p$A1
            B1=pos_p$B1
            C1=pos_p$C1
            D1=pos_p$D1          
            segments(A[1],A[2],B[1],B[2])
            segments(C[1],C[2],B[1],B[2])
            segments(A[1],A[2],A1[1],A1[2])
            segments(B[1],B[2],B1[1],B1[2])
            segments(C[1],C[2],C1[1],C1[2])
            segments(A1[1],A1[2],D1[1],D1[2])
            segments(A1[1],A1[2],B1[1],B1[2])
            segments(B1[1],B1[2],C1[1],C1[2])
            segments(C1[1],C1[2],D1[1],D1[2])
          }
        } 
      }
    }
  }
}
polyplot=function(num1,num,data,z_high,color,method="polygon",theta,border='grep80'){
  if(all(num>8)){
    k=44
  }else{
    k=0
  }
  for(j in num1){
    for(i in num){
      A=data[i,j,]
      D=data[i+1,j,]
      B=data[i,j+1,]
      C=data[i+1,j+1,]
      pos_p=pos_get(A,B,C,D,z_high[k+2*(23-j)+(i+1)/2],theta)
      A1=pos_p$A1
      B1=pos_p$B1
      C1=pos_p$C1
      D1=pos_p$D1
      m=k+2*(23-j)+(i+1)/2      
 #     m=floor(m/17)+1
      if(m < 17){
        m=1
      }else if(m < 33){
        m=2
      }else if(m < 49){
        m=3
      }else if(m < 65){
        m=4
      }else if(m < 81){
        m=5
      }else{
        m=6
      }      
      if(method=="polygon"){
        polygon_color(A,A1,D1,D,color[m])
        polygon_color(A1,B1,C1,D1,col=color[m])
        polygon_color(D,D1,C1,C,col=color[m])    
      }else{
         fitcolor(A,A1,D1,D,color[m])
         fitcolor(A1,B1,C1,D1,col=color[m])
         fitcolor(D,D1,C1,C,col=color[m])
      }
      segments(A[1],A[2],D[1],D[2],col=border,lwd=0.1)
      segments(C[1],C[2],D[1],D[2],col=border,lwd=0.1)
      segments(A[1],A[2],A1[1],A1[2],col=border,lwd=0.1)
      segments(D[1],D[2],D1[1],D1[2],col=border,lwd=0.1)
      segments(C[1],C[2],C1[1],C1[2],col=border,lwd=0.1)
      segments(A1[1],A1[2],D1[1],D1[2],col=border,lwd=0.1)
      segments(A1[1],A1[2],B1[1],B1[2],col=border,lwd=0.1)
      segments(B1[1],B1[2],C1[1],C1[2],col=border,lwd=0.1)
      segments(C1[1],C1[2],D1[1],D1[2],col=border,lwd=0.1)
    }
  }
}

polygon_color=function(A=NULL,B=NULL,C=NULL,D=NULL,col=NULL){
  if(!is.null(col)){
    x=c(A[1],B[1],C[1],D[1])
    y=c(A[2],B[2],C[2],D[2])
    polygon(x,y,col=col,border=NULL)
  }
}
fitcolor=function(A=NULL,B=NULL,C=NULL,D=NULL,col=NULL){
  if(!is.null(col)){
    x=c(A[1],B[1],C[1],D[1])
    y=c(A[2],B[2],C[2],D[2])
    x_lim=seq(min(x),max(x),by=0.01)
    y_lim=seq(min(y),max(y),by=0.01)
    for(i in x_lim){
      for(j in y_lim){
         P=c(i,j)
         ###for A,B ###
         d1=cal_d(P,P1=A,P2=B)
         ###for B,C ###
         d2=cal_d(P,P1=B,P2=C)
         ###for C,D ###
         d3=cal_d(P,P1=C,P2=D)
         ###for D,A ###
         d4=cal_d(P,P1=D,P2=A)
         ## A,B  C,D ##
         dd1=cal_line_d(A,B,C,D)
         ## B,C  A,D ##
         dd2=cal_line_d(B,C,D,A)
         if(d1+d3<=dd1 && d2+d4<=dd2){
            points(i,j,col=col,pch=16,cex=0.01)
         }
      } 
   }
  }
}
cal_d=function(P,P1=NULL,P2=NULL){
  if(is.null(P1) || is.null(P2)){
    d=0
  }else{
    A=P2[2]-P1[2]
    B=P1[1]-P2[1]
    C=(P2[1]-P1[1])*P1[2]-P1[1]*(P2[2]-P1[2])
    d=abs(A*P[1]+B*P[2]+C)/sqrt(A^2+B^2)
  } 
  return(d)
}
cal_line_d=function(P1=NULL,P2=NULL,P3=NULL,P4=NULL){
  if(is.null(P1) || is.null(P3)){
    d=0
  }else{
    if(P2[1]-P1[1]==0 || P3[1]-P4[1]==0){
       d=abs(P4[1]-P1[1])
    }else{
       k=(P2[2]-P1[2])/(P2[1]-P1[1])
       b1=P1[2]-k*P1[1]
       b2=P3[2]-k*P3[1]
       d=abs(b1-b2)/sqrt(1+k^2)
    }
  } 
  return(d)
}

Draw_lego=function(h=NULL,path=NULL,file_name=NULL,title=NULL,prepare=FALSE,samtools=NULL,fasta=NULL,sort=NULL,top=TRUE,kge=3,color=NULL,border=NULL){
name=NULL
if(is.null(h)){
   try(load(paste(getwd(),"/Mutdata.Rdata",sep="")),silent=TRUE)
   if(is.null(h)){
	if(is.null(path)){
	  path=getwd()
	}
	if(is.null(border)){
	  border='grey40'
	}
	if(is.null(file_name)){
	 stop("You must provide the name of file you want to plot!")
	}
	cat("Working directory",path,"\n")
	file=paste(path,"/",file_name,sep="")
	if(isTRUE(prepare)){
	        cat("Starting perl job ...\n")
		if(is.null(samtools)){
			stop("You must provide the software: samtools!")
		}
		if(is.null(fasta)){
			stop("You must provide the genome!")
		}
		if(is.null(title)){
			title='cancer'
		}
      		packagepath = system.file(package="lwlegopt")
		#if .perl folder exists, delete it.
		test = getwd()
		try( setwd(".perl"),silent=TRUE)
		if(getwd() != test)
		{
			setwd("..")
			system("rm -r .perl")
		}
		#copy perl folder
		system(paste("cp -r ",packagepath,"/perl .perl",sep=""))
		# new temporary file that holds position.
        	#cat("Package directory",packagepath,"\n")
		system(paste("perl .perl/Prepare.pl",file,samtools,fasta,paste(packagepath,"/data/96_mutation_types",sep=""),path,title,sep =" "))	
        	cat("Finishing the perl job!\n")
        	file=paste(path,"/",title,".originalGenomes_with_types.txt",sep="")
	}
	h=read.table(file,header=F,sep="\t")
	save(h,file=paste(path,"/","Mutdata.Rdata",sep=""))
	name = strsplit(tolower(file), ".originalgenomes_with_types.txt")
	name = strsplit(name[[1]], "/")
	name = name[[1]][length(name[[1]])]
        if(strsplit(file, "/")[[1]][length(strsplit(file, "/")[[1]])]==name){
          name='cancer'
        }      
   }
}
if(is.null(name)){
  name='cancer'
}
if(is.null(title)){
  title_name=name
}else{
  title_name=title
}
Mut96type=as.character(h$V1)
Mut6type=as.character(h$V2)
if(is.null(color)){
  color=c('#9D2C28','#877329','#F4A018','#668188','#104B84','#8CB68C')
}
if(isTRUE(sort)){
  h1=NULL
  h2=NULL
  h1=rbind(h1,data.frame(Mut96type,Mut6type,apply(h[,3:ncol(h)],1,sum)/sum(apply(h[,3:ncol(h)],1,sum))))
  colnames(h1)=c("M96","M6","High")
  Mut6type1=c("C>T","C>A","C>G","T>C","T>G","T>A")
  lable1=NULL
  lable=NULL
  for(i in 1:6){
    h3=NULL
    ix=h1$M6==Mut6type1[i]
    h3=h1[ix,]
    if(i==1){
      kh3=h3[order(h3$High,decreasing = T),]
      colnames(kh3)=c("M96","M6","High")
      Mtype=as.character(kh3$M96)
      for(ij in 1:16){
        lable1[ij]=paste(strsplit(Mtype[ij], "")[[1]][1],"_",strsplit(Mtype[ij], "")[[1]][3],sep="")
      }
      lable=c(lable1[13],lable1[9],lable1[5],lable1[1],lable1[14],lable1[10],lable1[6],lable1[2],lable1[15],lable1[11],lable1[7],lable1[3],lable1[16],lable1[12],lable1[8],lable1[4])
    }
    for(j in 1:16){
      st=sub("_",strsplit(Mut6type1[i],">")[[1]][1],lable1[j])
      ix=grepl(st,h3$M96)
      h2=rbind(h2,h3[ix,])
    }
  }
  h1=h2
  Mut6type=Mut6type1  
  name1=paste(name,".sort",sep="")
}else if(is.null(sort)){
  h1=NULL
  h2=NULL
  h1=rbind(h1,data.frame(Mut96type,Mut6type,apply(h[,3:ncol(h)],1,sum)/sum(apply(h[,3:ncol(h)],1,sum))))
  colnames(h1)=c("M96","M6","High")
  Mut6type1=c("C>T","C>A","C>G","T>C","T>G","T>A")
  lable1=c("T_G","C_G","A_G","G_G","T_A","C_A","A_A","G_A","T_C","C_C","A_C","G_C","T_T","C_T","A_T","G_T")
  lable=c(lable1[13],lable1[9],lable1[5],lable1[1],lable1[14],lable1[10],lable1[6],lable1[2],lable1[15],lable1[11],lable1[7],lable1[3],lable1[16],lable1[12],lable1[8],lable1[4])
  for(i in 1:6){
    h3=NULL
    ix=h1$M6==Mut6type1[i]
    h3=h1[ix,]
    for(j in 1:16){
      st=sub("_",strsplit(Mut6type1[i],">")[[1]][1],lable1[j])
      ix=grepl(st,h3$M96)
      h2=rbind(h2,h3[ix,])
    }
  }
  h1=h2
  Mut6type=Mut6type1  
  name1=paste(name,".Nonsort",sep="")
}else{
  h1=NULL
  h1=rbind(h1,data.frame(Mut96type,Mut6type,apply(h[,3:ncol(h)],1,sum)/sum(apply(h[,3:ncol(h)],1,sum))))
  colnames(h1)=c("M96","M6","High")
  Mut6type=unique(Mut6type)
  lable=c("T_A","G_A","C_A","A_A","T_C","G_C","C_C","A_C","T_G","G_G","C_G","A_G","T_T","G_T","C_T","A_T")
  name1=name
}
data_6type=NULL
for(i in Mut6type){
  tem=NULL
  ix=h$V2==i
  tem=h[ix,]
  data_6type[i]=round(sum(apply(tem[,3:ncol(h)],1,sum)))
}
z_high=h1$High*100
lable_max=(max(z_high)+kge-max(z_high)%%kge)/kge
y_high1=lable_max*kge
y_high=10
kge1=kge*y_high/y_high1
z_high=y_high*z_high/y_high1
##########
data=NULL
theta=0
alpha=83
beta=83
kge2=4
jap=0.3
line_length1=0.7
line_length2=1.3
data=Box_pos(8,12,line_length=line_length1,jap=jap,alpha=alpha,beta=beta,type=FALSE)
data1=Box_pos(4,4,line_length=line_length2,jap=jap,alpha=alpha,beta=beta,data2=data,type=TRUE,top=top)
x1=max(apply(data1[,,1],1,max))
x1=max(x1,max(apply(data[,,1],1,max)))
x2=min(apply(data[,,1],1,min))
y2=min(apply(data1[,,2],1,min))
data_mid=Box_pos(8,12,line_length=line_length1/2,jap=jap+line_length1/2,alpha=alpha,beta=beta,type=FALSE,top=TRUE)
data1_mid=Box_pos(4,4,line_length=line_length2/2,jap=jap+line_length2/2,alpha=alpha,beta=beta,data2=data,type=TRUE,top=top)
pos=pos_get(data[1,1,],data[1,24,],data[2,24,],data[2,1,],y_high+kge2,theta)
y_max=pos$B1[2]
pdf(file=paste(getwd(),"/",name1,".lego.pdf",sep=""),width=15,height=8)
layout(matrix(data=c(1,2), nrow=2, ncol=1), widths=c(20), heights=c(6,3))
par(mar = c(2,1,2,1))
plot(0,0,type='l',xlim=c(-ceiling(x2)-1,ceiling(x1)+1),ylim=c(y2,y_max),axes=F,xlab='',ylab='')
text(x2,y_max,title_name,font=2,cex=1.8)
text(x2+0.55,y_max-8.5,paste("n=",sum(data_6type),sep=""),font=1,cex=1.2)
segments(data[1,1,1],data[1,1,2],data[1,24,1],data[1,24,2],lwd=2)
segments(data[1,1,1],data[1,1,2],data[16,1,1],data[16,1,2],lwd=2)
segments(data[16,1,1],data[16,1,2],data[16,24,1],data[16,24,2],lwd=2)
segments(data[1,24,1],data[1,24,2],data[16,24,1],data[16,24,2],lwd=2)
num_x=c(1,seq(2,24,by=2),25)
num_y=seq(1,lable_max+1,by=1)
data_ge1=array(NA,dim=c(length(num_x),length(num_y),2))
for(i in 1:num_y[length(num_y)]){
  for(j in num_x){
    if(i==1){
      if(j==1){
        data_ge1[1,1,]=data_mid[1,j,]
      }else if(j==25){
        data_ge1[length(num_x),1,]=data[1,j-1,]
      }else{
        data_ge1[j/2+1,1,]=data_mid[1,j,]
      }
    }else{
      if(j==1){
        kk=pos_get(A=data_ge1[1,i-1,],z_high=kge1,theta=theta)
        data_ge1[1,i,]=kk$A1        
      }else if(j==25){
        kk=pos_get(A=data_ge1[length(num_x),i-1,],z_high=kge1,theta=theta) 
        data_ge1[length(num_x),i,]=kk$A1
      }else{
        kk=pos_get(A=data_ge1[j/2+1,i-1,],z_high=kge1,theta=theta) 
        data_ge1[j/2+1,i,]=kk$A1
      }         
    }
  }
}
for(i in 1:13){
  if(i==1){
    lty=1
    lwd=2
  }else{
    lty=2
    lwd=1
  }
  segments(data_ge1[i,1,1],data_ge1[i,1,2],data_ge1[i,length(num_y),1],data_ge1[i,length(num_y),2],lwd=lwd,lty=lty,col="grey65")
}
for(i in 1:1:length(num_y)){
  segments(data_ge1[1,i,1],data_ge1[1,i,2],data_ge1[14,i,1],data_ge1[14,i,2],lwd=1,lty=2,col="grey65")
}

k1=tan(-pi*alpha/180)
k2=tan(pi*beta/180)
segments(data_ge1[1,1,1],data_ge1[1,1,2],data_ge1[1,length(num_y),1],data_ge1[1,length(num_y),2],lty=1,lwd=2)
for(i in 1:length(num_y)){
  pos_z=posfun2(data_ge1[1,i,],length=0.2,k=k1,axis='z')
  segments(pos_z[1],pos_z[2],data_ge1[1,i,1],data_ge1[1,i,2],lty=1,lwd=2)
  text(pos_z[1]-0.1,pos_z[2],paste((i-1)*kge,"%",sep=""),font=2,cex=1.2)
}
mtext("Proportions",side=2,line=0.1,adj=0.38,padj=14.5,font=2,cex=1.3)
for(i in seq(2,16,by=2)){
  pos_z=posfun2(data_mid[i,1,],length=0.2,k=k2,axis='x')
  segments(pos_z[1],pos_z[2],data_mid[i,1,1],data_mid[i,1,2],lty=1,lwd=2)
}
for(i in seq(2,24,by=2)){
  pos_z=posfun2(data_mid[16,i,],length=0.5,k=k1,axis='y')
  segments(pos_z[1],pos_z[2],data_mid[16,i,1],data_mid[16,i,2],lty=1,lwd=2)
}
num_x=c(1,seq(2,16,by=2),17)
num_y=seq(1,lable_max+1,by=1)
data_ge1=array(NA,dim=c(length(num_x),length(num_y),2))
for(i in 1:num_y[length(num_y)]){
  for(j in num_x){
    if(i==1){
      if(j==1){
        data_ge1[1,1,]=data[1,24,]
      }else if(j==17){
        data_ge1[length(num_x),1,]=data[16,24,]
      }else{
        data_ge1[j/2+1,1,]=posfun(data_mid[j,24,],data_ge1[j/2,1,],k1,k2) 
      }
    }else{
      if(j==1){
        kk=pos_get(A=data_ge1[1,i-1,],z_high=kge1,theta=theta)
        data_ge1[1,i,]=kk$A1        
      }else if(j==17){
        kk=pos_get(A=data_ge1[length(num_x),i-1,],z_high=kge1,theta=theta) 
        data_ge1[length(num_x),i,]=kk$A1
      }else{
        kk=pos_get(A=data_ge1[j/2+1,i-1,],z_high=kge1,theta=theta) 
        data_ge1[j/2+1,i,]=kk$A1
      }         
    }
  }
}
for(i in 2:9){
  if(i==1){
    lty=1
    lwd=2
  }else{
    lty=2
    lwd=1
  }
  segments(data_ge1[i,1,1],data_ge1[i,1,2],data_ge1[i,length(num_y),1],data_ge1[i,length(num_y),2],lwd=lwd,lty=lty,col="grey65")
}
for(i in 1:length(num_y)){
  segments(data_ge1[1,i,1],data_ge1[1,i,2],data_ge1[10,i,1],data_ge1[10,i,2],lwd=1,lty=2,col="grey65")
}

Box_plot(data1,z_high=NULL,col=NULL,type=T,method=NULL,theta=theta,border=border)
Box_plot(data,z_high=z_high,col=color,type=F,method="polygon",theta=theta,border=border)
dim_text=dim(data1_mid)[1:2]
for(i in seq(2,dim_text[1],by=2)){
  if(isTRUE(top)){
    num_top=seq(1,dim_text[1]-1,by=2)
  }else{
    num_top=seq(2,dim_text[1],by=2)
  }
  for(j in num_top){
    KK=pos_get(C=data1_mid[i,j,],z_high=0.2,theta=theta)
    if(isTRUE(top)){
      t=2*(i-2)+(j+1)/2
    }else{
      t=2*(i-2)+j/2
    }
    text(KK$C1[1],KK$C1[2],lable[t],cex=1.1,font=1)
  }
}
x_0=max(apply(data[,,1],1,max))
y_0=y_max-5
geg=1.9
for(i in 1:6){
  points(x_0+0.05,y_0-i*geg-0.5,pch=22,cex=3,col=color[i],bg=color[i])
  text(x_0+0.12,y_0-i*geg-0.5,labels=Mut6type[i],font=2,cex=1.2,adj=0,srt=0)
}

### Draw the pie
########## 1 #####################################################################
par(mar = c(2,2,2,2))
total <-sum(data_6type)
value <-c(data_6type[1]/total,data_6type[2]/total,data_6type[3]/total,data_6type[4]/total,data_6type[5]/total,data_6type[6]/total)
cols <- color
pie(value,col=cols,labels=NA)
dev.off()
}
