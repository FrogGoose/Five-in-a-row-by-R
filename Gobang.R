judge<-function(l){#loop by hand
  bp<-l[nrow(l),]
  x<-bp[1]
  y<-bp[2]
  #vertical and horizontal
  k1<-0
  k2<-0
  if(any(apply(l,1,FUN=function(b) identical(c(x,y+1),b)))){
    k1<-k1+1
    if(k1==1 && any(apply(l,1,FUN=function(b) identical(c(x,y+2),b)))){
      k1<-k1+1
      if(k1==2 && any(apply(l,1,FUN=function(b) identical(c(x,y+3),b)))){
        k1<-k1+1
        if(k1==3 && any(apply(l,1,FUN=function(b) identical(c(x,y+4),b)))){
          k1<-k1+1
        }
      }
    }
  }
  if(any(apply(l,1,FUN=function(b) identical(c(x,y-1),b)))){
    k2<-k2+1
    if(k2==1 && any(apply(l,1,FUN=function(b) identical(c(x,y-2),b)))){
      k2<-k2+1
      if(k2==2 && any(apply(l,1,FUN=function(b) identical(c(x,y-3),b)))){
        k2<-k2+1
        if(k2==3 && any(apply(l,1,FUN=function(b) identical(c(x,y-4),b)))){
          k2<-k2+1
        }
      }
    }
  }
  if((k1+k2)>=4){#vertical direction
    return ("Win")
  }else{
    k1<-0
    k2<-0
    if(any(apply(l,1,FUN=function(b) identical(c(x+1,y),b)))){
      k1<-k1+1
      if(k1==1 && any(apply(l,1,FUN=function(b) identical(c(x+2,y),b)))){
        k1<-k1+1
        if(k1==2 && any(apply(l,1,FUN=function(b) identical(c(x+3,y),b)))){
          k1<-k1+1
          if(k1==3 && any(apply(l,1,FUN=function(b) identical(c(x+4,y),b)))){
            k1<-k1+1
          }
        }
      }
    }
    if(any(apply(l,1,FUN=function(b) identical(c(x-1,y),b)))){
      k2<-k2+1
      if(k2==1 && any(apply(l,1,FUN=function(b) identical(c(x-2,y),b)))){
        k2<-k2+1
        if(k2==2 && any(apply(l,1,FUN=function(b) identical(c(x-3,y),b)))){
          k2<-k2+1
          if(k2==3 && any(apply(l,1,FUN=function(b) identical(c(x-4,y),b)))){
            k2<-k2+1
          }
        }
      }
    }
    if((k1+k2)>=4){#horizontal direction
      return ("Win")
    }
  }
  t1<-0
  t2<-0
  if(any(apply(l,1,FUN=function(b) identical(c(x+1,y+1),b)))){
    t1<-t1+1
    if(t1==1 && any(apply(l,1,FUN=function(b) identical(c(x+2,y+2),b)))){
      t1<-t1+1
      if(t1==2 && any(apply(l,1,FUN=function(b) identical(c(x+3,y+3),b)))){
        t1<-t1+1
        if(t1==3 && any(apply(l,1,FUN=function(b) identical(c(x+4,y+4),b)))){
          t1<-t1+1
        }
      }
    }
  }
  if(any(apply(l,1,FUN=function(b) identical(c(x-1,y-1),b)))){
    t2<-t2+1
    if(t2==1 && any(apply(l,1,FUN=function(b) identical(c(x-2,y-2),b)))){
      t2<-t2+1
      if(t2==2 && any(apply(l,1,FUN=function(b) identical(c(x-3,y-3),b)))){
        t2<-t2+1
        if(t2==3 && any(apply(l,1,FUN=function(b) identical(c(x-4,y-4),b)))){
          t2<-t2+1
        }
      }
    }
  }
  if((t1+t2)>=4){#off diagnoal direction
    return ("Win")
  }else{
    t1<-0
    t2<-0
    if(any(apply(l,1,FUN=function(b) identical(c(x+1,y-1),b)))){
      t1<-t1+1
      if(t1==1 && any(apply(l,1,FUN=function(b) identical(c(x+2,y-2),b)))){
        t1<-t1+1
        if(t1==2 && any(apply(l,1,FUN=function(b) identical(c(x+3,y-3),b)))){
          t1<-t1+1
          if(t1==3 && any(apply(l,1,FUN=function(b) identical(c(x+4,y-4),b)))){
            t1<-t1+1
          }
        }
      }
    }
    if(any(apply(l,1,FUN=function(b) identical(c(x-1,y+1),b)))){
      t2<-t2+1
      if(t2==1 && any(apply(l,1,FUN=function(b) identical(c(x-2,y+2),b)))){
        t2<-t2+1
        if(t2==2 && any(apply(l,1,FUN=function(b) identical(c(x-3,y+3),b)))){
          t2<-t2+1
          if(t2==3 && any(apply(l,1,FUN=function(b) identical(c(x-4,y+4),b)))){
            t2<-t2+1
          }
        }
      }
    }
    if((t1+t2)>=4){#diagnoal direction
      return ("Win")
    }
  }
  if(t1+t2<4 && k1+k2<4)
    return(0)
}

wzq <- function(n = 13) {
  if (!interactive()) return()
  #source('~/R/Machine Learning-R/five in row/judge.R')
  par(mar = rep(0, 4),bg="bisque")
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  segments(1, 1:n, n, 1:n)
  segments(1:n, 1, 1:n, n)
  points(rep(c(4, 10, 16), 3), rep(c(4, 10, 16), each = 3),
         pch = 19, cex = 1.2)
  box(lwd=5)
  playedlist <- NULL
  #####parameters related to who will win#########start
  bl0<-matrix(,nr=1,nc=2)
  wl0<-matrix(,nr=1,nc=2)
  bl<-NULL
  wl<-NULL
  win<-NULL
  #####parameters related to who will win#########end
  i <- 0
  repeat {
    for (j in 1:2) {
      repeat {
        l <- locator(1)
        l$x <- min(n, max(1, round(l$x)))
        l$y <- min(n, max(1, round(l$y)))
        xy <- paste(l, collapse = ":")
        if (!is.element(xy, playedlist)) #can not locate again on one spot
          break
      }
      playedlist <- c(playedlist, xy)
      ###########judge who will win##############start
      if(j==1L){
        bl0[,1]<-l$x
        bl0[,2]<-l$y
        bl<-rbind(bl,bl0)
      }
      else{
        wl0[,1]<-l$x
        wl0[,2]<-l$y
        wl<-rbind(wl,wl0)
      }
      if(!is.null(bl)){# judge who wins
        if(judge(bl)=="Win"){
          win<-"black win"
          break
        }
      }
      if(!is.null(wl)){
        if(judge(wl)=="Win"){
          win<-"white win"
          break
        }
      }
      ###########judge ends#############end
      points(l, cex = 3, pch = c(19, 21)[j], bg = c("black", "white")[j])
      i <- i + 1
      if (i >= n^2) break
    }
    if (i >= n^2) break
    #########who will win########start  
    if(!is.null(win)){
      if(win=="black win"){
        xx<-bl[nrow(bl),1]
        yy<-bl[nrow(bl),2]
        ys<-"black"
      }else{
        xx<-wl[nrow(wl),1]
        yy<-wl[nrow(wl),2]
        ys<-"white" 
      }
      points(xx,yy,pch=21,cex=3,bg=ys)
      break
    }
  }
  return(win)
  ############ judge ends#########end
}

