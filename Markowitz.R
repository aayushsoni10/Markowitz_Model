dat = read.csv("CSV.csv", header = TRUE)
dat2 = read.csv("^NSEI.csv", header = TRUE)
dat3 = read.csv("DATA.csv", header = TRUE)
dat4 = read.csv("^DJI.csv", header = TRUE)
dat5 = read.csv("^GSPC.csv", header = TRUE)
dat6 = read.csv("^IXIC.csv", header = TRUE)
dat7 = read.csv("^N225.csv", header = TRUE)
dat8 = read.csv("^RUT.csv", header = TRUE)
dat9 = read.csv("^TNX.csv", header = TRUE)
dat10 = read.csv("^VIX.csv", header = TRUE)
dat[,5] = as.numeric(dat[,5])
dat2[,5] = as.numeric(dat2[,5])
dat3[,5] = as.numeric(dat3[,5])
dat4[,5] = as.numeric(dat4[,5])
dat5[,5] = as.numeric(dat5[,5])
dat6[,5] = as.numeric(dat6[,5])
dat7[,5] = as.numeric(dat7[,5])
dat8[,5] = as.numeric(dat8[,5])
dat9[,5] = as.numeric(dat9[,5])
dat10[,5] = as.numeric(dat10[,5])
tmp = -0.02
p_ret <- c()
for(t in 1:50){
  p_ret<- c(p_ret, tmp)
  tmp=tmp+0.001
}
par(mfrow = c(2,4))
pa<-0
for( x in 6:1){
  s = 2 + (x-1)*20
  e = 1091 + (x-1)*20
  r1 = ((dat[s:e, 5]- dat[(s-1):(e-1), 5])/dat[(s-1):(e-1), 5])
  av1 = ((dat[s:e, 5]+ dat[(s-1):(e-1), 5])/2)
  r_1 = sum(r1)/1090
  r1 = ((dat2[s:e, 5]- dat2[(s-1):(e-1), 5])/dat2[(s-1):(e-1), 5])
  av2 = ((dat2[s:e, 5]+ dat2[(s-1):(e-1), 5])/2)
  r_2 = sum(r1)/1090
  r1 = ((dat3[s:e, 5]- dat3[(s-1):(e-1), 5])/dat3[(s-1):(e-1), 5])
  av3 = ((dat3[s:e, 5]+ dat3[(s-1):(e-1), 5])/2)
  r_3 = sum(r1)/1090
  r1 = ((dat4[s:e, 5]- dat4[(s-1):(e-1), 5])/dat4[(s-1):(e-1), 5])
  av4 = ((dat4[s:e, 5]+ dat4[(s-1):(e-1), 5])/2)
  r_4 = sum(r1)/1090
  r1 = ((dat5[s:e, 5]- dat5[(s-1):(e-1), 5])/dat5[(s-1):(e-1), 5])
  av5 = ((dat5[s:e, 5]+ dat5[(s-1):(e-1), 5])/2)
  r_5 = sum(r1)/1090
  r1 = ((dat6[s:e, 5]- dat6[(s-1):(e-1), 5])/dat6[(s-1):(e-1), 5])
  av6 = ((dat6[s:e, 5]+ dat6[(s-1):(e-1), 5])/2)
  r_6 = sum(r1)/1090
  r1 = ((dat7[s:e, 5]- dat7[(s-1):(e-1), 5])/dat7[(s-1):(e-1), 5])
  av7 = ((dat7[s:e, 5]+ dat7[(s-1):(e-1), 5])/2)
  r_7 = sum(r1)/1090
  r1 = ((dat8[s:e, 5]- dat8[(s-1):(e-1), 5])/dat8[(s-1):(e-1), 5])
  av8 = ((dat8[s:e, 5]+ dat8[(s-1):(e-1), 5])/2)
  r_8 = sum(r1)/1090
  r1 = ((dat9[s:e, 5]- dat9[(s-1):(e-1), 5])/dat9[(s-1):(e-1), 5])
  av9 = ((dat9[s:e, 5]+ dat9[(s-1):(e-1), 5])/2)
  r_9 = sum(r1)/1090
  r1 = ((dat10[s:e, 5]- dat10[(s-1):(e-1), 5])/dat10[(s-1):(e-1), 5])
  av10 = ((dat10[s:e, 5]+ dat10[(s-1):(e-1), 5])/2)
  r_10 = sum(r1)/1090
  
  vec1 <- c(cov(av1, av1), cov(av1, av2), cov(av1, av3), cov(av1, av4), cov(av1, av5), cov(av1, av6), cov(av1, av7), cov(av1, av8), cov(av1, av9), cov(av1, av10))
  vec2<-c(cov(av2, av1), cov(av2, av2), cov(av2, av3), cov(av2, av4), cov(av2, av5), cov(av2, av6), cov(av2, av7), cov(av2, av8), cov(av2, av9), cov(av2, av10))
  vec3<-c(cov(av3, av1), cov(av3, av2), cov(av3, av3), cov(av3, av4), cov(av3, av5), cov(av3, av6), cov(av3, av7), cov(av3, av8), cov(av3, av9), cov(av3, av10))
  vec4<-c(cov(av4, av1), cov(av4, av2), cov(av4, av3), cov(av4, av4), cov(av4, av5), cov(av4, av6), cov(av4, av7), cov(av4, av8), cov(av4, av9), cov(av4, av10))
  vec5<-c(cov(av5, av1), cov(av5, av2), cov(av5, av3), cov(av5, av4), cov(av5, av5), cov(av5, av6), cov(av5, av7), cov(av5, av8), cov(av5, av9), cov(av5, av10))
  vec6<-c(cov(av6, av1), cov(av6, av2), cov(av6, av3), cov(av6, av4), cov(av6, av5), cov(av6, av6), cov(av6, av7), cov(av6, av8), cov(av6, av9), cov(av6, av10))
  vec7<-c(cov(av7, av1), cov(av7, av2), cov(av7, av3), cov(av7, av4), cov(av7, av5), cov(av7, av6), cov(av7, av7), cov(av7, av8), cov(av7, av9), cov(av7, av10))
  vec8<-c(cov(av8, av1), cov(av8, av2), cov(av8, av3), cov(av8, av4), cov(av8, av5), cov(av8, av6), cov(av8, av7), cov(av8, av8), cov(av8, av9), cov(av8, av10))
  vec9<-c(cov(av9, av1), cov(av9, av2), cov(av9, av3), cov(av9, av4), cov(av9, av5), cov(av9, av6), cov(av9, av7), cov(av9, av8), cov(av9, av9), cov(av9, av10))
  vec10<-c(cov(av10, av1), cov(av10, av2), cov(av10, av3), cov(av10, av4), cov(av10, av5), cov(av10, av6), cov(av10, av7), cov(av10, av8), cov(av10, av9), cov(av10, av10))
  cov_m<-array(c(vec1,vec2,vec3,vec4,vec5,vec6,vec7,vec8,vec9,vec10), dim=c(10,10))
  coff_1<-c(cov_m[1,1], cov_m[1,2], cov_m[1,3], cov_m[1,4], cov_m[1,5],
            cov_m[1,6], cov_m[1,7], cov_m[1,8], cov_m[1,9], cov_m[1,10],
            (-1*r_1), -1)
  coff_2<-c(cov_m[2,1], cov_m[2,2], cov_m[2,3], cov_m[2,4], cov_m[2,5],
            cov_m[2,6], cov_m[2,7], cov_m[2,8], cov_m[2,9], cov_m[2,10],
            (-1*r_2), -1)
  coff_3<-c(cov_m[3,1], cov_m[3,2], cov_m[3,3], cov_m[3,4], cov_m[3,5],
            cov_m[3,6], cov_m[3,7], cov_m[3,8], cov_m[3,9], cov_m[3,10],
            (-1*r_3), -1)
  coff_4<-c(cov_m[4,1], cov_m[4,2], cov_m[4,3], cov_m[4,4], cov_m[4,5],
            cov_m[4,6], cov_m[4,7], cov_m[4,8], cov_m[4,9], cov_m[4,10],
            (-1*r_4), -1)
  coff_5<-c(cov_m[5,1], cov_m[5,2], cov_m[5,3], cov_m[5,4], cov_m[5,5],
            cov_m[5,6], cov_m[5,7], cov_m[5,8], cov_m[5,9], cov_m[5,10],
            (-1*r_5), -1)
  coff_6<-c(cov_m[6,1], cov_m[6,2], cov_m[6,3], cov_m[6,4], cov_m[6,5],
            cov_m[6,6], cov_m[6,7], cov_m[6,8], cov_m[6,9], cov_m[6,10],
            (-1*r_6), -1)
  coff_7<-c(cov_m[7,1], cov_m[7,2], cov_m[7,3], cov_m[7,4], cov_m[7,5],
            cov_m[7,6], cov_m[7,7], cov_m[7,8], cov_m[7,9], cov_m[7,10],
            (-1*r_7), -1)
  coff_8<-c(cov_m[8,1], cov_m[8,2], cov_m[8,3], cov_m[8,4], cov_m[8,5],
            cov_m[8,6], cov_m[8,7], cov_m[8,8], cov_m[8,9], cov_m[8,10],
            (-1*r_8), -1)
  coff_9<-c(cov_m[9,1], cov_m[9,2], cov_m[9,3], cov_m[9,4], cov_m[9,5],
            cov_m[9,6], cov_m[9,7], cov_m[9,8], cov_m[9,9], cov_m[9,10],
            (-1*r_9), -1)
  coff_10<-c(cov_m[10,1], cov_m[10,2], cov_m[10,3], cov_m[10,4], cov_m[10,5],
             cov_m[10,6], cov_m[10,7], cov_m[10,8], cov_m[10,9], cov_m[10,10],
             (-1*r_10), -1)
  coff_11<-c(r_1, r_2, r_3,r_4,r_5,r_6,r_7,r_8,r_9,r_10,0,0)
  coff_12<-c(1,1,1,1,1,1,1,1,1,1,0,0)
  A<-matrix(c(coff_1,coff_2,coff_3,coff_4,coff_5
              ,coff_6,coff_7,coff_8,coff_9,coff_10,
              coff_11,coff_12), 12, 12)
  arr <- array(dim=c(length(p_ret),2))
  i=1
  min_l <- 100
  min_sd <- 10000
  for (l in p_ret) {
    arr[i,1] = l
    b<-c(0,0,0,0,0,0,0,0,0,0,l,1)
    W = solve(A,b)
    arr[i,2] = sqrt((W[1]*W[1] + W[2]*W[2] + W[3]*W[3]  + W[4]*W[4]  + W[5]*W[5]  + W[6]*W[6] + W[7]*W[7] + W[8]*W[8] + W[9]*W[9] + W[10]*W[10]))
    i = i+1
    if(min_sd>arr[i-1, 2]){
      min_sd = arr[i-1, 2]
      min_l = l
    }
  }
  b<-c(0,0,0,0,0,0,0,0,0,0,min_l,1)
  W = solve(A,b)
  tmp = sqrt((W[1]*W[1] + W[2]*W[2] + W[3]*W[3]  + W[4]*W[4]  + W[5]*W[5]  + W[6]*W[6] + W[7]*W[7] + W[8]*W[8] + W[9]*W[9] + W[10]*W[10]))
  to = e+20
  a_r<-c(0,0,0,0)
  while(to!=e){
    f = e+5
    f_r_1 = ((dat[f:f, 5] - dat[e:e, 5])/dat[e:e, 5])
    f_r_2 = ((dat2[f:f, 5] - dat2[e:e, 5])/dat2[e:e, 5])
    f_r_3 = ((dat3[f:f, 5] - dat3[e:e, 5])/dat3[e:e, 5])
    f_r_4 = ((dat4[f:f, 5] - dat4[e:e, 5])/dat4[e:e, 5])
    f_r_5 = ((dat5[f:f, 5] - dat5[e:e, 5])/dat5[e:e, 5])
    f_r_6 = ((dat6[f:f, 5] - dat6[e:e, 5])/dat6[e:e, 5])
    f_r_7 = ((dat7[f:f, 5] - dat7[e:e, 5])/dat7[e:e, 5])
    f_r_8 = ((dat8[f:f, 5] - dat8[e:e, 5])/dat8[e:e, 5])
    f_r_9 = ((dat9[f:f, 5] - dat9[e:e, 5])/dat9[e:e, 5])
    f_r_10 = ((dat10[f:f, 5] - dat10[e:e, 5])/dat10[e:e, 5])
    a_r[((e-to-20)/5) +1] = W[1]*f_r_1 + W[2]*f_r_2 + W[3]*f_r_3 + W[4]*f_r_4 + 
      W[5]*f_r_5 + W[6]*f_r_6 + W[7]*f_r_7 + W[8]*f_r_8 + 
      W[9]*f_r_9 + W[10]*f_r_10
    e=e+5
  }
  if(x>5){
    plot(arr[,2], arr[,1], type = "s", col="blue", xlab = paste(as.character(x), " ", "Rate :", format(round(min_l, 4), nsmall = 1), "Min dev :", format(round(tmp, 3), nsmall = 1), "RW1: ", format(round(a_r[1], 5), nsmall = 1), "\n", "RW2: ", format(round(a_r[2], 5), nsmall = 1),"RW3: ", format(round(a_r[3], 5), nsmall = 1), "RW4: ", format(round(a_r[4], 5), nsmall = 1)), ylab = paste("r", as.character(x)))
  }else{
    plot(arr[,2], arr[,1], type = "s", col="blue", xlab = paste(as.character(x), " ", "Rate :", format(round(min_l, 4), nsmall = 1), "Min dev :", format(round(tmp, 3), nsmall = 1), "RW1: ", format(round(a_r[1], 5), nsmall = 1), "\n", "RW2: ", format(round(a_r[2], 5), nsmall = 1),"RW3: ", format(round(a_r[3], 5), nsmall = 1), "RW4: ", format(round(a_r[4], 5), nsmall = 1)), ylab = paste("r", as.character(x)),
      lines(arr[,2],pa, col="red"))
  }
  if(a_r[1]==a_r[2] && a_r[2]==a_r[3] && a_r[3]==a_r[4])
    print("ALL SAME")
  pa<- arr[,1]
}

