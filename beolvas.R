## Időzóna beállítása
Sys.setenv(TZ="UTC")
## Hőmérséklet beolvasás függvénye
beolvas <- function(file) {
  require(xts)
  raw <- read.table(file, colClasses = "character")
  #str(raw)
  idő <- strptime(raw$V2,"%Y%m%d%H%M%S")

  t1=strsplit(raw$V3, "/")
  t2=unlist(t1)
  t3=matrix(t2,nrow = nrow(raw), byrow = T)
  hőm <- as.numeric(t3[,2])
  hőm[hőm< -500] <- NA
  xts(hőm, idő)
}

## Beolvasás teszt
teszt <- beolvas("20150801.TXT")
plot(teszt)

## Fájl nevek beolvasása
fl = dir(pattern = "TXT")
fl = fl[-(5:25)]

## Hőmérséklet beolvasása
hőm.xts <- beolvas("20150101.TXT")
for(i in fl){
  cat(i,"\n")
  hőm.xts <- c(hőm.xts, beolvas(i))
}
plot(hőm.xts)

## Augusztus
plot(hőm.xts['2015-08'])
aug = hőm.xts['2015-08']
plot(aug)

## Dobozábrák
## teljes augusztus
boxplot(coredata(aug))
## napok (kétféleképpen)
boxplot(coredata(aug) ~ rep(1:31,each=144))
boxplot(coredata(aug) ~ format(index(aug),"%d"))
## Órás boxplotok a teljes hónapra
boxplot(coredata(aug) ~ format(index(aug),"%H"))

## Napi átlagok
apply.daily(aug,mean)
plot(apply.daily(aug,mean))

## Napi átlagok havi dobozábrái
napi=apply.daily(hőm.xts,mean)
boxplot(coredata(napi) ~ format(index(napi),"%m"))

plot(aug)
plot(aug['2015-08-05'])

## Augusztus néhány nap napi görbéi
kn=aug['2015-08-05']
plot(data.frame(index(kn),as.numeric(coredata(kn))),typ="l", ylim=c(14,33))
an=aug['2015-08-06']
lines(data.frame(index(kn),as.numeric(coredata(an))))
for(i in 7:12){
  an=aug[paste0('2015-08-',i)]
  lines(data.frame(index(kn),as.numeric(coredata(an))))
}

plot(aug, xaxs="i")

t.test(aug['2015-08-05'],aug['2015-08-06'])
kn=aug['2015-08-05']
an=aug['2015-08-06']
t.test(kn,an)

aug2n=aug['2015-08-05/2015-08-06']
boxplot(coredata(aug2n) ~ format(index(aug2n),"%d"))
t.test(coredata(aug2n) ~ format(index(aug2n),"%d"))

aug2n.df = data.frame(hőm=as.numeric(coredata(aug2n)),
           nap=as.factor(format(index(aug2n),"%d")))

t.test(hőm ~ nap, data = aug2n.df)
boxplot(hőm ~ nap, data = aug2n.df)

t.test(hőm ~ nap, data = aug2n.df, alt="l")

t.test(hőm ~ nap, data = aug2n.df, pair=T)
var.test(hőm ~ nap, data = aug2n.df)

wilcox.test(hőm ~ nap, data = aug2n.df)

