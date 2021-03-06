#Fungsi Bunga
# A: Nilai Akumulasi , k: modal , t: tahun, m:
bunga=function(num,A,k,t,m=TRUE)
{
  switch(num, 
         satu = {
           bunga.tunggal = ((A/k)-1)/t
           cat("Nilai bunga tunggal:",bunga.tunggal)
         },
         dua = {
           majemuk.nominal = ((A/k)^(1/(m*t))-1)*m
           cat("Nilai bunga majemuk nominal:",majemuk.nominal)
         },
         tiga = {
           majemuk.kontinu = (log(A/k))/t
           cat("Nilai bunga majemuk kontinu:",majemuk.kontinu)
         }
  )
}
# bunga("satu",3500,2000,4)
# bunga("dua",3500,2000,4)
# bunga("dua",3500,2000,4,2)
# bunga("tiga",3500,2000,4)