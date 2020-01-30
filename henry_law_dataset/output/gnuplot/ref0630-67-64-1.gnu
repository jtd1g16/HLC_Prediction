# load "ref0630-67-64-1.gnu"
# chem = "propanone"

set terminal postscript eps color
set title "ref = 630; chem = propanone; casrn = 67-64-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3458900     * exp(  -3843.620    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.7036763     point
set label "" at    298.1500    ,   0.3414755     point
set label "" at    303.1500    ,   0.2733777     point
set label "" at    308.1500    ,   0.2181100     point
set label "" at    318.1500    ,   0.1618554     point
set label "" at    298.1500    ,   0.3458900     point ps 2 pt 6

plot [280:320] H(T)
