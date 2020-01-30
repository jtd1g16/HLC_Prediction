# load "ref0630-50-00-0.gnu"
# chem = "methanal"

set terminal postscript eps color
set title "ref = 630; chem = methanal; casrn = 50-00-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    30.83597     * exp(  -6525.526    *(1/   298.    -1/T))

set label "" at    283.1500    ,    97.70540     point
set label "" at    298.1500    ,    33.55539     point
set label "" at    303.1500    ,    19.73847     point
set label "" at    308.1500    ,    14.80385     point
set label "" at    313.1500    ,    10.85616     point
set label "" at    318.1500    ,    7.994078     point
set label "" at    298.1500    ,    30.83597     point ps 2 pt 6

plot [280:320] H(T)
