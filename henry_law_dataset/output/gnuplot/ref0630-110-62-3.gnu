# load "ref0630-110-62-3.gnu"
# chem = "pentanal"

set terminal postscript eps color
set title "ref = 630; chem = pentanal; casrn = 110-62-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6338507E-01 * exp(  -6347.474    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.2023193     point
set label "" at    298.1500    ,   0.6415001E-01 point
set label "" at    303.1500    ,   0.4145078E-01 point
set label "" at    308.1500    ,   0.3059462E-01 point
set label "" at    318.1500    ,   0.1776462E-01 point
set label "" at    298.1500    ,   0.6338507E-01 point ps 2 pt 6

plot [280:320] H(T)
