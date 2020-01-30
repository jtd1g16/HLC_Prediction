# load "ref0858-78-93-3.gnu"
# chem = "2-butanone"

set terminal postscript eps color
set title "ref = 858; chem = 2-butanone; casrn = 78-93-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1407347     * exp(  -4683.365    *(1/   298.    -1/T))

set label "" at    318.1600    ,   0.5470018E-01 point
set label "" at    333.1600    ,   0.2498153E-01 point
set label "" at    343.1600    ,   0.1790977E-01 point
set label "" at    353.1600    ,   0.1263480E-01 point
set label "" at    298.1500    ,   0.1407347     point ps 2 pt 6

plot [310:360] H(T)
