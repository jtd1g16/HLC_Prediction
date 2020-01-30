# load "ref0088-12033-49-7.gnu"
# chem = "nitrogen trioxide"

set terminal postscript eps color
set title "ref = 88; chem = nitrogen trioxide; casrn = 12033-49-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1180485     * exp(  -1915.107    *(1/   298.    -1/T))

set label "" at    288.0000    ,   0.1480385     point
set label "" at    298.0000    ,   0.1184308     point
set label "" at    298.1500    ,   0.1180485     point ps 2 pt 6

plot [280:300] H(T)
