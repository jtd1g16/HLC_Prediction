# load "ref0630-78-93-3.gnu"
# chem = "2-butanone"

set terminal postscript eps color
set title "ref = 630; chem = 2-butanone; casrn = 78-93-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1930528     * exp(  -5043.261    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.4835924     point
set label "" at    298.1500    ,   0.1954108     point
set label "" at    303.1500    ,   0.1391562     point
set label "" at    308.1500    ,   0.1075746     point
set label "" at    318.1500    ,   0.7007155E-01 point
set label "" at    298.1500    ,   0.1930528     point ps 2 pt 6

plot [280:320] H(T)
