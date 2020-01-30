# load "ref2453-108-67-8.gnu"
# chem = "1,3,5-trimethylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = 1,3,5-trimethylbenzene; casrn = 108-67-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1126620E-02 * exp(  -4706.588    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1948659E-02 point
set label "" at    298.1500    ,   0.1122554E-02 point
set label "" at    308.1500    ,   0.6798760E-03 point
set label "" at    318.1500    ,   0.4161046E-03 point
set label "" at    298.1500    ,   0.1126620E-02 point ps 2 pt 6

plot [280:320] H(T)
