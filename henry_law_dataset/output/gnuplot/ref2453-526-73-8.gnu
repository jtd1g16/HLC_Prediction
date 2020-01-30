# load "ref2453-526-73-8.gnu"
# chem = "1,2,3-trimethylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = 1,2,3-trimethylbenzene; casrn = 526-73-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2359416E-02 * exp(  -4507.254    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.4129993E-02 point
set label "" at    298.1500    ,   0.2258853E-02 point
set label "" at    308.1500    ,   0.1411783E-02 point
set label "" at    318.1500    ,   0.9411889E-03 point
set label "" at    298.1500    ,   0.2359416E-02 point ps 2 pt 6

plot [280:320] H(T)
