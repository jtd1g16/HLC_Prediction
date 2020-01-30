# load "ref0759-7726-95-6.gnu"
# chem = "molecular bromine"

set terminal postscript eps color
set title "ref = 759; chem = molecular bromine; casrn = 7726-95-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1803089E-01 * exp(  -3570.806    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3123276E-01 point
set label "" at    298.1500    ,   0.1825319E-01 point
set label "" at    318.1500    ,   0.8419525E-02 point
set label "" at    278.1500    ,   0.4504178E-01 point
set label "" at    298.1500    ,   0.1858965E-01 point
set label "" at    303.1500    ,   0.1474878E-01 point
set label "" at    298.1500    ,   0.1803089E-01 point ps 2 pt 6

plot [270:320] H(T)
