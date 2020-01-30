# load "ref2453-103-65-1.gnu"
# chem = "propylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = propylbenzene; casrn = 103-65-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9497386E-03 * exp(  -4718.412    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1677028E-02 point
set label "" at    298.1500    ,   0.9379985E-03 point
set label "" at    308.1500    ,   0.5479397E-03 point
set label "" at    318.1500    ,   0.3617118E-03 point
set label "" at    298.1500    ,   0.9497386E-03 point ps 2 pt 6

plot [280:320] H(T)
