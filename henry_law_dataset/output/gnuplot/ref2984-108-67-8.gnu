# load "ref2984-108-67-8.gnu"
# chem = "1,3,5-trimethylbenzene"

set terminal postscript eps color
set title "ref = 2984; chem = 1,3,5-trimethylbenzene; casrn = 108-67-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1073462E-02 * exp(  -4579.785    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1820458E-02 point
set label "" at    298.1500    ,   0.1072518E-02 point
set label "" at    308.1500    ,   0.6635721E-03 point
set label "" at    318.1500    ,   0.4039555E-03 point
set label "" at    298.1500    ,   0.1073462E-02 point ps 2 pt 6

plot [280:320] H(T)
