# load "ref2905-106-42-3.gnu"
# chem = "1,4-dimethylbenzene"

set terminal postscript eps color
set title "ref = 2905; chem = 1,4-dimethylbenzene; casrn = 106-42-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9846814E-03 * exp(  -3228.492    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1389468E-02 point
set label "" at    298.1500    ,   0.1050862E-02 point
set label "" at    308.1500    ,   0.6700168E-03 point
set label "" at    298.1500    ,   0.9846814E-03 point ps 2 pt 6

plot [280:310] H(T)
