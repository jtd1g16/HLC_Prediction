# load "ref0791-106-42-3.gnu"
# chem = "1,4-dimethylbenzene"

set terminal postscript eps color
set title "ref = 791; chem = 1,4-dimethylbenzene; casrn = 106-42-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2285137E-02 * exp(  -5354.980    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1188854E-01 point
set label "" at    286.1500    ,   0.4791560E-02 point
set label "" at    296.1500    ,   0.2599166E-02 point
set label "" at    298.1500    ,   0.2285137E-02 point ps 2 pt 6

plot [270:300] H(T)
