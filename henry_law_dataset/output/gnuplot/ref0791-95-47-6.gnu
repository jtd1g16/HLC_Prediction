# load "ref0791-95-47-6.gnu"
# chem = "1,2-dimethylbenzene"

set terminal postscript eps color
set title "ref = 791; chem = 1,2-dimethylbenzene; casrn = 95-47-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2940187E-02 * exp(  -5439.668    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1563122E-01 point
set label "" at    286.1500    ,   0.6304684E-02 point
set label "" at    296.1500    ,   0.3330182E-02 point
set label "" at    298.1500    ,   0.2940187E-02 point ps 2 pt 6

plot [270:300] H(T)
