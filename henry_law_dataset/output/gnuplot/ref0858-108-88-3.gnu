# load "ref0858-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 858; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1547880E-02 * exp(  -2472.213    *(1/   298.    -1/T))

set label "" at    318.1600    ,   0.8959187E-03 point
set label "" at    333.1600    ,   0.6714689E-03 point
set label "" at    343.1600    ,   0.5327369E-03 point
set label "" at    353.1600    ,   0.4120783E-03 point
set label "" at    298.1500    ,   0.1547880E-02 point ps 2 pt 6

plot [310:360] H(T)
