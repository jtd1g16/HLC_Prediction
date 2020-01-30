# load "ref2645-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2645; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1539335E-02 * exp(  -4323.477    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.4367688E-02 point
set label "" at    288.1500    ,   0.2545091E-02 point
set label "" at    298.1500    ,   0.1539677E-02 point
set label "" at    298.1500    ,   0.1539335E-02 point ps 2 pt 6

plot [270:300] H(T)
