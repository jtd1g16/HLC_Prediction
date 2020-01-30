# load "ref0791-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 791; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1570098E-02 * exp(  -4514.842    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.6296521E-02 point
set label "" at    286.1500    ,   0.2942186E-02 point
set label "" at    296.1500    ,   0.1746315E-02 point
set label "" at    298.1500    ,   0.1570098E-02 point ps 2 pt 6

plot [270:300] H(T)
