# load "ref2942-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2942; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1821784E-02 * exp(  -4173.989    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3792932E-02 point
set label "" at    288.1500    ,   0.3001001E-02 point
set label "" at    293.1500    ,   0.2314331E-02 point
set label "" at    298.1500    ,   0.1808550E-02 point
set label "" at    303.1500    ,   0.1448759E-02 point
set label "" at    298.1500    ,   0.1821784E-02 point ps 2 pt 6

plot [280:310] H(T)
