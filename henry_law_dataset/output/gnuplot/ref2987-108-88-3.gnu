# load "ref2987-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2987; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1662714E-02 * exp(  -4150.539    *(1/   298.    -1/T))

set label "" at    283.0000    ,   0.3750308E-02 point
set label "" at    288.0000    ,   0.2368616E-02 point
set label "" at    293.0000    ,   0.2269924E-02 point
set label "" at    298.0000    ,   0.1677770E-02 point
set label "" at    298.1500    ,   0.1662714E-02 point ps 2 pt 6

plot [280:300] H(T)
