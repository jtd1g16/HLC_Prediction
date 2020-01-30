# load "ref2982-108-86-1.gnu"
# chem = "bromobenzene"

set terminal postscript eps color
set title "ref = 2982; chem = bromobenzene; casrn = 108-86-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3940459E-02 * exp(  -2907.606    *(1/   298.    -1/T))

set label "" at    303.1500    ,   0.3480623E-02 point
set label "" at    323.1500    ,   0.1713372E-02 point
set label "" at    343.1500    ,   0.1143428E-02 point
set label "" at    298.1500    ,   0.3940459E-02 point ps 2 pt 6

plot [300:350] H(T)
