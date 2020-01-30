# load "ref2464-138-86-3.gnu"
# chem = "limonene"

set terminal postscript eps color
set title "ref = 2464; chem = limonene; casrn = 138-86-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1709165E-03 * exp(  -10135.03    *(1/   298.    -1/T))

set label "" at    279.1500    ,   0.1728261E-02 point
set label "" at    296.6500    ,   0.2029703E-03 point
set label "" at    298.1500    ,   0.1709165E-03 point ps 2 pt 6

plot [270:300] H(T)
