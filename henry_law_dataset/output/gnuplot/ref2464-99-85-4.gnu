# load "ref2464-99-85-4.gnu"
# chem = "gamma-terpinene"

set terminal postscript eps color
set title "ref = 2464; chem = gamma-terpinene; casrn = 99-85-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5402138E-03 * exp(  -7974.651    *(1/   298.    -1/T))

set label "" at    279.1500    ,   0.3335821E-02 point
set label "" at    296.6500    ,   0.6184466E-03 point
set label "" at    298.1500    ,   0.5402138E-03 point ps 2 pt 6

plot [270:300] H(T)
