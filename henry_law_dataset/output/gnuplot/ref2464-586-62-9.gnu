# load "ref2464-586-62-9.gnu"
# chem = "terpinolene"

set terminal postscript eps color
set title "ref = 2464; chem = terpinolene; casrn = 586-62-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5704772E-03 * exp(  -12319.18    *(1/   298.    -1/T))

set label "" at    279.1500    ,   0.9497487E-02 point
set label "" at    296.6500    ,   0.7030303E-03 point
set label "" at    298.1500    ,   0.5704772E-03 point ps 2 pt 6

plot [270:300] H(T)
