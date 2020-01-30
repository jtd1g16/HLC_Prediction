# load "ref2464-80-56-8.gnu"
# chem = "$\alpha$-pinene"

set terminal postscript eps color
set title "ref = 2464; chem = $\alpha$-pinene; casrn = 80-56-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2838110E-04 * exp(  -10022.95    *(1/   298.    -1/T))

set label "" at    279.1500    ,   0.2797320E-03 point
set label "" at    296.6500    ,   0.3363971E-04 point
set label "" at    298.1500    ,   0.2838110E-04 point ps 2 pt 6

plot [270:300] H(T)
