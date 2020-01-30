# load "ref2551-689-97-4.gnu"
# chem = "3-buten-1-yne"

set terminal postscript eps color
set title "ref = 2551; chem = 3-buten-1-yne; casrn = 689-97-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3789782E-03 * exp(  -1756.465    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.6472648E-03 point
set label "" at    273.1500    ,   0.6604743E-03 point
set label "" at    303.1500    ,   0.3346403E-03 point
set label "" at    303.1500    ,   0.3434466E-03 point
set label "" at    318.1500    ,   0.2597865E-03 point
set label "" at    318.1500    ,   0.2641897E-03 point
set label "" at    333.1500    ,   0.2069486E-03 point
set label "" at    298.1500    ,   0.3789782E-03 point ps 2 pt 6

plot [270:340] H(T)
