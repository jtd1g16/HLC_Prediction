# load "ref0533-928-45-0.gnu"
# chem = "1-butyl nitrate"

set terminal postscript eps color
set title "ref = 533; chem = 1-butyl nitrate; casrn = 928-45-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1002222E-01 * exp(  -6023.584    *(1/   298.    -1/T))

set label "" at    295.0000    ,   0.1243523E-01 point
set label "" at    279.4000    ,   0.3888478E-01 point
set label "" at    298.1500    ,   0.1002222E-01 point ps 2 pt 6

plot [270:300] H(T)
