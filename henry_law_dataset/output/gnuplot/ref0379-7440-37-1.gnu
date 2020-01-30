# load "ref0379-7440-37-1.gnu"
# chem = "argon"

set terminal postscript eps color
set title "ref = 379; chem = argon; casrn = 7440-37-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1369380E-04 * exp(  -1678.011    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2324869E-04 point
set label "" at    283.1500    ,   0.1818506E-04 point
set label "" at    293.1500    ,   0.1483866E-04 point
set label "" at    303.1500    ,   0.1268111E-04 point
set label "" at    298.1500    ,   0.1369380E-04 point ps 2 pt 6

plot [270:310] H(T)
