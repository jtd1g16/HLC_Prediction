# load "ref0477-10102-43-9.gnu"
# chem = "nitrogen monoxide"

set terminal postscript eps color
set title "ref = 477; chem = nitrogen monoxide; casrn = 10102-43-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1911231E-04 * exp(  -1437.500    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2273756E-04 point
set label "" at    293.1500    ,   0.2067846E-04 point
set label "" at    298.1500    ,   0.1899075E-04 point
set label "" at    303.1500    ,   0.1759799E-04 point
set label "" at    308.1500    ,   0.1645101E-04 point
set label "" at    298.1500    ,   0.1911231E-04 point ps 2 pt 6

plot [280:310] H(T)
