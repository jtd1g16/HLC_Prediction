# load "ref2834-10102-43-9.gnu"
# chem = "nitrogen monoxide"

set terminal postscript eps color
set title "ref = 2834; chem = nitrogen monoxide; casrn = 10102-43-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1940286E-04 * exp(  -1582.550    *(1/   298.    -1/T))

set label "" at    273.2200    ,   0.3249533E-04 point
set label "" at    273.2200    ,   0.3247772E-04 point
set label "" at    273.2200    ,   0.3234122E-04 point
set label "" at    283.1700    ,   0.2515086E-04 point
set label "" at    283.2000    ,   0.2512444E-04 point
set label "" at    283.1400    ,   0.2509802E-04 point
set label "" at    293.0800    ,   0.2071688E-04 point
set label "" at    293.1700    ,   0.2071688E-04 point
set label "" at    293.2700    ,   0.2070807E-04 point
set label "" at    303.1700    ,   0.1760824E-04 point
set label "" at    303.1400    ,   0.1766108E-04 point
set label "" at    303.1700    ,   0.1760824E-04 point
set label "" at    313.0600    ,   0.1545069E-04 point
set label "" at    313.1000    ,   0.1549473E-04 point
set label "" at    313.1700    ,   0.1540666E-04 point
set label "" at    298.1500    ,   0.1940286E-04 point ps 2 pt 6

plot [270:320] H(T)
