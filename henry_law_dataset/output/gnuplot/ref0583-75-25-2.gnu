# load "ref0583-75-25-2.gnu"
# chem = "tribromomethane"

set terminal postscript eps color
set title "ref = 583; chem = tribromomethane; casrn = 75-25-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1383704E-01 * exp(  -5237.957    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.6989146E-01 point
set label "" at    283.1500    ,   0.3425529E-01 point
set label "" at    293.1500    ,   0.1890672E-01 point
set label "" at    298.1500    ,   0.1383704E-01 point ps 2 pt 6

plot [270:300] H(T)
