# load "ref0985-78-85-3.gnu"
# chem = "2-methylpropenal"

set terminal postscript eps color
set title "ref = 985; chem = 2-methylpropenal; casrn = 78-85-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4240716E-01 * exp(  -5283.215    *(1/   298.    -1/T))

set label "" at    278.0000    ,   0.1545123     point
set label "" at    283.0000    ,   0.1089720     point
set label "" at    288.0000    ,   0.7733562E-01 point
set label "" at    293.0000    ,   0.5949071E-01 point
set label "" at    298.0000    ,   0.4248406E-01 point
set label "" at    298.1500    ,   0.4240716E-01 point ps 2 pt 6

plot [270:300] H(T)
