# load "ref2936-292-64-8.gnu"
# chem = "cyclooctane"

set terminal postscript eps color
set title "ref = 2936; chem = cyclooctane; casrn = 292-64-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7127416E-04 * exp(  -4978.999    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3522529E-03 point
set label "" at    278.1500    ,   0.2402228E-03 point
set label "" at    283.1500    ,   0.1685578E-03 point
set label "" at    288.1500    ,   0.1216895E-03 point
set label "" at    293.1500    ,   0.9017051E-04 point
set label "" at    298.1500    ,   0.6860467E-04 point
set label "" at    303.1500    ,   0.5311139E-04 point
set label "" at    308.1500    ,   0.4242441E-04 point
set label "" at    313.1500    ,   0.3429220E-04 point
set label "" at    298.1500    ,   0.7127416E-04 point ps 2 pt 6

plot [270:320] H(T)
