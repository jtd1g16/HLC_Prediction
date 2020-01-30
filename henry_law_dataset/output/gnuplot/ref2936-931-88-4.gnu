# load "ref2936-931-88-4.gnu"
# chem = "cyclooctene"

set terminal postscript eps color
set title "ref = 2936; chem = cyclooctene; casrn = 931-88-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2094363E-03 * exp(  -4441.171    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.8435176E-03 point
set label "" at    278.1500    ,   0.6142061E-03 point
set label "" at    283.1500    ,   0.4562466E-03 point
set label "" at    288.1500    ,   0.3449545E-03 point
set label "" at    293.1500    ,   0.2646941E-03 point
set label "" at    298.1500    ,   0.2058140E-03 point
set label "" at    303.1500    ,   0.1625992E-03 point
set label "" at    308.1500    ,   0.1305367E-03 point
set label "" at    313.1500    ,   0.1055145E-03 point
set label "" at    298.1500    ,   0.2094363E-03 point ps 2 pt 6

plot [270:320] H(T)
