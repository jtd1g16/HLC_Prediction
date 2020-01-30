# load "ref3008-80-56-8.gnu"
# chem = "alpha-pinene"

set terminal postscript eps color
set title "ref = 3008; chem = alpha-pinene; casrn = 80-56-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2920108E-03 * exp(  -1803.250    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.2862077E-03 point
set label "" at    293.0000    ,   0.3355539E-03 point
set label "" at    288.0000    ,   0.3552924E-03 point
set label "" at    283.0000    ,   0.4145078E-03 point
set label "" at    278.0000    ,   0.4441155E-03 point
set label "" at    298.1500    ,   0.2920108E-03 point ps 2 pt 6

plot [270:300] H(T)
