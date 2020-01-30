# load "ref2496-34883-43-7.gnu"
# chem = "2,4'-dichlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 2,4'-dichlorobiphenyl; casrn = 34883-43-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2301672E-01 * exp(  -5268.756    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.7142857E-01 point
set label "" at    288.1500    ,   0.5000000E-01 point
set label "" at    298.1500    ,   0.2564103E-01 point
set label "" at    308.1500    ,   0.1136364E-01 point
set label "" at    298.1500    ,   0.2301672E-01 point ps 2 pt 6

plot [270:310] H(T)
