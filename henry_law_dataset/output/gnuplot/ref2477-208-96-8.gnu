# load "ref2477-208-96-8.gnu"
# chem = "acenaphthylene"

set terminal postscript eps color
set title "ref = 2477; chem = acenaphthylene; casrn = 208-96-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7881004E-01 * exp(  -6607.540    *(1/   298.    -1/T))

set label "" at    277.2500    ,   0.4201681     point
set label "" at    284.1500    ,   0.2341920     point
set label "" at    291.1500    ,   0.1340483     point
set label "" at    298.1500    ,   0.7874016E-01 point
set label "" at    304.1500    ,   0.5102041E-01 point
set label "" at    298.1500    ,   0.7881004E-01 point ps 2 pt 6

plot [270:310] H(T)
