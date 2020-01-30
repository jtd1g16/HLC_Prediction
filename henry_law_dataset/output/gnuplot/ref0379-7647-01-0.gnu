# load "ref0379-7647-01-0.gnu"
# chem = "hydrogen chloride"

set terminal postscript eps color
set title "ref = 379; chem = hydrogen chloride; casrn = 7647-01-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1877925     * exp(  -599.2743    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2254419     point
set label "" at    283.1500    ,   0.2091502     point
set label "" at    293.1500    ,   0.1946197     point
set label "" at    303.1500    ,   0.1814103     point
set label "" at    298.1500    ,   0.1877925     point ps 2 pt 6

plot [270:310] H(T)
