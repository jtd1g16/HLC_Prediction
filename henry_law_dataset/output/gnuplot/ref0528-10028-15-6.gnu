# load "ref0528-10028-15-6.gnu"
# chem = "ozone"

set terminal postscript eps color
set title "ref = 528; chem = ozone; casrn = 10028-15-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1264857E-03 * exp(  -1969.602    *(1/   298.    -1/T))

set label "" at    276.6500    ,   0.2113518E-03 point
set label "" at    292.9500    ,   0.1422221E-03 point
set label "" at    298.1500    ,   0.1264857E-03 point ps 2 pt 6

plot [270:300] H(T)
