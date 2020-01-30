# load "ref0862-7440-37-1.gnu"
# chem = "argon"

set terminal postscript eps color
set title "ref = 862; chem = argon; casrn = 7440-37-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1454716E-04 * exp(  -1448.531    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2333676E-04 point
set label "" at    283.1500    ,   0.1849328E-04 point
set label "" at    293.1500    ,   0.1541107E-04 point
set label "" at    303.1500    ,   0.1320949E-04 point
set label "" at    313.1500    ,   0.1188854E-04 point
set label "" at    298.1500    ,   0.1454716E-04 point ps 2 pt 6

plot [270:320] H(T)
