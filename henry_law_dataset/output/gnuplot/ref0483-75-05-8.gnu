# load "ref0483-75-05-8.gnu"
# chem = "ethane nitrile"

set terminal postscript eps color
set title "ref = 483; chem = ethane nitrile; casrn = 75-05-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4810998     * exp(  -3949.356    *(1/   298.    -1/T))

set label "" at    273.0000    ,    1.630065     point
set label "" at    298.0000    ,   0.4843182     point
set label "" at    298.1500    ,   0.4810998     point ps 2 pt 6

plot [270:300] H(T)
