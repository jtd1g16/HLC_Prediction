# load "ref0483-67-63-0.gnu"
# chem = "2-propanol"

set terminal postscript eps color
set title "ref = 483; chem = 2-propanol; casrn = 67-63-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.235524     * exp(  -7446.898    *(1/   298.    -1/T))

set label "" at    273.0000    ,    12.33563     point
set label "" at    298.0000    ,    1.251155     point
set label "" at    298.1500    ,    1.235524     point ps 2 pt 6

plot [270:300] H(T)
