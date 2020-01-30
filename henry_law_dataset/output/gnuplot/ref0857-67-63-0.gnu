# load "ref0857-67-63-0.gnu"
# chem = "2-propanol"

set terminal postscript eps color
set title "ref = 857; chem = 2-propanol; casrn = 67-63-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7916446     * exp(  -5746.130    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.3168600     point
set label "" at    333.1500    ,   0.1032505     point
set label "" at    343.1500    ,   0.6273861E-01 point
set label "" at    353.1500    ,   0.3984671E-01 point
set label "" at    298.1500    ,   0.7916446     point ps 2 pt 6

plot [310:360] H(T)
