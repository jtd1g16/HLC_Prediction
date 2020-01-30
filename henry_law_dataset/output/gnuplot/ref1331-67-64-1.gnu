# load "ref1331-67-64-1.gnu"
# chem = "propanone"

set terminal postscript eps color
set title "ref = 1331; chem = propanone; casrn = 67-64-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3172083     * exp(  -5443.499    *(1/   298.    -1/T))

set label "" at    361.4500    ,   0.1304901E-01 point
set label "" at    332.1500    ,   0.4828034E-01 point
set label "" at    322.1500    ,   0.8116146E-01 point
set label "" at    312.6500    ,   0.1373882     point
set label "" at    298.1500    ,   0.3172083     point ps 2 pt 6

plot [310:370] H(T)
