# load "ref1331-1194-65-6.gnu"
# chem = "2,6-dichlorobenzenenitrile"

set terminal postscript eps color
set title "ref = 1331; chem = 2,6-dichlorobenzenenitrile; casrn = 1194-65-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4800094     * exp(  -5438.628    *(1/   298.    -1/T))

set label "" at    361.4500    ,   0.2119425E-01 point
set label "" at    351.6500    ,   0.2693094E-01 point
set label "" at    341.8500    ,   0.4629314E-01 point
set label "" at    332.1500    ,   0.7704310E-01 point
set label "" at    298.1500    ,   0.4800094     point ps 2 pt 6

plot [330:370] H(T)
