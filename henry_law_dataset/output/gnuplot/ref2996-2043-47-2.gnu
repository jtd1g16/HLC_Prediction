# load "ref2996-2043-47-2.gnu"
# chem = "4:2 FTOH"

set terminal postscript eps color
set title "ref = 2996; chem = 4:2 FTOH; casrn = 2043-47-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6052690E-04 * exp(  -5382.083    *(1/   298.    -1/T))

set label "" at    308.1500    ,   0.3617214E-04 point
set label "" at    318.1500    ,   0.1882197E-04 point
set label "" at    328.1500    ,   0.1082560E-04 point
set label "" at    338.1500    ,   0.6904269E-05 point
set label "" at    348.1500    ,   0.4730476E-05 point
set label "" at    358.1500    ,   0.3016991E-05 point
set label "" at    298.1500    ,   0.6052690E-04 point ps 2 pt 6

plot [300:360] H(T)
