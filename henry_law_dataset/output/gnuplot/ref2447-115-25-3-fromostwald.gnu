# load "ref2447-115-25-3-fromostwald.gnu"
# chem = "octafluorocyclobutane"

set terminal postscript eps color
set title "ref = 2447; chem = octafluorocyclobutane; casrn = 115-25-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1339788E-03 * exp(  -3156.953    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3147880E-03 point
set label "" at    283.1500    ,   0.2497621E-03 point
set label "" at    288.1500    ,   0.1849060E-03 point
set label "" at    293.1500    ,   0.1489301E-03 point
set label "" at    298.1500    ,   0.1214220E-03 point
set label "" at    303.1500    ,   0.1055334E-03 point
set label "" at    308.1500    ,   0.9289249E-04 point
set label "" at    313.1500    ,   0.8372785E-04 point
set label "" at    318.1500    ,   0.7711948E-04 point
set label "" at    298.1500    ,   0.1339788E-03 point ps 2 pt 6

plot [270:320] H(T)
