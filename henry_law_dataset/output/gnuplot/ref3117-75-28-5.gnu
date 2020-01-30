# load "ref3117-75-28-5.gnu"
# chem = "2-methylpropane"

set terminal postscript eps color
set title "ref = 3117; chem = 2-methylpropane; casrn = 75-28-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1065795E-03 * exp(  -5149.775    *(1/   298.    -1/T))

set label "" at    293.3000    ,   0.1545863E-03 point
set label "" at    292.7500    ,   0.1415394E-03 point
set label "" at    290.8500    ,   0.1599477E-03 point
set label "" at    288.6500    ,   0.1901784E-03 point
set label "" at    283.1500    ,   0.2481700E-03 point
set label "" at    283.1500    ,   0.2395754E-03 point
set label "" at    279.3500    ,   0.3739318E-03 point
set label "" at    277.8500    ,   0.4099401E-03 point
set label "" at    276.9500    ,   0.4192569E-03 point
set label "" at    276.0500    ,   0.4357631E-03 point
set label "" at    274.9500    ,   0.4099401E-03 point
set label "" at    298.1500    ,   0.1065795E-03 point ps 2 pt 6

plot [270:300] H(T)
