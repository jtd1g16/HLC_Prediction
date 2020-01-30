# load "ref3077-78-00-2.gnu"
# chem = "tetraethyllead"

set terminal postscript eps color
set title "ref = 3077; chem = tetraethyllead; casrn = 78-00-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1311493E-04 * exp(  -6431.271    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.9145028E-04 point
set label "" at    297.8500    ,   0.1738591E-04 point
set label "" at    299.7500    ,   0.1135222E-04 point
set label "" at    304.6500    ,   0.8153244E-05 point
set label "" at    304.6500    ,   0.6291944E-05 point
set label "" at    310.6500    ,   0.6027696E-05 point
set label "" at    298.1500    ,   0.1311493E-04 point ps 2 pt 6

plot [270:320] H(T)
