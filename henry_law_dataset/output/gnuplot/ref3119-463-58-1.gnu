# load "ref3119-463-58-1.gnu"
# chem = "carbon oxide sulfide"

set terminal postscript eps color
set title "ref = 3119; chem = carbon oxide sulfide; casrn = 463-58-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1498279E-03 * exp(  -3516.632    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.2831770E-03 point
set label "" at    288.1500    ,   0.2196816E-03 point
set label "" at    293.1500    ,   0.1864890E-03 point
set label "" at    298.1500    ,   0.1494057E-03 point
set label "" at    298.1500    ,   0.1498279E-03 point ps 2 pt 6

plot [280:300] H(T)
