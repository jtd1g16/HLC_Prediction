# load "ref2996-647-42-7.gnu"
# chem = "6:2 FTOH"

set terminal postscript eps color
set title "ref = 2996; chem = 6:2 FTOH; casrn = 647-42-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8542163E-04 * exp(  -7013.114    *(1/   298.    -1/T))

set label "" at    318.1500    ,   0.2040244E-04 point
set label "" at    328.1500    ,   0.9675974E-05 point
set label "" at    338.1500    ,   0.5028109E-05 point
set label "" at    348.1500    ,   0.2873892E-05 point
set label "" at    358.1500    ,   0.1736158E-05 point
set label "" at    298.1500    ,   0.8542163E-04 point ps 2 pt 6

plot [310:360] H(T)
