# load "ref2939-507-55-1.gnu"
# chem = "CClF2CF2CHClF"

set terminal postscript eps color
set title "ref = 2939; chem = CClF2CF2CHClF; casrn = 507-55-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1079988E-03 * exp(  -3078.240    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.1085616E-03 point
set label "" at    353.0000    ,   0.2171231E-04 point
set label "" at    298.1500    ,   0.1079988E-03 point ps 2 pt 6

plot [290:360] H(T)
