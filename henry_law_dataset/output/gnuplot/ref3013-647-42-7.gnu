# load "ref3013-647-42-7.gnu"
# chem = "6-2FTOH"

set terminal postscript eps color
set title "ref = 3013; chem = 6-2FTOH; casrn = 647-42-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1768010E-03 * exp(  -2628.800    *(1/   298.    -1/T))

set label "" at    309.6000    ,   0.1380384E-03 point
set label "" at    314.5000    ,   0.1071519E-03 point
set label "" at    319.6000    ,   0.9120108E-04 point
set label "" at    323.0000    ,   0.8709636E-04 point
set label "" at    330.0000    ,   0.7943282E-04 point
set label "" at    334.2000    ,   0.6918310E-04 point
set label "" at    298.1500    ,   0.1768010E-03 point ps 2 pt 6

plot [300:340] H(T)
