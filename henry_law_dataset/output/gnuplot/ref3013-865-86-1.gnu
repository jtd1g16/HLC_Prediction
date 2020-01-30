# load "ref3013-865-86-1.gnu"
# chem = "10-2FTOH"

set terminal postscript eps color
set title "ref = 3013; chem = 10-2FTOH; casrn = 865-86-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1280826E-03 * exp(  -2660.242    *(1/   298.    -1/T))

set label "" at    309.6000    ,   0.1000000E-03 point
set label "" at    314.5000    ,   0.7762471E-04 point
set label "" at    319.6000    ,   0.6606934E-04 point
set label "" at    323.0000    ,   0.6165950E-04 point
set label "" at    330.0000    ,   0.5495409E-04 point
set label "" at    334.2000    ,   0.5128614E-04 point
set label "" at    298.1500    ,   0.1280826E-03 point ps 2 pt 6

plot [300:340] H(T)
