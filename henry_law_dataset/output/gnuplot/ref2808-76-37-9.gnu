# load "ref2808-76-37-9.gnu"
# chem = "2,2,3,3-tetrafluoro-1-propanol"

set terminal postscript eps color
set title "ref = 2808; chem = 2,2,3,3-tetrafluoro-1-propanol; casrn = 76-37-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.395914     * exp(  -6957.006    *(1/   298.    -1/T))

set label "" at    275.0000    ,    9.780410     point
set label "" at    283.0000    ,    4.924747     point
set label "" at    291.0000    ,    2.556131     point
set label "" at    299.0000    ,    1.273131     point
set label "" at    298.1500    ,    1.395914     point ps 2 pt 6

plot [270:300] H(T)
