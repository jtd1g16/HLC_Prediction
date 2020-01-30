# load "ref2808-422-05-9.gnu"
# chem = "2,2,3,3,3-pentafluoro-1-propanol"

set terminal postscript eps color
set title "ref = 2808; chem = 2,2,3,3,3-pentafluoro-1-propanol; casrn = 422-05-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1444415     * exp(  -4287.423    *(1/   298.    -1/T))

set label "" at    275.0000    ,   0.4845793     point
set label "" at    285.0000    ,   0.2792993     point
set label "" at    291.0000    ,   0.2072539     point
set label "" at    299.0000    ,   0.1381693     point
set label "" at    298.1500    ,   0.1444415     point ps 2 pt 6

plot [270:300] H(T)
