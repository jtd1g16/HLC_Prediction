# load "ref0727-76-37-9.gnu"
# chem = "2,2,3,3-tetrafluoro-1-propanol"

set terminal postscript eps color
set title "ref = 727; chem = 2,2,3,3-tetrafluoro-1-propanol; casrn = 76-37-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.579725     * exp(  -6746.250    *(1/   298.    -1/T))

set label "" at    288.0000    ,    3.603732     point
set label "" at    298.0000    ,    1.551005     point
set label "" at    308.0000    ,   0.7464647     point
set label "" at    318.0000    ,   0.3958723     point
set label "" at    298.1500    ,    1.579725     point ps 2 pt 6

plot [280:320] H(T)
