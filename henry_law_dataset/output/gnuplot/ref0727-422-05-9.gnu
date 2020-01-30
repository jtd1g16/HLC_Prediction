# load "ref0727-422-05-9.gnu"
# chem = "2,2,3,3,3-pentafluoro-1-propanol"

set terminal postscript eps color
set title "ref = 727; chem = 2,2,3,3,3-pentafluoro-1-propanol; casrn = 422-05-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4529601     * exp(  -5995.051    *(1/   298.    -1/T))

set label "" at    288.0000    ,   0.9427225     point
set label "" at    298.0000    ,   0.4454724     point
set label "" at    308.0000    ,   0.2327843     point
set label "" at    318.0000    ,   0.1323938     point
set label "" at    298.1500    ,   0.4529601     point ps 2 pt 6

plot [280:320] H(T)
