# load "ref0630-112-31-2.gnu"
# chem = "decanal"

set terminal postscript eps color
set title "ref = 630; chem = decanal; casrn = 112-31-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6016566E-02 * exp(  -8739.855    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.6217617E-02 point
set label "" at    308.1500    ,   0.2171231E-02 point
set label "" at    318.1500    ,   0.9869233E-03 point
set label "" at    298.1500    ,   0.6016566E-02 point ps 2 pt 6

plot [290:320] H(T)
