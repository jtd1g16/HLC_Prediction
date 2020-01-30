# load "ref2122-2162-99-4.gnu"
# chem = "1,8-dichlorooctane"

set terminal postscript eps color
set title "ref = 2122; chem = 1,8-dichlorooctane; casrn = 2162-99-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7529216E-02 * exp(  -7508.934    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.8309596E-01 point
set label "" at    278.1500    ,   0.4689992E-01 point
set label "" at    283.1500    ,   0.2767095E-01 point
set label "" at    288.1500    ,   0.1697605E-01 point
set label "" at    293.1500    ,   0.1080897E-01 point
set label "" at    298.1500    ,   0.7140892E-02 point
set label "" at    303.1500    ,   0.4854553E-02 point
set label "" at    308.1500    ,   0.3416167E-02 point
set label "" at    313.1500    ,   0.2481700E-02 point
set label "" at    298.1500    ,   0.7529216E-02 point ps 2 pt 6

plot [270:320] H(T)
