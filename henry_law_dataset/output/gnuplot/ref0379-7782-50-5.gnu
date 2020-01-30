# load "ref0379-7782-50-5.gnu"
# chem = "molecular chlorine"

set terminal postscript eps color
set title "ref = 379; chem = molecular chlorine; casrn = 7782-50-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9206197E-03 * exp(  -2289.380    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.1403107E-02 point
set label "" at    284.1500    ,   0.1359240E-02 point
set label "" at    285.1500    ,   0.1317152E-02 point
set label "" at    286.1500    ,   0.1276397E-02 point
set label "" at    287.1500    ,   0.1237871E-02 point
set label "" at    288.1500    ,   0.1200234E-02 point
set label "" at    289.1500    ,   0.1164382E-02 point
set label "" at    290.1500    ,   0.1129870E-02 point
set label "" at    291.1500    ,   0.1096698E-02 point
set label "" at    292.1500    ,   0.1065769E-02 point
set label "" at    293.1500    ,   0.1036187E-02 point
set label "" at    294.1500    ,   0.1010212E-02 point
set label "" at    295.1500    ,   0.9855965E-03 point
set label "" at    296.1500    ,   0.9614394E-03 point
set label "" at    297.1500    ,   0.9391013E-03 point
set label "" at    298.1500    ,   0.9176806E-03 point
set label "" at    299.1500    ,   0.8971801E-03 point
set label "" at    300.1500    ,   0.8776030E-03 point
set label "" at    301.1500    ,   0.8598677E-03 point
set label "" at    302.1500    ,   0.8430676E-03 point
set label "" at    303.1500    ,   0.8267481E-03 point
set label "" at    298.1500    ,   0.9206197E-03 point ps 2 pt 6

plot [280:310] H(T)
