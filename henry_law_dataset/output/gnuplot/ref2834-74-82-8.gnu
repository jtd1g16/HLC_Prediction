# load "ref2834-74-82-8.gnu"
# chem = "methane"

set terminal postscript eps color
set title "ref = 2834; chem = methane; casrn = 74-82-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1344575E-04 * exp(  -1855.244    *(1/   298.    -1/T))

set label "" at    273.4000    ,   0.2434068E-04 point
set label "" at    273.4500    ,   0.2431866E-04 point
set label "" at    273.3800    ,   0.2428344E-04 point
set label "" at    273.4200    ,   0.2428784E-04 point
set label "" at    273.4300    ,   0.2426582E-04 point
set label "" at    283.1300    ,   0.1839641E-04 point
set label "" at    283.1500    ,   0.1843163E-04 point
set label "" at    283.1500    ,   0.1836559E-04 point
set label "" at    293.2300    ,   0.1456126E-04 point
set label "" at    293.1500    ,   0.1458327E-04 point
set label "" at    293.1500    ,   0.1462290E-04 point
set label "" at    293.1300    ,   0.1457887E-04 point
set label "" at    293.2000    ,   0.1455245E-04 point
set label "" at    293.1700    ,   0.1452163E-04 point
set label "" at    293.1500    ,   0.1453924E-04 point
set label "" at    293.1500    ,   0.1456126E-04 point
set label "" at    293.1500    ,   0.1452603E-04 point
set label "" at    303.1000    ,   0.1217034E-04 point
set label "" at    303.2500    ,   0.1215713E-04 point
set label "" at    303.1500    ,   0.1214832E-04 point
set label "" at    313.1700    ,   0.1045751E-04 point
set label "" at    313.1800    ,   0.1040467E-04 point
set label "" at    313.1500    ,   0.1042228E-04 point
set label "" at    298.1500    ,   0.1344575E-04 point ps 2 pt 6

plot [270:320] H(T)
