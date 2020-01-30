# load "ref2834-630-08-0.gnu"
# chem = "carbon monoxide"

set terminal postscript eps color
set title "ref = 2834; chem = carbon monoxide; casrn = 630-08-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9666242E-05 * exp(  -1466.942    *(1/   298.    -1/T))

set label "" at    273.3200    ,   0.1546831E-04 point
set label "" at    273.3200    ,   0.1547711E-04 point
set label "" at    273.3200    ,   0.1545950E-04 point
set label "" at    273.2200    ,   0.1558719E-04 point
set label "" at    273.1700    ,   0.1563122E-04 point
set label "" at    273.1700    ,   0.1560481E-04 point
set label "" at    283.1700    ,   0.1234206E-04 point
set label "" at    283.1600    ,   0.1231564E-04 point
set label "" at    283.1700    ,   0.1234206E-04 point
set label "" at    283.2500    ,   0.1245654E-04 point
set label "" at    283.1800    ,   0.1243453E-04 point
set label "" at    283.2200    ,   0.1243453E-04 point
set label "" at    293.1800    ,   0.1018451E-04 point
set label "" at    293.1600    ,   0.1018011E-04 point
set label "" at    293.1500    ,   0.1021534E-04 point
set label "" at    293.1200    ,   0.1019332E-04 point
set label "" at    293.1700    ,   0.1024175E-04 point
set label "" at    293.1700    ,   0.1025056E-04 point
set label "" at    303.1200    ,   0.8793114E-05 point
set label "" at    303.1800    ,   0.8793114E-05 point
set label "" at    303.2300    ,   0.8779904E-05 point
set label "" at    303.1700    ,   0.8810727E-05 point
set label "" at    303.1900    ,   0.8806323E-05 point
set label "" at    303.1700    ,   0.8740276E-05 point
set label "" at    303.2500    ,   0.8810727E-05 point
set label "" at    303.2200    ,   0.8810727E-05 point
set label "" at    303.2200    ,   0.8793114E-05 point
set label "" at    312.9200    ,   0.7815612E-05 point
set label "" at    312.8300    ,   0.7815612E-05 point
set label "" at    312.7700    ,   0.7828822E-05 point
set label "" at    313.1500    ,   0.7837628E-05 point
set label "" at    313.1400    ,   0.7833225E-05 point
set label "" at    313.1200    ,   0.7789193E-05 point
set label "" at    313.1700    ,   0.7806806E-05 point
set label "" at    313.1200    ,   0.7846434E-05 point
set label "" at    313.1000    ,   0.7837628E-05 point
set label "" at    298.1500    ,   0.9666242E-05 point ps 2 pt 6

plot [270:320] H(T)
