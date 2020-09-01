#!/bin/bash
filename='N'
zamm=1
while read p; do
    echo $p > C_$zamm
    zamm=$(($zamm+1))
done < $filename

prosa=""
for ((jok=1; jok <= zamm-1; jok++)) do
    tropp=$(cat C_$jok)
    fropp=$(($tropp + 1))
    cropp=$(($tropp - 1))
    fropp1=$(($tropp + 2))
    cropp1=$(($tropp + 2))
    prosa="$tropp $fropp $cropp $fropp1 $cropp1 $prosa"
done
echo $prosa > finale
#sed -i "s/and$//" finale
resnum=$(cat finale)
rm C_* finale

sed "s/NNN/$resnum/g" fixate_ret.pgn > fixate_h.pgn
tail -1 1M0L.pdb > hitme
awk '{print $3}' hitme >  hitme1
base=$(cat hitme1)
case $base in 
    "C15")
        echo "C15 is our case"
        rename=REA
        awk '{print $2}' hitme > hitme2
        c15=$(cat hitme2)
        nz=$(($c15 - 1))
    ;;
    "C14")
        echo "C14 is our case"
        rename=REB
        awk '{print $2}' hitme > hitme2
        c14=$(cat hitme2)
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C13")
        echo "C13 is our case"
        rename=REC
        awk '{print $2}' hitme > hitme2
        c13=$(cat hitme2)
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C20")
        echo "C20 is our case"
        rename=RED
        awk '{print $2}' hitme > hitme2
        c20=$(cat hitme2)
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C12") 
        echo "C12 is our case"
        rename=REE
        awk '{print $2}' hitme > hitme2
        c12=$(cat hitme2)
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C11")
        echo "C11 is our case"
        rename=REF
        awk '{print $2}' hitme > hitme2
        c11=$(cat hitme2)
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C10")
        echo "C10 is our case"
        rename=REG
        awk '{print $2}' hitme > hitme2
        c10=$(cat hitme2)
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C9")
        echo "C9 is our case"
        rename=REH
        awk '{print $2}' hitme > hitme2
        c9=$(cat hitme2)
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C19")
        echo "C19 is our case"
        rename=REI
        awk '{print $2}' hitme > hitme2
        c19=$(cat hitme2)
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C8")
        echo "C8 is our case"
        rename=REJ
        awk '{print $2}' hitme > hitme2
        c8=$(cat hitme2)
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C7") 
        echo "C7 is our case"
        rename=REK
        awk '{print $2}' hitme > hitme2
        c7=$(cat hitme2)
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C6")
        echo "C6 is our case"
        rename=REM
        awk '{print $2}' hitme > hitme2
        c6=$(cat hitme2)
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C5") 
        echo "C5 is our case"
        rename=REN
        awk '{print $2}' hitme > hitme2
        c5=$(cat hitme2)
        c6=$(($c5 - 1))
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C18")
        echo "C18 is our case"
        rename=REO
        awk '{print $2}' hitme > hitme2
        c18=$(cat hitme2)
        c5=$(($c18 - 1))
        c6=$(($c5 - 1))
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C4")
        echo "C4 is our case"
        rename=REP
        awk '{print $2}' hitme > hitme2
        c4=$(cat hitme2)
        c18=$(($c4 - 1))
        c5=$(($c18 - 1))
        c6=$(($c5 - 1))
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C3")
        echo "C3 is our case"
        rename=REQ
        awk '{print $2}' hitme > hitme2
        c3=$(cat hitme2)
        c4=$(($c3 - 1))
        c18=$(($c4 - 1))
        c5=$(($c18 - 1))
        c6=$(($c5 - 1))
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C2")
        echo "C2 is our case"
        rename=RER
        awk '{print $2}' hitme > hitme2
        c2=$(cat hitme2)
        c3=$(($c2 - 1))
        c4=$(($c3 - 1))
        c18=$(($c4 - 1))
        c5=$(($c18 - 1))
        c6=$(($c5 - 1))
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C1")
        echo "C1 is our case"
        rename=RES
        awk '{print $2}' hitme > hitme2
        c1=$(cat hitme2)
        c2=$(($c1 - 1))
        c3=$(($c2 - 1))
        c4=$(($c3 - 1))
        c18=$(($c4 - 1))
        c5=$(($c18 - 1))
        c6=$(($c5 - 1))
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C17")
        echo "C17 is our case"
        rename=REU
        awk '{print $2}' hitme > hitme2
        c17=$(cat hitme2)
        c1=$(($c17 - 1))
        c2=$(($c1 - 1))
        c3=$(($c2 - 1))
        c4=$(($c3 - 1))
        c18=$(($c4 - 1))
        c5=$(($c18 - 1))
        c6=$(($c5 - 1))
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    "C16")
        echo "C16 is our case, the retinal is finally inserted"
        rename=REL
        awk '{print $2}' hitme > hitme2
        c16=$(cat hitme2)
        c17=$(($c16 - 1))
        c1=$(($c17 - 1))
        c2=$(($c1 - 1))
        c3=$(($c2 - 1))
        echo $c1 $c2 $c3
        c4=$(($c3 - 1))
        c18=$(($c4 - 1))
        c5=$(($c18 - 1))
        c6=$(($c5 - 1))
        c7=$(($c6 - 1))
        c8=$(($c7 - 1))
        c19=$(($c8 - 1))
        c9=$(($c19 - 1))
        c10=$(($c9 - 1))
        c11=$(($c10 - 1))
        c12=$(($c11 - 1))
        c20=$(($c12 - 1))
        c13=$(($c20 - 1))
        c14=$(($c13 - 1))
        c15=$(($c14 - 1))
        nz=$(($c15 - 1))
    ;;
    *)
        echo "Something got completely wrong!!!"
esac

sed "s/REL/${rename}/g" 1M0L.pdb > 1M0L_a.pdb
vmd -dispdev text -e create.pgn
vmd -dispdev text -e fixate_h.pgn
vmd -dispdev text -e size.pgn > size.log
awk 'c&&!--c; /atomselect0/{c=1}' size.log > A
sed 's/{//g; s/}//g' A > AA
awk 'c&&!--c; /atomselect0/{c=2}' size.log > B
awk '{print $1}' AA > X1
awk '{print $4}' AA > X2
x1=$(cat X1)
x2=$(cat X2)
dx=$(awk "BEGIN {print $x2 - $x1 + 0.5; exit}")
ddx=$(awk -v v="$dx" 'BEGIN{printf "%d", v}')
Dx=$(awk "BEGIN {print $ddx + 1.2 ; exit}")
awk '{print $2}' AA > Y1
awk '{print $5}' AA > Y2
y1=$(cat Y1)
y2=$(cat Y2)
dy=$(awk "BEGIN {print $y2 - $y1 + 0.5; exit}")
ddy=$(awk -v v="$dy" 'BEGIN{printf "%d", v}')
Dy=$(awk "BEGIN {print $ddy + 1.2 ; exit}")
awk '{print $3}' AA > Z1
awk '{print $6}' AA > Z2
z1=$(cat Z1)
z2=$(cat Z2)
dz=$(awk "BEGIN {print $z2 - $z1 + 0.5; exit}")
ddz=$(awk -v v="$dz" 'BEGIN{printf "%d", v}')
Dz=$(awk "BEGIN {print $ddz + 1.2 ; exit}")
awk '{print $1}' B > C1
awk '{print $2}' B > C2
awk '{print $3}' B > C3
q1=$(cat C1)
q2=$(cat C2)
q3=$(cat C3)
sed "s/xxxx/${Dx}/; s/yyyy/${Dy}/; s/zzzz/${Dz}/; s/aaaa/${q1}/; s/bbbb/${q2}/; s/cccc/${q3}/"  m1.conf > m11.conf
rm A AA B C1 C2 C3 X1 X2 Z1 Z2 Y1 Y2

/home/wurthel/Desktop/NAMD_2.13_Linux-x86_64/namd2 m11.conf > m1.log

vmd -dispdev text -e mout.pgn
cp 1M0L_0.pdb 1M0L_A.pdb

rm 1M0L_min-0.* 
rm hitme hitme1 hitme2
rm m1.log 

grep "NZ  $rename" 1M0L_0.pdb > NZ-1 
grep "C15 $rename" 1M0L_0.pdb > C15-1
grep "C14 $rename" 1M0L_0.pdb > C14-1
grep "C13 $rename" 1M0L_0.pdb > C13-1
grep "C20 $rename" 1M0L_0.pdb > C20-1
grep "C12 $rename" 1M0L_0.pdb > C12-1
grep "C11 $rename" 1M0L_0.pdb > C11-1
grep "C10 $rename" 1M0L_0.pdb > C10-1
grep "C9  $rename" 1M0L_0.pdb > C9-1
grep "C19 $rename" 1M0L_0.pdb > C19-1
grep "C8  $rename" 1M0L_0.pdb > C8-1
grep "C7  $rename" 1M0L_0.pdb > C7-1
grep "C6  $rename" 1M0L_0.pdb > C6-1
grep "C5  $rename" 1M0L_0.pdb > C5-1
grep "C18 $rename" 1M0L_0.pdb > C18-1
grep "C4  $rename" 1M0L_0.pdb > C4-1
grep "C3  $rename" 1M0L_0.pdb > C3-1
grep "C2  $rename" 1M0L_0.pdb > C2-1
grep "C1  $rename" 1M0L_0.pdb > C1-1
grep "C17 $rename" 1M0L_0.pdb > C17-1
grep "C16 $rename" 1M0L_0.pdb > C16-1
grep "CE  $rename" 1M0L_0.pdb > CE 
grep "CD  $rename" 1M0L_0.pdb > CD
grep "CG  $rename" 1M0L_0.pdb > CG

sed -i "/HZ1 $rename/d" 1M0L_0.pdb
sed -i "/H151 $rename/d" 1M0L_0.pdb
sed -i "/NZ  $rename/d" 1M0L_0.pdb
sed -i "/C15 $rename/d" 1M0L_0.pdb
sed -i "/C1  $rename/d" 1M0L_0.pdb
sed -i "/C2  $rename/d" 1M0L_0.pdb
sed -i "/ H21 $rename/d" 1M0L_0.pdb
sed -i "/ H22 $rename/d" 1M0L_0.pdb
sed -i "/ C3  $rename/d" 1M0L_0.pdb
sed -i "/ H31 $rename/d" 1M0L_0.pdb
sed -i "/ H32 $rename/d" 1M0L_0.pdb
sed -i "/ C4  $rename/d" 1M0L_0.pdb
sed -i "/ H41 $rename/d" 1M0L_0.pdb
sed -i "/ H42 $rename/d" 1M0L_0.pdb
sed -i "/ C5  $rename/d" 1M0L_0.pdb
sed -i "/ C6  $rename/d" 1M0L_0.pdb
sed -i "/ C7  $rename/d" 1M0L_0.pdb
sed -i "/ H71 $rename/d" 1M0L_0.pdb
sed -i "/ C8  $rename/d" 1M0L_0.pdb
sed -i "/ H81 $rename/d" 1M0L_0.pdb
sed -i "/ C9  $rename/d" 1M0L_0.pdb
sed -i "/ C10 $rename/d" 1M0L_0.pdb
sed -i "/H101 $rename/d" 1M0L_0.pdb
sed -i "/ C11 $rename/d" 1M0L_0.pdb
sed -i "/H111 $rename/d" 1M0L_0.pdb
sed -i "/ C12 $rename/d" 1M0L_0.pdb
sed -i "/H121 $rename/d" 1M0L_0.pdb
sed -i "/ C13 $rename/d" 1M0L_0.pdb
sed -i "/ C14 $rename/d" 1M0L_0.pdb
sed -i "/H141 $rename/d" 1M0L_0.pdb
sed -i "/ C16 $rename/d" 1M0L_0.pdb
sed -i "/H161 $rename/d" 1M0L_0.pdb
sed -i "/H162 $rename/d" 1M0L_0.pdb
sed -i "/H163 $rename/d" 1M0L_0.pdb
sed -i "/ C17 $rename/d" 1M0L_0.pdb
sed -i "/H171 $rename/d" 1M0L_0.pdb
sed -i "/H172 $rename/d" 1M0L_0.pdb
sed -i "/H173 $rename/d" 1M0L_0.pdb
sed -i "/ C18 $rename/d" 1M0L_0.pdb
sed -i "/H181 $rename/d" 1M0L_0.pdb
sed -i "/H182 $rename/d" 1M0L_0.pdb
sed -i "/H183 $rename/d" 1M0L_0.pdb
sed -i "/ C19 $rename/d" 1M0L_0.pdb
sed -i "/H191 $rename/d" 1M0L_0.pdb
sed -i "/H192 $rename/d" 1M0L_0.pdb
sed -i "/H193 $rename/d" 1M0L_0.pdb
sed -i "/ C20 $rename/d" 1M0L_0.pdb
sed -i "/H201 $rename/d" 1M0L_0.pdb
sed -i "/H202 $rename/d" 1M0L_0.pdb
sed -i "/H203 $rename/d" 1M0L_0.pdb

sed -i "/END/d" 1M0L_0.pdb
awk '{print $2}' NZ-1 > nz_1
awk '{print $2}' C15-1 > c15_1
awk '{print $2}' C14-1 > c14_1
awk '{print $2}' C13-1 > c13_1
awk '{print $2}' C20-1 > c20_1
awk '{print $2}' C12-1 > c12_1
awk '{print $2}' C11-1 > c11_1
awk '{print $2}' C10-1 > c10_1
awk '{print $2}' C9-1 >  c9_1
awk '{print $2}' C19-1 > c19_1
awk '{print $2}' C8-1 >  c8_1
awk '{print $2}' C7-1 >  c7_1
awk '{print $2}' C6-1 >  c6_1
awk '{print $2}' C5-1 >  c5_1
awk '{print $2}' C18-1 > c18_1
awk '{print $2}' C4-1 >  c4_1
awk '{print $2}' C3-1 >  c3_1
awk '{print $2}' C2-1 >  c2_1
awk '{print $2}' C1-1 >  c1_1
awk '{print $2}' C17-1 > c17_1
awk '{print $2}' C16-1 > c16_1
awk '{print $2}' CE > c-e
awk '{print $2}' CD > c-d
awk '{print $2}' CG > c-g

ces=$(cat c-e)
cds=$(cat c-d)
cgs=$(cat c-g)

nz_1=$(cat nz_1)
c15_1=$(cat c15_1)
c14_1=$(cat c14_1)
c13_1=$(cat c13_1)
c20_1=$(cat c20_1)
c12_1=$(cat c12_1)
c11_1=$(cat c11_1)
c10_1=$(cat c10_1)
c9_1=$(cat c9_1)
c19_1=$(cat c19_1)
c8_1=$(cat c8_1)
c7_1=$(cat c7_1)
c6_1=$(cat c6_1)
c5_1=$(cat c5_1)
c18_1=$(cat c18_1)
c4_1=$(cat c4_1)
c3_1=$(cat c3_1)
c2_1=$(cat c2_1)
c1_1=$(cat c1_1)
c17_1=$(cat c17_1)
c16_1=$(cat c16_1)


echo "c3 = " $c3
echo "c3_1 = " $c3_1

sed -i "s/$nz_1/$nz/g" NZ-1
sed -i "s/$c15_1/$c15/g" C15-1
sed -i "s/$c14_1/$c14/g" C14-1
sed -i "s/$c13_1/$c13/g" C13-1
sed -i "s/$c20_1/$c20/g" C20-1
sed -i "s/$c12_1/$c12/g" C12-1
sed -i "s/$c11_1/$c11/g" C11-1
sed -i "s/$c10_1/$c10/g" C10-1
sed -i "s/$c9_1/$c9/g" C9-1
sed -i "s/$c19_1/$c19/g" C19-1
sed -i "s/$c8_1/$c8/g" C8-1
sed -i "s/$c7_1/$c7/g" C7-1
sed -i "s/$c6_1/$c6/g" C6-1
sed -i "s/$c5_1/$c5/g" C5-1
sed -i "s/$c18_1/$c18/g" C18-1
sed -i "s/$c4_1/$c4/g" C4-1
sed -i "s/$c3_1/$c3/g" C3-1
sed -i "s/$c2_1/$c2/g" C2-1
sed -i "s/$c1_1/$c1/g" C1-1
sed -i "s/$c17_1/$c17/g" C17-1
sed -i "s/$c16_1/$c16/g" C16-1

cat NZ-1 >> 1M0L_0.pdb
cat C15-1 >> 1M0L_0.pdb
cat C14-1 >> 1M0L_0.pdb
cat C13-1 >> 1M0L_0.pdb
cat C20-1 >> 1M0L_0.pdb
cat C12-1 >> 1M0L_0.pdb
cat C11-1 >> 1M0L_0.pdb
cat C10-1 >> 1M0L_0.pdb
cat C9-1 >> 1M0L_0.pdb
cat C19-1 >> 1M0L_0.pdb
cat C8-1 >> 1M0L_0.pdb
cat C7-1 >> 1M0L_0.pdb
cat C6-1 >> 1M0L_0.pdb
cat C5-1 >> 1M0L_0.pdb
cat C18-1 >> 1M0L_0.pdb
cat C4-1 >> 1M0L_0.pdb
cat C3-1 >> 1M0L_0.pdb
cat C2-1 >> 1M0L_0.pdb
cat C1-1 >> 1M0L_0.pdb
cat C17-1 >> 1M0L_0.pdb
cat C16-1 >> 1M0L_0.pdb

sed -i "s/$rename/REL/g" 1M0L_0.pdb

#sed -i "s/${ces}/8002/g" 1M0L_0.pdb
#sed -i "s/${cds}/8001/g" 1M0L_0.pdb
#sed -i "s/${cgs}/8000/g" 1M0L_0.pdb


rm NZ-1 C15-1 C14-1 C13-1 C20-1 C12-1 C11-1 C10-1 C9-1 C19-1 C8-1 C7-1 C6-1 C5-1 C4-1 C3-1 C2-1 C1-1 C18-1 C17-1 C16-1
rm nz_1 c15_1 c14_1 c13_1 c20_1 c12_1 c11_1 c10_1 c9_1 c19_1 c8_1 c7_1 c6_1 c5_1 c4_1 c3_1 c2_1 c1_1 c18_1 c17_1 c16_1

