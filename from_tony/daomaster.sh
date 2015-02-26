#!/bin/csh
set input=${1}
daomaster << DONE
${input}.mch
3,1,3
99.
6
10
5
4
3
2
1
1
1
1
0
y
n
n
y

y


y

n
n
DONE
