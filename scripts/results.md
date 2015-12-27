# Spec

- CentOS 6.5 (on Sakura VPS)
- CPU
  - Westmere E56xx/L56xx/X56xx (Nehalem-C)
  - 2 Cores
  - 2.4GHz
- Memory: 1GB
- Lisp Implementation: SBCL/1.2.16

# results

## Refactor mc-simulate slightly

2015.12.27 (Maybe this improvement is within the error range)

- 69.8 sec -> 68.0 sec
- 1.75 GB  -> 1.75 GB

"mc-3000" 
Evaluation took:
  28.869 seconds of real time
  28.861612 seconds of total run time (28.787623 user, 0.073989 system)
  [ Run times consist of 0.032 seconds GC time, and 28.830 seconds non-GC time. ]
  99.98% CPU
  72 lambdas converted
  69,285,649,389 processor cycles
  872,923,536 bytes consed
  

"uct-3000" 
Evaluation took:
  27.094 seconds of real time
  27.088882 seconds of total run time (27.019893 user, 0.068989 system)
  [ Run times consist of 0.089 seconds GC time, and 27.0000 seconds non-GC time. ]
  99.98% CPU
  16 lambdas converted
  65,028,418,782 processor cycles
  869,276,864 bytes consed
  

"minimax-6" 
Evaluation took:
  12.031 seconds of real time
  12.028172 seconds of total run time (12.017173 user, 0.010999 system)
  99.98% CPU
  16 lambdas converted
  28,874,844,732 processor cycles
  10,746,272 bytes consed
  
Evaluation took:
  67.997 seconds of real time
  67.981666 seconds of total run time (67.827689 user, 0.153977 system)
  [ Run times consist of 0.121 seconds GC time, and 67.861 seconds non-GC time. ]
  99.98% CPU
  163,194,817,440 processor cycles
  1,753,096,400 bytes consed

## Optimize decide-move-by-random-policy

2015.12.27

- 74.8 sec -> 69.8 sec
- 1.82 GB  -> 1.75 GB

"mc-3000" 
Evaluation took:
  29.400 seconds of real time
  29.394530 seconds of total run time (29.311543 user, 0.082987 system)
  [ Run times consist of 0.034 seconds GC time, and 29.361 seconds non-GC time. ]
  99.98% CPU
  72 lambdas converted
  70,562,404,089 processor cycles
  873,084,560 bytes consed
  

"uct-3000" 
Evaluation took:
  28.523 seconds of real time
  28.516665 seconds of total run time (28.422679 user, 0.093986 system)
  [ Run times consist of 0.095 seconds GC time, and 28.422 seconds non-GC time. ]
  99.98% CPU
  16 lambdas converted
  68,456,288,358 processor cycles
  869,293,472 bytes consed
  

"minimax-6" 
Evaluation took:
  11.844 seconds of real time
  11.841200 seconds of total run time (11.840200 user, 0.001000 system)
  99.97% CPU
  16 lambdas converted
  28,427,711,562 processor cycles
  10,757,472 bytes consed
  
Evaluation took:
  69.770 seconds of real time
  69.755395 seconds of total run time (69.577422 user, 0.177973 system)
  [ Run times consist of 0.129 seconds GC time, and 69.627 seconds non-GC time. ]
  99.98% CPU
  167,451,730,572 processor cycles
  1,753,303,584 bytes consed

## Optimize get-piecs

- 89.6 sec -> 74.8 sec
- 1.82 GB -> 1.82 GB

"mc-3000" 
Evaluation took:
  32.743 seconds of real time
  32.735023 seconds of total run time (32.658035 user, 0.076988 system)
  [ Run times consist of 0.035 seconds GC time, and 32.701 seconds non-GC time. ]
  99.98% CPU
  72 lambdas converted
  78,585,487,938 processor cycles
  907,170,080 bytes consed
  

"uct-3000" 
Evaluation took:
  30.053 seconds of real time
  30.046432 seconds of total run time (29.980442 user, 0.065990 system)
  [ Run times consist of 0.089 seconds GC time, and 29.958 seconds non-GC time. ]
  99.98% CPU
  16 lambdas converted
  72,128,564,166 processor cycles
  905,778,736 bytes consed
  

"minimax-6" 
Evaluation took:
  11.999 seconds of real time
  11.997177 seconds of total run time (11.995177 user, 0.002000 system)
  99.98% CPU
  16 lambdas converted
  28,797,392,016 processor cycles
  10,756,624 bytes consed
  
Evaluation took:
  74.798 seconds of real time
  74.780632 seconds of total run time (74.634654 user, 0.145978 system)
  [ Run times consist of 0.124 seconds GC time, and 74.657 seconds non-GC time. ]
  99.98% CPU
  179,517,140,637 processor cycles
  1,823,858,896 bytes consed
  

## Optimize is-xxx-dir

2015.12.26

- 100.2 sec -> 89.6 sec
- 4.89 GB -> 1.82 GB

"mc-3000" 
Evaluation took:
  38.405 seconds of real time
  38.395163 seconds of total run time (38.297178 user, 0.097985 system)
  [ Run times consist of 0.032 seconds GC time, and 38.364 seconds non-GC time. ]
  99.97% CPU
  72 lambdas converted
  92,174,065,785 processor cycles
  907,174,368 bytes consed
  

"uct-3000" 
Evaluation took:
  36.924 seconds of real time
  36.916388 seconds of total run time (36.829401 user, 0.086987 system)
  [ Run times consist of 0.087 seconds GC time, and 36.830 seconds non-GC time. ]
  99.98% CPU
  16 lambdas converted
  88,620,853,929 processor cycles
  905,778,672 bytes consed
  

"minimax-6" 
Evaluation took:
  14.251 seconds of real time
  14.246834 seconds of total run time (14.233836 user, 0.012998 system)
  99.97% CPU
  16 lambdas converted
  34,203,469,032 processor cycles
  10,756,976 bytes consed
  
Evaluation took:
  89.582 seconds of real time
  89.560384 seconds of total run time (89.361415 user, 0.198969 system)
  [ Run times consist of 0.119 seconds GC time, and 89.442 seconds non-GC time. ]
  99.98% CPU
  215,003,443,983 processor cycles
  1,823,863,472 bytes consed

## 9e138f1b97be1cf47f1918c66e46d70ef7dc2b1b

2015.12.26

- 100.2 sec
- 4.89 GB

```
"mc-3000" 
Evaluation took:
  42.540 seconds of real time
  42.528535 seconds of total run time (42.367559 user, 0.160976 system)
  [ Run times consist of 0.094 seconds GC time, and 42.435 seconds non-GC time. ]
  99.97% CPU
  72 lambdas converted
  102,096,208,347 processor cycles
  2,263,661,936 bytes consed
  

"uct-3000" 
Evaluation took:
  41.149 seconds of real time
  41.139746 seconds of total run time (41.019764 user, 0.119982 system)
  [ Run times consist of 0.185 seconds GC time, and 40.955 seconds non-GC time. ]
  99.98% CPU
  16 lambdas converted
  98,759,997,585 processor cycles
  2,264,084,848 bytes consed
  

"minimax-6" 
Evaluation took:
  16.505 seconds of real time
  16.502491 seconds of total run time (16.492493 user, 0.009998 system)
  [ Run times consist of 0.024 seconds GC time, and 16.479 seconds non-GC time. ]
  99.98% CPU
  16 lambdas converted
  39,614,249,064 processor cycles
  365,174,544 bytes consed
  
Evaluation took:
  100.197 seconds of real time
  100.172772 seconds of total run time (99.880816 user, 0.291956 system)
  [ Run times consist of 0.303 seconds GC time, and 99.870 seconds non-GC time. ]
  99.98% CPU
  240,476,484,594 processor cycles
  4,893,096,848 bytes consed
```
