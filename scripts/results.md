# Spec

- CentOS 6.5 (on Sakura VPS)
- CPU
  - Westmere E56xx/L56xx/X56xx (Nehalem-C)
  - 2 Cores
  - 2.4GHz
- Memory: 1GB
- Lisp Implementation: SBCL/1.2.16

# results

## Refactor check-move-valid by removing duplicated check

2015.12.29

- 54.2 sec -> 51.2 sec
- 1.80 GB  -> 1.80 GB

"mc-3000" 
Evaluation took:
  21.109 seconds of real time
  21.102792 seconds of total run time (21.036802 user, 0.065990 system)
  [ Run times consist of 0.035 seconds GC time, and 21.068 seconds non-GC time. ]
  99.97% CPU
  72 lambdas converted
  50,661,112,902 processor cycles
  893,348,816 bytes consed
  

"uct-3000" 
Evaluation took:
  21.229 seconds of real time
  21.222773 seconds of total run time (21.123788 user, 0.098985 system)
  [ Run times consist of 0.093 seconds GC time, and 21.130 seconds non-GC time. ]
  99.97% CPU
  16 lambdas converted
  50,949,457,614 processor cycles
  886,711,392 bytes consed
  

"minimax-6" 
Evaluation took:
  8.855 seconds of real time
  8.853654 seconds of total run time (8.844656 user, 0.008998 system)
  99.99% CPU
  16 lambdas converted
  21,253,087,248 processor cycles
  17,176,432 bytes consed
  
Evaluation took:
  51.195 seconds of real time
  51.181219 seconds of total run time (51.006246 user, 0.174973 system)
  [ Run times consist of 0.128 seconds GC time, and 51.054 seconds non-GC time. ]
  99.97% CPU
  122,868,415,113 processor cycles
  1,797,386,960 bytes consed

## Inline board:get-piece

2015.12.29

- 56.9 sec -> 54.2 sec
- 1.80 GB  -> 1.80 GB

"mc-3000" 
Evaluation took:
  23.506 seconds of real time
  23.497428 seconds of total run time (23.434438 user, 0.062990 system)
  [ Run times consist of 0.032 seconds GC time, and 23.466 seconds non-GC time. ]
  99.96% CPU
  72 lambdas converted
  56,414,392,350 processor cycles
  893,353,296 bytes consed
  

"uct-3000" 
Evaluation took:
  21.929 seconds of real time
  21.922668 seconds of total run time (21.843680 user, 0.078988 system)
  [ Run times consist of 0.100 seconds GC time, and 21.823 seconds non-GC time. ]
  99.97% CPU
  16 lambdas converted
  52,628,433,546 processor cycles
  886,713,136 bytes consed
  

"minimax-6" 
Evaluation took:
  8.757 seconds of real time
  8.754668 seconds of total run time (8.749669 user, 0.004999 system)
  99.98% CPU
  16 lambdas converted
  21,017,332,956 processor cycles
  17,176,048 bytes consed
  
Evaluation took:
  54.193 seconds of real time
  54.177763 seconds of total run time (54.030786 user, 0.146977 system)
  [ Run times consist of 0.132 seconds GC time, and 54.046 seconds non-GC time. ]
  99.97% CPU
  130,065,405,126 processor cycles
  1,797,392,800 bytes consed

## Inline some functions in defines.lisp

2015.12.29

- 61.8 sec -> 56.9 sec
- 1.80 GB  -> 1.80 GB

"mc-3000" 
Evaluation took:
  24.379 seconds of real time
  24.375294 seconds of total run time (24.310304 user, 0.064990 system)
  [ Run times consist of 0.034 seconds GC time, and 24.342 seconds non-GC time. ]
  99.98% CPU
  72 lambdas converted
  58,512,736,404 processor cycles
  893,350,656 bytes consed
  

"uct-3000" 
Evaluation took:
  22.974 seconds of real time
  22.968508 seconds of total run time (22.882521 user, 0.085987 system)
  [ Run times consist of 0.089 seconds GC time, and 22.880 seconds non-GC time. ]
  99.98% CPU
  16 lambdas converted
  55,137,489,060 processor cycles
  886,711,344 bytes consed
  

"minimax-6" 
Evaluation took:
  9.581 seconds of real time
  9.578544 seconds of total run time (9.578544 user, 0.000000 system)
  99.98% CPU
  16 lambdas converted
  22,996,720,167 processor cycles
  17,176,096 bytes consed
  
Evaluation took:
  56.937 seconds of real time
  56.925346 seconds of total run time (56.774369 user, 0.150977 system)
  [ Run times consist of 0.123 seconds GC time, and 56.803 seconds non-GC time. ]
  99.98% CPU
  136,652,597,145 processor cycles
  1,797,388,416 bytes consed

## Optimize and inline get-fn-to-replace-by-next

2015.12.28

- 66.0 sec -> 61.8 sec
- 1.75 GB  -> 1.80 GB

"mc-3000" 
Evaluation took:
  26.160 seconds of real time
  26.154024 seconds of total run time (26.081035 user, 0.072989 system)
  [ Run times consist of 0.036 seconds GC time, and 26.119 seconds non-GC time. ]
  99.98% CPU
  72 lambdas converted
  62,784,946,188 processor cycles
  893,350,400 bytes consed
  

"uct-3000" 
Evaluation took:
  25.124 seconds of real time
  25.119181 seconds of total run time (25.022196 user, 0.096985 system)
  [ Run times consist of 0.092 seconds GC time, and 25.028 seconds non-GC time. ]
  99.98% CPU
  16 lambdas converted
  60,298,656,783 processor cycles
  889,689,056 bytes consed
  

"minimax-6" 
Evaluation took:
  10.507 seconds of real time
  10.504403 seconds of total run time (10.491405 user, 0.012998 system)
  99.97% CPU
  16 lambdas converted
  25,217,194,353 processor cycles
  17,173,360 bytes consed
  
Evaluation took:
  61.793 seconds of real time
  61.779608 seconds of total run time (61.596636 user, 0.182972 system)
  [ Run times consist of 0.128 seconds GC time, and 61.652 seconds non-GC time. ]
  99.98% CPU
  148,306,196,880 processor cycles
  1,800,363,120 bytes consed

## Refactor move-on-board by reducing duplicated check

2015.12.28

- 68.0 sec -> 66.0 sec
- 1.75 GB  -> 1.75 GB

"mc-3000" 
Evaluation took:
  27.810 seconds of real time
  27.804773 seconds of total run time (27.717786 user, 0.086987 system)
  [ Run times consist of 0.033 seconds GC time, and 27.772 seconds non-GC time. ]
  99.98% CPU
  72 lambdas converted
  66,747,264,363 processor cycles
  872,923,744 bytes consed
  

"uct-3000" 
Evaluation took:
  26.584 seconds of real time
  26.576960 seconds of total run time (26.485974 user, 0.090986 system)
  [ Run times consist of 0.090 seconds GC time, and 26.487 seconds non-GC time. ]
  99.97% CPU
  16 lambdas converted
  63,801,968,157 processor cycles
  869,276,704 bytes consed
  

"minimax-6" 
Evaluation took:
  11.581 seconds of real time
  11.579239 seconds of total run time (11.568241 user, 0.010998 system)
  99.98% CPU
  16 lambdas converted
  27,795,820,341 processor cycles
  10,746,208 bytes consed
  
Evaluation took:
  65.978 seconds of real time
  65.962972 seconds of total run time (65.774001 user, 0.188971 system)
  [ Run times consist of 0.123 seconds GC time, and 65.840 seconds non-GC time. ]
  99.98% CPU
  158,350,171,458 processor cycles
  1,753,096,592 bytes consed

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
