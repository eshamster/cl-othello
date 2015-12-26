# Spec

- CentOS 6.5 (on Sakura VPS)
- CPU
  - Westmere E56xx/L56xx/X56xx (Nehalem-C)
  - 2 Cores
  - 2.4GHz
- Memory: 1GB
- Lisp Implementation: SBCL/1.2.16

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
