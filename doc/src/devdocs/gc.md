# Garbage Collection in Julia


## Allocation via Free Lists
Julia has a precise generational non-moving garbage collector.  Free lists, maintained by the GC, allow fast object allocation.

![GC2 Image](../assets/images/GC2.png)


## Heuristics

## System Initialization

When you start Julia you initialize the system by loading objects into memory. This is why your first GC might be a full GC because all
![System Initialization Image](../assets/images/SystemInitialization.png)

## Generational
![GC1 Image](../assets/images/GC1.png)

