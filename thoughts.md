# Thoughts ...

## ... on memory

### Using strict ByteStrings

Not sure if strict or lazy ByteStrings are better. Here's a single sample
running with strict ones and a lazy conversation from wreq:

```
     100,959,808 bytes allocated in the heap
      37,946,208 bytes copied during GC
       7,288,472 bytes maximum residency (9 sample(s))
         485,704 bytes maximum slop
              21 MB total memory in use (2 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       173 colls,   173 par    0.049s   0.023s     0.0001s    0.0003s
  Gen  1         9 colls,     8 par    0.091s   0.020s     0.0022s    0.0053s

  Parallel GC work balance: 17.18% (serial 0%, perfect 100%)

  TASKS: 21 (1 bound, 20 peak workers (20 total), using -N8)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.001s elapsed)
  MUT     time    0.071s  (  8.200s elapsed)
  GC      time    0.140s  (  0.043s elapsed)
  EXIT    time    0.000s  (  0.001s elapsed)
  Total   time    0.424s  (  8.244s elapsed)

  Alloc rate    1,412,222,800 bytes per MUT second

  Productivity  66.8% of total user, 3.4% of total elapsed

gc_alloc_block_sync: 9801
whitehole_spin: 0
gen[0].sync: 22
gen[1].sync: 2251
```

Seems alright to me. 100MB allocations seems a lot, but not even .1s pause.

### Using lazy ByteStrings, naively

After just changing a bunch of imports, and importantly removing one
lazy-to-strict conversion, the "total memory in use" for a static test image
is reduced from 24 to 17MB and the GC pause times to strictly below 0.1s. All
super unscientific, but already clearly better.

Now, let's try builders!
