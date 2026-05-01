## Work-Stealing Deque Implementation in OCaml 5

An implementation of the Chase-Lev dynamic circular work-stealing deque
([SPAA 2005](https://dl.acm.org/doi/10.1145/1073970.1073974)) in OCaml 5,
together with a parallel task scheduler and benchmarks.

### Repository structure

```
.
├── lib/          Core library
│   ├── circular_array.ml   Dynamic cyclic array backing the deque
│   ├── deque.ml            Lock-free work-stealing deque (Chase-Lev)
│   ├── future.ml           Thread-safe single-assignment result box
│   ├── scheduler.ml        Parallel work-stealing scheduler (fork/join)
│   ├── naive_scheduler.ml  Mutex-protected shared-queue scheduler
│   └── benchmark.ml        Shared timing and statistics utilities
│
├── bin/          Benchmark executables
│   ├── fibonacci.ml        Parallel Fibonacci — compute-bound, balanced
│   ├── mergesort.ml        Parallel mergesort — memory-bound, unbalanced
│   └── map.ml              Parallel prime map — compute-bound, balanced
│
├── test/         Correctness tests
│   ├── test_deque_qcheck_lin.ml   QCheck-Lin linearizability tests
│   └── test_deque_qcheck_stm.ml   QCheck-STM sequential model tests
│
├── outputs/      Benchmark results and plots
│
├── report/       LaTeX source for the project report
│
└── dune-project  Dune build configuration
```

## Shrink Policies

### `Simple` — Standard
Shrinks the underlying circular array if the number of elements is less than `array size / K`, where `K ≥ 3`.

---

### `No_Copy` — Shrink Without Full Copying
**Strategy:**
- When growing, a back-pointer (`a.prev`) is stored to the smaller array, along with a
  `low_water_mark` recording the lowest `bottom` index ever written into the big array
  while it was active.
- On shrink, the smaller array is reused. Only indices in the range `[lwm, bottom)` were
  mutated while the big array was live, so only those slots need to be copied back.

---

### `Multi` — Combined Multiple Shrinks
**Strategy:**

Walk the chain of back-pointers (`a → a.prev → a.prev.prev → …`) to find the smallest
ancestor whose size can still hold all live elements (i.e. `size > num_elements`, with
one cell unused per the paper). Track the minimum LWM across every skipped array.

---

### `No_Shrink` — Never Shrinks
Does not shrink; only grows. 


### Building

Requires OCaml 5.4 and opam.

To check for data races, please install a TSAN-instrumented OCaml switch
and then install dependencies:
```bash
opam switch create test-tsan ocaml-variants.5.4.0+options ocaml-option-tsan
opam install dune qcheck qcheck-lin qcheck-stm unix
```

#### Build everything (code, tests etc.)

```bash
dune build
```

#### Run QCheck tests

```bash
dune test
```

### Running benchmarks

```bash
dune exec bin/fibonacci.exe
dune exec bin/mergesort.exe
dune exec bin/map.exe
```

### Running tests individually

#### QCheck-Lin linearizability test
```bash
dune exec test/test_deque_qcheck_lin.exe
```

#### QCheck-STM sequential model test

```bash
dune exec test/test_deque_qcheck_stm.exe
```

## Main References

- Chase & Lev. *Dynamic Circular Work-Stealing Deque*. SPAA 2005.
- Arora, Blumofe & Plaxton. *Thread Scheduling for Multiprogrammed
  Multiprocessors*. SPAA 1998.
- Herlihy, Shavit, Luchangco & Spear. *The Art of Multiprocessor
  Programming*. 2nd ed. 2020.
