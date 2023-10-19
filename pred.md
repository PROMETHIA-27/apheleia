```prolog
lists:append([], A, A).
lists:append([A|B], C, [A|D]) :- append(B, C, D).
```

```rust
fn append_find3<T>(ls: &List<T>, ls2: &List<T>, ls3: &mut List<T>) -> bool
where
    T: Clone + PartialEq
{
    match (ls, ls2) {
        (Nil, a) => {
            *ls3 = a.clone();
            true
        },
        (Cons(a, b), c) => {
            let mut d = Nil;
            append_find3(b, c, &mut d);
            *ls3 = cons(a.clone(), d);
            true
        },
        _ => false,
    }
}
```

```prolog
a :- b.
b :- c.
c :- d.
b.
```

```rust
fn a() -> bool {
    b()
}

fn b() -> bool {
    c() || true
}

fn c() -> bool {
    false
}
```

```prolog
append(I-M, M-O, I-O).
```

```rust
fn append_find3<I, M, O>(im: (I, M), mo: (M, O), io: &mut (I, O)) {
    match (im, mo, io) {
        ((i1, m1), (m2, o1), (i2, o2)) => {
            *i2 = i1.clone();
            *o2 = o1.clone();
        }
    }
}
```

```prolog
fib(0,1).   
fib(1,1).   
fib(N,F) :- 
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),      
    fib(N2, F2),     
    F is F1 + F2.
```

trying to malloc/free
```prolog
main :-
    M is alloc!(sizeof(arr(i32, 8)))
    forall(
        between(0, 8, I), 
        M + I *= 1
    )
    free!(M)
```