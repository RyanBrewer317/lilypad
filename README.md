# lilypad

a minimal functional language (bidir, first class functions, ADTs, backpassing with application-mode bidir, call/cc) for writing compilers. No typeclasses, implicits, HKTs, etc. Compiles to Wasm via a sequent calculus IR. It will hopefully be self-hosted soon, but is currently written in Idris. I'd also like to port it to C later. I'd like the type system to be based on classical linear logic with heavy use of the exponentials, which will simply translate to lots of clones at runtime, replacing a garbage collector and introducing natural in-place mutation. Lilypad will be intentionally unsafe: there will be special operations to avoid clones or cast to other types, and whatever else proves useful.

NOTE: this is for me, Ryan. It will evolve to fit the needs of the things I build with it, and no one else. I encourage others to build their own utility languages. I'd be thrilled if you take inspiration from lilypad or learn compilation techniques from it, but I won't recommend using it yourself.

Notes:

very minimal polymorphism, instead of unification (great for single-user projects like this): https://www.haskellforall.com/2024/02/unification-free-keyword-type-checking.html

fast tree transformations (this approach aligns with having lots of cloning): https://drops.dagstuhl.de/storage/00lipics/lipics-vol074-ecoop2017/LIPIcs.ECOOP.2017.26/LIPIcs.ECOOP.2017.26.pdf
