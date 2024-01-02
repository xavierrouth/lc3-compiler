## TODOs:


#### Variable Declaration (HIR, Analysis)
Fix VarDecl, right now this type represents globals, locals, and parameters, split these up better and make it better!.. 


#### AST Structure
Am wondering if it is worth splitting up AST nodes into separate types, to further enforce some type-safety / optimize some match statements. An example of waht this would look like is instead of a binary expression taking two ASTNodes as children, it would take specialized ASTExpressionNodes. This means we don't have to check that. 

Some things to note, case statements are only really inefficient if their switch arms are not consecutive. In our case the match statement is switching over enum tags, Rust most likely will make this integers in order. I am not sure if there is any mesurable performance difference between matching on 3 statements vs matching over 100. 

So performance improvement is basically out, the other thing to consider is will this help type-safety? 

Even though we aren't ever constructing these nodes ourselves, if the AST is wrong doesn't necessarily mean the input program was wrong. This could also mean that our parsing logic is wrong! and the added type safety would've allowed us to catch a parsing / type (input program level types) error earlier, or notice a bug in our compiler. 

it will add a lot more boilerplate to our code that interacts with the AST. 

Related Case Study:
- When walking the AST to generate HIR, mutually recursive with special return types and such. 
- I forget what i was going to say here.


#### Complicated Memory Locations
I forget what I was going to say here.





#### String Allocations
Quite difficult to manage string allocations when printing intermediate representations. Not really worried about this because you shouldn't really be printing these things that often? 

#### AST -> HIR
There is some friction when writing code to lower a recursive data structure into a non recursive one. Y

#### HIR -> LIR 
There is some friction when writing code to lower a 


This is a complete failure, I hate it, it is so messy. 
#### HIR Invariants

#### LIR Invariants
I want rust's type system & borrow checker to help enforce these, but the use of an arena makes that very difficult, but thats okay! Maybe it's meant to be hard, rust isn't magic after all.



#### Slotmaps, ownership, and encapsulation
We are working with graph-like data structures in our IRs and AST. The obvious and correct choice is to use arenas / slotmaps to handle this. Our IR consists of layers, 

Program (translation unit basically) -> Function -> Basic BlockData -> Instruction.

For the Program -> Function layer, we simply use reference counted smart pointers to handle the graph instead of arenas, this may change later. 

For the Function -> Basic BlockData -> Instruction layer, we have arenas / slotmaps. 

Function owns the arenas for Basic Blocks and instructions, this allows merging Basic Blocks and other things we probably want to do. 


These leaves us with two real options for the structs below the split, 
    - 1) Implement methods on the parent struct (Function) who owns the arenas, that take handles as input and do useful things. 
    - 2) Implement methods taking self for BlockData and Instruction, and give them a reference to their parent Function in order to do useful things.

Comparison:
1 is slightly awkward from a function signature standpoint, functions meant to operate on Blocks need to have their names prepended with block_ or something, same with instructions. 

2 requires a RefCell to Function probably, which has runtime overhead. 

So after choosing to implement 1, there is so design / api things want to figure out. 

- No internal state on LIR about a 'current block' or whatever, this should be left to a builder or lirgen. 

- Do we want to accept handles, or the backing structs themselves, or both. Often we want to do repeated operations on the same handle, and probably want to avoid computing its hash through slotmap every time. In 2) this is easy, as it is the struct itself already and not a handle, but in 1) it is harder. Will the borrow checker even let us pass the struct itself? 

#### Error Handling / Source Location Info
Some discussion:
I was previously under the impression that error reporting should be added after the main functionality of the compiler was done, as to not interfere with the implementation or speed of the . This implementation would involve a few auxiliary maps from tokens or ast nodes to debug information. I was assuming that a compiler's primary job was to compile correct programs.

Now I am realizing that debug information and error reporting is much more essential than I thought. In C (this is a c compiler):
1) a majority of development time is spent debugging (so debug information is essential to include in an optimized way) and
2) More than half the invocations of the compiler will result in code that does not compile, it is not correct to accept that the rare case is an invalid program, instead the rare case is a correct program. 

So optimize for the common case of an incorrect program!, meaning include debug information in the types used in the various IRs of our compiler. 
