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
- When walking the ast to generate HIR, mutually recursive with special return types and such. 

#### Complicated Memory Locations



#### Error Handling
Some discussion:
I was previously under the impression that error reporting should be added after the main functionality of the compiler was done, as to not interfere with the implementation or speed of the . This implementation would involve a few auxiliary maps from tokens or ast nodes to debug information. I was assuming that a compiler's primary job was to compile correct programs.

Now I am realizing that debug information and error reporting is much more essential than I thought. In C (this is a c compiler):
1) a majority of development time is spent debugging (so debug information is essential to include in an optimized way) and
2) More than half the invocations of the compiler will result in code that does not compile, it is not correct to accept that the rare case is an invalid program, instead the rare case is a correct program. 

So optimize for the common case of an incorrect program!, meaning include debug information in the types used in the various IRs of our compiler. 
