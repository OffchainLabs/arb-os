# Mini language tutorial

Mini is a programming language and compiler designed for writing code for the Arbitrum Virtual Machine (AVM) platform.  The language is simple but has some features customized for AVM.  This tutorial will tell you what you need to know to write Mini programs.

[This document lists potential changes or improvements to Mini in brackets like this.  Your feedback on these ideas is welcome.]

## Structure of a program

A Mini program is written as a set of source code files.  You can write separate source code files, and they will automatically be compiled and linked together to make an executable AVM program.

## Top level declarations

A Mini source code file consists of a series of top-level declarations.  
An optional file scope attribute declaration sets an attribute for the whole file.
Use declarations make functions and types from other files usable in the file they are contained in.
Constant declarations add a constant.
Use and constant declarations together form head declarations.
Body declarations are any other declaration.

The head declarations must come before all other declarations. It is an error to put a head declaration after a body declaration
If present, a file scope attribute declaration must be the first declaration in the file, and only one file scope attribute delcaration may be present.
See the Attributes section for more details.

Within each group the order of declarations does not matter.

### Head Declarations

`use` *path* `::` *name*;

> This imports the type or function with name *name* in the mini file with virtual path *path*. The code in the local file can refer to the type as *name*.
> Please see the section on path syntax for a description of how virtual paths work.

`const` *ident* `=` *unsignedInt* `;`

> This declares a constant with name *ident* and value *unsignedInt*. This constant is only usable in the file it is defined.

### Body declarations

`type` *name* (`<` *ident1*, *ident2*, ... `>`)? = *type* `;`

> This declares a type alias or generic type, allowing name to be used as a synonym for the specified type or as a generic type. 
> If `<` *ident1*, *ident2*, ... `>` is included, the type will be generic with *ident1*, *ident2*, etc as type variables.
> In generic types the type variables will be available in the definition of *type*, in any place a nominal type would be valid, and will be replaced by other types in specialization.
> See the Generics section for more details.

*Attributes*? `var` *name* `:` *type* `;`

> This declares a global variable. The variable will be initially assigned the default value for the type.
> Please see the Default Values and Attributes sections for more details. 

*Attributes*? [ `view` | `write` | `public` ]* `func` *name* ( *argname1: type1, argname2: type2, ...* ) [-> *returntype* | `noreturn`] *codeblock*

> This declares a function and provides its code.
> 
> The *Attributes* set the attributes for all instructions in the function.
>
> The `public` modifier indicates that the function can be called by code outside this source code file. Non-public functions cannot be called directly by outside code.  (However, pointers to non-public functions can be passed to outside code, and this would allow the pointed-to function to be called by outside code.)
>
> The `view` modifier indicates that the function reads global state or calls other `view` functions. It is possible to create a `view` function that does not read global state, but the compiler throw a warning in this case.
>
> The `write` modifier indicates that the function writes to global state or calls other `write` functions. It is possible to create a `write` function that does not read global state, but the compiler throw a warning in this case. 
>
> Any duplicate modifiers are ignored.
> 
> The arguments are treated as local variables within the function, so code in the function can read them or assign to them.
>
> If there is a return type specified, the function will return a single value of the specified type. (We'll see below that the type can be a tuple, allowing multiple values to be packaged together into a single return value.)
> 
> If no return type is specified, then the function returns `void`, as void represents a lack of a value, this means no value is returned.
>
> If the return type is `every`, then the function cannot return, because no return value of type `every` can exist. If the compiler detects a return of any other type, it will not be able to verify that it cannot return, and it will generate an error.
>
> Declaring a function as `noreturn` is equivalent to declaring that the function returns `every`. 
> 
> Please see the Attributes section for more information about attributes.

## Global State

Global state is state that can be accessed anywhere in the system, this consists of global variables, system gas, the inbox, the log, the error codepoint, and the register.

`View` operations consist of: calling a `view` function, reading a global variable, getting the system gas, and calling an inline assembly expression that uses any `view` instructions.

`Write` operations include: calling a `write` function, assigning to a global variable, setting the system gas, and calling an inline assembly expression that uses any `write` instructions

Instructions that are both `view` and `write` are
`inbox`, `inboxpeek`, `pushinsn`, `pushinsnimm`, `sideload`, `jump`, `cjump`, `auxpop`, and `auxpush`.

`Write` instructions that are not `view` are `log`, `send`, `rset`, `errset`, and `setgas`.

`View` instructions that are not `write` are `rpush`, `errcodepoint`, `errpush`, and `pushgas`.


## Types

Mini is a type-checked language.  The compiler should catch any inconsistent use of types. We believe the only ways that type errors can go undetected by the compiler are incorrect uses of the `unsafecast` or `unioncast` operators, or `asm` expressions.

Mini has the following types:

`bool`

> `true` or `false` (an atomic type with the zero value `false`)

`uint`

> a 256-bit unsigned big-endian integer.

`int`

> a 256-bit signed (twos complement) big-endian integer.

`bytes32`

> a string of 32 bytes.

`address`

> a 20-byte Ethereum address, can hold any value between `0` and `2^160-1`.
 
`buffer`

> Buffers are a type that contains an series of bytes.
> Buffers internally can be of length up to 2^128-1, however, as both the `setbuffer` and `getbuffer` expressions may only handle indices
> up to `2^("system register size in bits")-1`, this limitation does not impact maximum size unless the hardware running the system has a bit width over 128.

`void`

> Represents a lack of a type, it is the implicit return type of functions with no return type.
> This type can cause bugs when embedded in other types.

`string`

> Represents a string of text. Internally, this is represented by `(uint, buffer)`, with the `uint` representing the length of the string, and the `buffer` representing the raw data.

`(` *type1*`,` *type2*`,` ... `)`

> a tuple, consisting of zero or more ordered, typed fields (a compound type)

`[` *size* `]` *type*

> a fixed-size array of values, all of the same type; *size* must be a constant unsigned integer (a compound type)

`[]` *type*

> an array of values, all of the same type (a compound type)

`map` `<` *type* `,` *type* `>`

> a hash map, which maps keys of one type to values of another type

`union<` *type1*`,` *type2*`,` ... `>`

> an untagged union type that can hold values from any of *type1*, *type2*, etc. There must be explicit casts to convert to and from its component types. This is done via `newunion` and `unioncast`.

`struct` `{` *name1* `:` *type1* `,` *name2* `:` *type2* `,` ... `}`

> a struct with one or more named, typed fields (a compound type)

`option` `<` *type* `>`

> Either `Some(`*value*`)` or `None<`*type*`>`, where *value* represents an arbitrary value of type *type*. 
> The specific variant being used can be determined by `if let` and `?`.
> The shortcut `None` can be used to create a value of `None<every>`, which is assignable to all `None<`*T*`>` for any type *T*.
> The term "inner type" refers to the type specified by *type*.

[ `view` | `write` | `public` ]* `func` ( *type1, type2, ...*) (`->` *returntype*)?

[ `view` | `write` | `public` ]* `closure` `(` *type*, *type*, ... `)` (`->` *returntype*)?

> A reference to a function, while `public` can be included a type declaration, it has no effect on the internal type.
> The use of `public` may be removed in the future, so it is recommended to not use it. 
> While `view`, `write`, and `public` may each be used multiple times, the duplicate instances are ignored.
> The ability to use duplicates of `view`, `write`, and `public` may be removed in the future.

*ident*

> A nominal type or a type variable. It can only be treated as a type variable in the context of a generic type or function,
> and only if that identifier is listed as one of the type variables of that generic type or function. 
> Otherwise, it is treated as a nominal type. If that nominal type is not defined in the file or is not imported with a use statement, 
> it is not a valid type and will cause an error.
> We define the representation of a nominal type to be the type on the right side of the type declaration with the same name as *ident*.
> The representation of a nominal type impacts the assignability rules of the type among other areas.

*ident* `<` *type1* `,` *type2* `,` ... `>`

> A specialization of a generic type. *ident* must be the name of some generic type, *type1*, *type2*, ... must be types,
> and there must be the same number of types listed as there are type variables of *ident*.
> We define the representation of a specialized generic type to be the type on the right side of the generic type declaration,
> with *type1*, *type2*, etc, replacing each type variable of *ident* in order of their definition.

`any`

> Can hold values of arbitrary type.

`every`

> A type that has no values. No value of this type can ever exist.

### Generics

Generic types and generic functions should be thought of as different objects than regular types and functions.
In particular, unless a generic type is specialized it can not be used as a type, and unless a generic function is specialized, it cannot be called or used as a value.
It is important to distinguish between unspecialized generics and generics specialized by type variables, for example consider this example mini program:
```rs
type queue<T> = option<struct {
    item: T,
    rest: queue<T>,
}>;

func main() {
    return;
}

func queue_new<T>() -> queue<T> {
    return None;
}

func single_queue<T>(item: T) -> queue<T> {
    return Some(struct {
        item: item,
        rest: queue_new::<T>(),
    });
}
```
In this context, `queue<T>` is a generic type that is specialized by the type variable `T`, and `queue` by itself is a
non-specialized generic type. Also, `queue_new::<T>` is a reference to a generic function specialized with a type variable,
where `queue_new` is a non-specialized generic function. 
It is not permitted for `queue` by itself to be used as a type nor is `queue_new` by itself permitted to be used as a function.
 
Type variables are deeply tied with generic types and generic functions, we introduced these in the Types section,
and we will expand on their use here.
A type variable represents an arbitrary type, that will be replaced with a specific type via the process of specialization.
Because they represent arbitrary types, their rules for equality, assignability, and castability must be such that they can be replaced with any concrete type and still produce valid assignments and casts.
Therefore, they are equal, assignable, and castable to themselves, can be assigned or casted to any, and can be assigned or casted from every,
as these actions can be done with all types.

Specialization can occur in two different contexts, in the context of a specialized generic type, and a reference to a specialized generic function.
In both contexts the process operates similarly, with each use of a type variable being replaced with a different specified type. 
And the primary difference between the two specializations are the scopes under which it applies.

For generic types, a specialization creates a new specialized generic type, for example `queue<uint>` would be a specialization of `queue`.
The type `queue<uint>` will be given a representation equal to the type on the right side of the type declaration, with each type variable replaced by the associated type.
For example, `queue<uint>` will have a representation of:
```rs
option<struct {
    item: uint,
    rest: queue<uint>,
}>
```
A specialization with type variables is also possible in contexts where type variables are present. For example, `queue<U>` would have representation:
```rs
option<struct {
    item: U,
    rest: queue<U>,
}>
``` 
This is important to distinguish from non specialized generics, as `queue<U>`, `queue<T>` and `queue<queue<T>>` are all different types, where there is only one non-specialized `queue`.

The other type of specialization is the specialization of generic functions.
A generic function like `single_queue` can be specified with a type to produce a specified generic function.
For example `single_queue::<uint>` is a reference to `single_queue` specialized by `uint`.
The type of the function obtained by specialization is a function type, which has `view` and `write` if and only if the generic function does,
whose argument types are the arguments with each type variable replaced by the specified type, and whose return type is the return type of the generic function, with each type variable replaced by the type specified.
For example the type of `single_queue::<uint>` is `func(uint) -> queue<uint>`.
 
The code executed by a specialized generic function is identical to the code that would be generated by replacing every type variable in the unspecialized function with the type specified by the specialization. 
However, this transformation is not necessary, as the rules for type variables ensure that no type specific logic is necessary, and the code for the non-specialized generic function can be safely used.
The practical impact of this is that there is no possibility for a type error originating from inside the body of a specialized generic function, as any type errors will be caught in the non-specialized function.
Therefore, as long as a correct generic function name is used, the correct number of types is specified, and each type is valid in the scope the specialized generic function is being used, no errors should occur within a specialized generic function reference.

These constructs define the extent of the generics system in mini.

## Equality, assignability, and castability for types

Two types are equal if they have the same structure. Nominal types, as defined by type declarations, create types that are considered equal to their definitions. 
(For example, after the declaration `type foo = uint`, `foo` and `uint` are considered equal, and `uint` is considered the representation of `foo`.)

Every atomic type is equal to itself. 
This includes `uint`, `int`, `bool`, `buffer`, `bytes32`, `address`, `void` and `string`.

Two tuple types are equal if they have the same number of fields and their field types are equal, field-by-field.

Two fixed-size array types are equal if they have the same size and their field types are equal.

Two variable-sized array types are equal if their field types are equal.

Two struct types are equal if have the same number of fields, each field has the same name and equal type, field-by-field.

Two func types are equal if they have the same purity, in other words, that the presence or non-presence of `view` and `write` modifiers is the same in both, 
the same number of argument types, and each argument type is equal, argument-by-argument, and the return types are equal (or neither has a return type).

Two map types are equal if their key types are equal and their value types are equal.

Two option types are equal if their inner types are equal

Two union types are equal if they have the same number of variants and their variants are equal, variant by variant.

Two specialized generic or nominal types are equal if their representations are equal.

Two type variables are equal if they share the same name.
It is also necessarily the case that they are defined in the same location, as any generic type or function must be specialized to be used.

`any` equals itself.

`every` equals itself.

Unless specified as equal by the rules above, a pair of types is unequal.

### Assignability

A value of type `V` is assignable to storage of type `S` if:

* `S` is `any` and `V` is not of type `void`, or
* `V` equals `S`,
* `V` and `S` are tuple types with the same number of fields, and each field of `V` is assignable to the corresponding field of `S`,
* `V` and `S` are fixed-size arrays of the same size, and the field type of `V` is assignable to the field type of `S`,
* `V` and `S` are arrays, and the field type of `V` is assignable to the field type of `S`,
* `V` and `S` are structs, with the same number of fields, and each field of `V` has the same name as the corresponding field of `S`, and each field of `V` is assignable to the corresponding field of `S`,
* `V` and `S` are function types, with the same number of arguments, and if `V` is `view` or `write`, then `S` must also be `view` or `write` respectively, each argument type of `V` must be assignable to the corresponding argument type of `S`, and the return type of `S` is assignable to the return type of `V`.  (Note that the return type is compared for assignability "backwards". This is needed to make calls through function references type-safe.)
* `V` and `S` are map types, and the key type of `V` is assignable to the key type of `S`, and the value types of `V` and `S` are equal.
* `V` and `S` are optional types, and the inner type of `V` is assignable to the inner type of `S`
* `V` and `S` are union types with the same number of variants, and each variant of `V` in order must be assignable to each variant of `S`, variant-by-variant.
* `V` and `S` are nominal types or specialized generic types, and the representation of `V` is assignable to the representation of `S`.

These rules guarantee that assignability is transitive. 

The compiler uses often uses type inference to infer the types of variables from the types of values assigned to them.  If a programmer wants the compiler to infer a different type, they should use an explicit type-casting operation to convert the value to the desired type.

### Castability

A value of type `V` is castable to storage of type `S` if:

* `S` is `any` and `V` is not of type `void`,
* `V` is assignable to `S`,
* `V` is `every`,
* `V` is `bool` and `S` is one of `bool`, `address`, `bytes32`, `uint`, or `int`,
* `V` is `address` and `S` is one of `address`, `bytes32`, `uint`, or `int`,
* `V` is one of `bytes32`, `uint`, or `int` and `S` is one of `bytes32`, `uint`, or `int`,
* `V` is `void` and `S` is `void`,
* `V` is `buffer` and `S` is `buffer`,
* `V` and `S` are tuple types with the same number of fields, and each field of `V` is castable to the corresponding field of `S`,
* `V` and `S` are fixed-size arrays of the same size, and the field type of `V` is castable to the field type of `S`,
* `V` and `S` are variable-sized arrays, and the field type of `V` is castable to the field type of `S`,
* `V` and `S` are structs, with the same number of fields, and each field of `V` is castable to the corresponding field of `S`, the names of each field do not need to match,
* `V` and `S` are function types, with the same number of arguments, and if `V` is `view` or `write`, then `S` must also be `view` or `write` respectively, each argument type of `V` must be castable to the corresponding argument type of `S`, and the return type of `S` is castable to the return type of `V`.  (Note that the return type is compared for castability "backwards". This is needed to make casts through function references type-safe.),
* `V` and `S` are map types, and the key type of `V` is castable to the key type of `S`, and the value type of `V` is castable to the value type of `S`,
* `V` and `S` are optional types, and the inner type of `V` is castable to the inner type of `S`,
* `V` and `S` are union types with the same number of variants, and each variant of `V` is castable to each variant of `S` variant-by-variant,
* `V` or `S` is a nominal or specialized generic type, and the representation of `V` is castable to the representation of `S`.

## Codeblocks

Codeblocks are used in a number of different constructs in mini, however there are actually two distinct kinds of codeblocks.
In general, only one of these two kinds may be used for a given construct, therefore it is necessary to describe the distinction between these variants.

`{` [*statement*]* `}`

> This is a statement codeblock, it is a sequence of zero or more statements, enclosed in curly braces.  Statement codeblocks form the bodies of functions, `if` and `loop` statements, among other constructs.  Local variables may be declared and used within a codeblock. A local variable that is declared within a codeblock can be used only within that same codeblock (or other codeblocks nested inside of it). 

`{` [*statement*]* *expression* `}`

> This is an expression codeblock, the statements are executed in order, with the same behavior as statement codeblocks, and then *expression* is evaluated, with access to locals defined in *statement*s. 
> The type of the codeblock is the same as the type of *expression*.

The major distinction in usage between the two types is that statement codeblocks cannot be used as an expression, and cannot return a value.
So in contexts where a value is needed, expression codeblocks are generally used, and in contexts where a return value isn't needed,
statement contexts are generally used.

## Statements

All statements may be prefixed by *Attributes*, see the Attributes section for more details.

*codeblock*

> The statement codeblock *codeblock* is executed, any variables declared inside the codeblock will only be accessible inside the codeblock.

`loop` *codeblock*

> An "infinite loop" which executes the statement codeblock *codeblock* repeatedly. The only way to exit the loop is via a `return` statement or a runtime error.

`while` ( *condition* ) *codeblock*

> Like a loop, except *condition*, which must be an expression of type `bool`, is evaluated before each iteration, and the loop terminates if *condition* is found to be false.
> *codeblock* must be a statement codeblock.

`if` ( *condition* ) *codeblock* ([`else` [*elsecodeblock* | *if statement* | *if let statement*]])?

> *condition* must be a expression of type `bool`. If *condition* is true then the statement codeblock *codeblock* is executed, otherwise, the `else` statement is executed if present. *elsecodeblock* must be a statement codeblock. You can string together as many else ifs as you want.

`let` *name* = *expression* ;

> Create a new local variable and initialize it with the value of *expression*.  
> The compiler infers that the new variable has the same type as *expression*.  
> The variable goes out of scope when execution leaves the current codeblock.  
> If the new variable has the same name as an already-existing variable, it will mask the existing variable definition for as long as the new variable is in scope.
> The type of *expression* cannot be *void*, or a compilation error will occur.

`let` ( *nameorbinding1* , *nameorbinding2*, ... ) = *expression* ;

Where each nameorbinding may be:

*identifier*

Or:

`*`*identifier*

> Creates or assigns to multiple variables based on unpacking a tuple. 
> If the *nameorbinding* is an identifier it creates a new variable, if *nameorbinding* is `*`*identifier* it assigns to an existing variable with that name. 
> *expression* must be a tuple type, with the number of fields in the tuple equal to the number of names on the left-hand side.
> The compiler creates a new local variable for each name on the left-hand side, and infers the type of each new variable based on the type of the corresponding field of the right-hand side tuple.
> In the case of assignments to existing variables, the corresponding field on the right hand side must be assignable to the type of the variable.
> No assignment may use type *void* on the right hand side.

`if let` `Some(`*ident*`)` `=` *expression* *codeblock* [`else` [*elsestatementcodeblock* | *if statement* | *if let statement*]]?

> It is required that *ident* is an identifier, and *expression* is an expression of some option type.  
> 
> If *expression* returns the Some variant, at the start of the statement codeblock *codeblock* a new local variable *ident* is created with the inner value of the result of *expression*,
> and type of *expression*'s inner type. *codeblock* is then run with *ident* present.
> 
> If *expression* is the `None` variant and the `else` statement is present, 
> statement codeblock *elsestatementcodeblock*, if statement *if statement*, or if let statement *if let statement* is run instead.  

*expression*

> The expression is executed, this is primarily useful when the side effects of *expresssion* are desired, but the either the expression does not return a value or the value is not needed.

*name* = *expression* ;

> Assign a new value to an existing variable, which can be a global variable or a local variable. The type of *expression* must be assignable to the variable's type.

`return` ;

> Return from the current function. This is an error if the function has a return type other than `void`.

`return` *expression* ;

> Return a value from the current function. The value of *expression* must be assignable to the function's return type.

`debug` `(` *expression* `)`

> Prints the result of *expression* to the user via the debugprint pipeline.
 
`assert` `(` *expression* `)`

> The type of *expression* must be `(bool,` *type*`)`, where *type* may be any type.
> Asserts are only evaluated when the program has been compiled in debug mode, otherwise *expression* will not run.
> If the `bool` is `true`, then execution continues as normal, but if `bool` is `false`, 
> then execution will throw an error, with the value in the second field of the tuple being displayed via a debugprint instruction.
> In the rust emulator this means that it will be printed to standard out with a prefix of `debugprint: `.

`set` *nameident* ([`.` *fieldident* | `[` *keyexpression* `]` ])+ `=` *expression* `;`

> Sets the field, map, or array element specified by the sequence of *fieldident*s and *keyexpression*s, to *expression*.
> All *keyexpressions*s that index into an array or fixed size array type must be of type `uint`.
> For *keyexpressions*s that index into a map, the type of the *keyexpression* must be assignable to the key type of the map.

## Attributes

`#[` *ident* (`,` *ident*)* `]`

Attributes set certain properties of the object they are attached to. The only possible attributes are `breakpoint` and `print`,
any other value for *ident* will cause an compilation error.
Attributes will apply to instructions generated by the file, function, or statement they are applied to, as well as any children of that object.
The `breakpoint` attribute creates a breakpoint that halts execution in the rust debugger,
and the `print` attribute causes the compiler to print the index and instruction details during the "postlink_compile" stage of compilation.

A file scope Attribute Declaration is identical to other attribute declarations except it must use a `!` after the `#`:

`#![` *ident* (`,` *ident*)* `]`

## Expressions

There are many types of expressions, which we'll catalog here.   Operator precedence works roughly as expected.

Mini never automatically converts types to make an operation succeed.  Programmers will need to do explicit conversions. The compiler will report an error if a conversion would be necessary.

`-` *expression*

> Unary minus. Defined only for type `int`, and produces an `int`. This will error if the result is not expressible in the `int` datatype. This will only occur if the value of *expression* is `MaxNegInt`.

`!` *expression*

> Logical negation, defined only for type `bool`, produces a `bool`.

`~` *expression*

> Bitwise negation.  Defined for numeric types and `bytes32`. Produces a result of the same type as the operand.

*expression* `?`

> The containing function must return either an option type or `any`, and *expression* must be an option type. If *expression* is the `Some` variant, then it evaluates as the inner value, otherwise, this will cause the function to return `None`.

*expression* `+` *expression*

*expression* `-` *expression*

> Addition and subtraction.  Both operands must have the same numeric type, and the result is of that same type. These do 256-bit arithmetic and do not check for overflow or underflow.

*expression* `*` *expression*

*expression* `/` *expression*

*expression* `%` *expression*

> Multiplication, integer division, and modulo. Both operands must have the same numeric type, and the result is of that same type. Multiplication does 256-bit arithmetic and does not check for overflow or underflow.  Division and modulo error if the second operand is zero.

*expression* `<` *expression*

*expression* `>` *expression*

*expression* `<=` *expression*

*expression* `>=` *expression*

> Numeric comparisons. Both operands must have the same numeric type. The result has type `bool`.

*expression* `==` *expression*

*expression* `!=` *expression*

> Equality comparison, checking that value have the same AVM representation. The result has type `bool`.
> 
> Please see [the AVM spec](https://github.com/OffchainLabs/arbitrum/blob/master/docs/AVM_Specification.md) in particular the `eq` opcode `0x14`,
> and the AVM Representation section for more information.

*expression* `&` *expression*

*expression* `^` *expression*

*expression* `|` *expression*

> Bitwise and, xor, and or.  Both operands must have the same numeric type, and the result will have that same type.

*expression* `&&` *expression*

*expression* `||` *expression*

> Logical and, and logical or.  Both operands must have type `bool`, and the result has type `bool`.  Execution will shortcut, so that the second expression is evaluated only if the outcome is still in doubt after evaluating the first expression.

*leftexpression* `>>` *rightexpression*

> Right shift operator, it shifts the bits of *leftexpression* right by *rightexpression* bits.
> *leftexpression* and *rightexpression* must be expressions of type `uint`. When a right shift occurs, the leftmost *rightexpression* bits are filled by 0s.
> If both *leftexpression* and *rightexpression* are constants, then *leftexpression* may be of type `int` or `bytes32`.

*leftexpression* `<<` *rightexpression*

> Left shift operator, it shifts the bits of *leftexpression* left by *rightexpression* bits.
> *leftexpression* and *rightexpression* must be expressions of type `uint`. When a left shift occurs, the rightmost *rightexpression* bits are filled by 0s.
> If both *leftexpression* and *rightexpression* are constants, then *leftexpression* may be of type `int` or `bytes32`.

`uint` `(` *expression* `)`

`int` `(` *expression* `)`

`bytes32` `(` *expression* `)`

`address` `(` *expression* `)`

> Type conversions. The operand must be a numeric type. The result type is per the operator name.  Conversion to `address` truncates the operand value to the 20 lowest-order bytes.

`len` `(` *expression* `)` 

> Get the length of *expression*, whose value must be a variable sized array.  Result is a `uint`.

`hash` `(` *expression* `)`

`hash` `(` *expression* `,` *expression* `)`

> Compute the hash of the value(s).  The single-argument version can take a value of any type.  The two-argument version requires both arguments to be `bytes32`. Both produce a `bytes32`.
>
> [Improvement needed: Currently the one-argument hash  just hashes the underlying AVM representation of the value. This will cause some values of different types to have equal hashes.  It seems better to guarantee that two values test as equal (using ==) if and only if they have the same hash. To do that, we would need to generate code that incorporates a typecode of some sort into the hash.]
>
> [Possible improvement: For some user-defined data structures, the "representation hash" approach we use here won't make sense.  We might approach this by adding a "nohash" modifier to types.  Attempts to hash an object whose type had the nohash modifier would generate an error. Or possibly we want a "nocompare" modifier which would prohibit both equality comparisons and hashing.  Note that the underlying implementation uses hashing to do comparisons of non-atomic types, so values are comparable if and only if they are hashable.]
>
> [Likely improvement: Eliminate the two-argument hash, on the rationale that the programmer can always make a tuple and hash that using the single-argument hash. Applying the single-argument hash to general values is a simpler and equally expressive mechanism.]

`struct` `{` *name1* `:` *expression1* `,` *name2* `:` *expression2* `,` ... `}`

> Create a new `struct` value. The types of the `struct` fields are inferred from the types of the expressions. Returns a `struct` value whose specific type is determined by the sequence of names and expression types given.

`(` *expression1* `,` *expression2*`,` ... `)`

> Create a new tuple value, whose type will be inferred from the number and types of the expressions.

`newarray` `<` *type* `>` `(` *expression* `)`

> Create a new variable-sized array object. *expression*, which must have type `uint`, gives the initial size of the new array, and *type* is the type of its elements. The contents of the array are initialized to the default value of *type*.

`newfixedarray` `(` *size* `)`

> Create a new fixed-array of `any` elements. *size*, which must be a constant `uint`, is the size of the new array.
> The values of the array are initialized as values of `()`. The type of this expression is `[`*size*`]any`.

`newfixedarray` `(` *size* `,` *expression* `)`

> Create a new fixed-size array, with every slot initialized to the value of *expression*. *size*, which must be a `uint` constant, is the size of the new array. The element type is inferred from the type of *expression*.

`newmap` `<` *keytype* `,`  *valuetype* `>`

> Create a new map object of type `map<`*keytype*`,` *valuetype*`>`, initially empty.
 
`cast` `<` *type* `>` `(` *expression* `)`

> Casts the result of *expression* to *type*. This is a safe operation, unlike `unsafecast` or `unioncast`. See the casting subsection for more information.
> If the type of *expression* cannot be safely cast to *type*, then a compile error will occur.

`unioncast` `<` *type* `>` `(` *expression* `)`

> Converts from a type of `union<`*type1*`,` *type2*`,` ... `>` to *type*, where *type* must be a member of *type1*, *type2*, ... . This is an unsafe operation, as which type the union actually contains is not checked. 

`unsafecast` `<` *type* `>` `(` *expression*  `)`

> Evaluate *expression*, and then treat the in-memory representation of the result as an object having type *type*. This is an unsafe operation, as the programmer must ensure that the value being cast can be safely interpreted as *type*.  It is most often used to convert a value of type `any` into a more specific type, when the programmer knows the real type of the value.  

`Some` `(`*expression*`)`

> Creates an option typed value with the `Some` variant, the inner value is the result of *expression* and inner type is equal to the type of *expression*.

`None`

> Creates an optional value of type `option<every>` with no inner value.
 
`None` `<` *type* `>`

> Creates an optional value of type `option<` *type* `>` with no inner value.

`newunion` `<` *type1*`,` *type2*`,` ... `>` `(` *expression* `)`

> Creates a value of type `union<`*type1*`,` *type2*`,` ... `>` from an *expression* of any of *type1*, *type2*, ... .

*arrExpression* `[` *indexExpression* `]`

> Get an element of an array.  *arrExpression* must have type `[]`*T* or `[`*N*`]`*T* for some type *T* and some non negative integer *N*.  *indexExpression* must have type `uint`. The access is bounds-checked, and will throw a runtime error if the index is outside the bounds of the array. The result of this expression has type *T*.

*mapExpression* `[` *keyExpression* `]`

> Get a value from a map.  *mapExpression* must be a map type. *keyExpression*, which must be assignable to the map's key type, gives the key to look up in the map. The result, is an `option<`*valuetype*`>` where *valuetype* is the type of the values of *mapExpression*. 
> This expression will return a `None` if there is not a value associated with the key, or `Some(`*value*`)` if there is a value *value* associated with the result of *keyExpression* in the result of *mapExpression*.

*expression* `.` *name*

> Access a field of a struct.  The type of *expression* must be a struct that has a field called *name*. The result has the type of that field.

*expression* `.` *number*

> Access a field of a tuple.  *expression*, which must be an expression that has a tuple type, gives the tuple the field is taken from. *number*, which must be a constant `uint`, specifies which field number to access.  (The first field is number zero) The type of *expression* must have more than *number* fields. The result has the type of that field.

*funcExpression* `(` *argExpression1* `,` *argExpression2* `,` ... `)`

> A function call. *funcExpression* must be an expression that returns a function type. (Typically *funcExpression* will just be the name of a function.) The number of *argExpressions* must be consistent with the number of arguments in *funcExpression*'s type, and each *argExpression* must be assignable to the type of the corresponding argument of *funcExpression*'s type.  The result has the type of *funcExpression's* return value.

*arrayExpression* `with` `{` `[` *indexExpression* `]` `=` *valExpression* `}`

> Create a new array by copying an existing array with one element modified.  *arrayExpression*, which must be an array type, specifies the array to start with. *indexExpression*, which must have type `uint`, specifies which slot in the array should be modified.  *valExpression*, whose type must be assignable to the element type of the array, is the new value to put into the slot.  The result has the same type as *arrayExpression*. If the index is out of bounds, this will cause either a compile-time error or a runtime error.  

*mapExpression* `with` `{` `[` *keyExpression* `]` `=` *valExpression* `}`

> Create a new map by copying an existing map with one element added or modified. *mapExpression*, which must be a map type, specifies the map to start with. *keyExpression*, which must be assignable to the map's key type, specifies the key to be added or modified. *valExpression*, which must match the map's value type, is the new value to be associated with the key.  The result has the same type as *mapExpression*.

*structExpression* `with` `{` *name* `:` *valExpression* `}`

> Create a new struct by copying an existing struct with one field modified.  *structExpression*, which must be a struct type with a field called *name*, specifies the struct to start with. *name* is an identifier specifying the name of the field to modify. *valExpression*, which must have a type assignable to the named field, is the value that will be assigned to the named field in the newly created struct. 

`false`

`true`

> Literal values. `false` and `true` are of type `bool`.

*number*

*number*`s`

> An integer constant, in either decimal format or hexadecimal format, it is hexadecimal if it starts with "0x". This will be interpreted as a `uint`, and it must be representable as a `uint`. Most importantly, it must not overflow the maximum value for the type. If a decimal number is followed by the single character `s`, it is interpreted as a signed integer `int`; in this case it must be representable as an `int`. And thus must not overflow or underflow the maximum and minimum values for this type.

*name*

> A reference to a local variable, a global variable, or a function. It will have the type of the variable or function in scope with name *name*.

`asm` `(` *expression1* `,` *expression2* `,` ... `)` (*type*)? `{` *instructions* `}`

> Escape to assembly code.  The arguments, *expression1*, *expression2*, etc., if present, are pushed onto the AVM stack, with *expression1* at the top of the stack. 
> Then the *instructions*, which are a sequence of AVM assembly instructions, are executed.  
> The compiler assumes that the assembly instructions consume the arguments, and if *type* is present, leave a single value on the stack of type *type*, 
> otherwise, it is assumed to leave no extra values on the stack. If *type* is present, *type* is the type of the expression. Otherwise, the expression is type `void`.

`if` *condition* *codeblockexpr* `else` [*elsecodeblockexpr* | *ifexpression* | *ifletexpression*]

> *condition* must be a expression returning *bool*. 
> If *condition* returns `true` then codeblock expression *codeblockexpr* is executed and its value is returned. 
> If *condition* returns `false`, then either the codeblock expression *elsecodeblockexpr*, if expression *ifexpression* or if let expression *ifletexpression* is returned, based on which is present.
> The type of *elsecodeblockexpr*, *ifexpression*, or *ifletexpression* must be assignable to  *codeblockexpr*, and the type of the whole expression is the type of *codeblockexpr*.

`if let` `Some(`*ident*`)` `=` *expression* *clodeblockexpr* `else` [*elseblockexpr* | *ifexpr* | *ifletexpr*]

> It is required that *ident* is an identifier, and *expression* is an expression of some option type.
> If *expression* returns the `Some` variant of an option type, a new local variable *ident* is created with the inner value of *expression* at the start of *codeblockexpr*,
> and *codeblockexpr* is evaluated and returned.  If *expression* is the `None` variant then *elseblockexpr*, *ifexpr* or *ifletexpr* is evaluated and returned.  
> The types of *elseblockexpr*, *ifexpr* or *ifletexpr* must be assignable to *codeblockexpr*. 
> The expression returns the type of *codeblockexpr*.

`loop` (`<` *type* `>`)? *statementcodeblock*

> Executes the statements in *statementcodeblock* repeatedly until a return statement is encountered.
> The type *type* determines the type of the loop expression if present. 
> 
> [The current compiler does not support break statements, but they may be added in the future. In which case a loop may break with a break statement of corresponding type]

`newbuffer` `(` `)`

> Creates a new `buffer` with zero length.

`getbuffer8` `(` *offset* `,` *buffer* `)`

`getbuffer64` `(` *offset* `,` *buffer* `)`

`getbuffer256` `(` *offset* `,` *buffer* `)`

> Gets the byte, 8 bytes, or 32 bytes at index *offset* of *buffer* respectively, *offset* must be an expression returning `uint`
> and *buffer* must be an expression of type `buffer`.
> If *offset* evaluates to a value greater than `2^("system register size in bits)-1`, `2^("system register size in bits")-8`, and `2^("system register size in bits")-32` respectively, the expression will throw a runtime error.
> This is a `uint` expression.

`setbuffer8` `(` *offsetexpr* `,` *valueexpr* `,` *bufferexpr* `)`

`setbuffer64` `(` *offsetexpr* `,` *valueexpr* `,` *bufferexpr* `)`

`setbuffer256` `(` *offsetexpr* `,` *valueexpr* `,` *bufferexpr* `)`

> Return the buffer created from setting the byte, 8 bytes, or 32 bytes at *offsetexpr* respectively,
> to *valueexpr* in the buffer *bufferexpr*. *offsetexpr* and *valueexpr* must both be `uint` expressions,
> and *bufferexpr* must be a `buffer` expression. 
> If *offsetexpr* evaluates to a value greater than `2^("system register size in bits)-1`, `2^("system register size in bits")-8`, and `2^("system register size in bits")-32` respectively, the expression will throw a runtime error. 
> This is a `buffer` expression.

`any` `(` *expression* `)`

> Casts the type of the expression *expression* to *any*. This is always safe.

[ `view` | `write`]* [`closure` | `_closure` ]  `(` *ident1* `:` *type1* `,` *ident2* `:` *type2* `,` ... `)` (`->` *returntype*)? *codeblockexpr*

> Creates a new closure, that takes arguments *ident1*, *ident2*, ... of type *type1*, *type2*, ... respectively, and a return type of *returntype* if present, or `void` otherwise.
> The body of the closure is defined by codeblock expression *codeblockexpr*.
> If `_closure` is used then the compiler will not warn if the closure is unused.
> This function returns type `func(`*type1*`,` *type2*`,` ...`) ->` (either *returntype* or *void* if not present).

`getGas` `(` `)` 

> Returns the remaining AVM gas left in the system as a `uint`. The expression is of type `uint`.

`setGas` `(` *expression* `)`

> The type of expression *expression* must be `uint`. The remaining AVM gas for the system is set to the value of *expression*. This is a `void` expression.

*ident* `::` `<` *type1*, *type2*, ... `>`

> A reference to a specialization of a generic function. *ident* must be a generic function name, 
> *type1*, *type2*, ... must all be types,
> and there must be the same number of *type*s specified as there are type variables on *ident*.
> The function reference is treated as the type resulting from substitution of each type variable with the concrete types listed in *type1*, *type2* etc.
 
`error`

> Causes the system to error, moving to the error handler. This expresion has type *every*.

`"` [a-zA-Z0-9_ .,:?'<+>()!@#$%^&*|~\\/-]* `"`

> Creates a string literal. The type of this string literal is `string`.

`"` (`0x`)? [a-fA-F0-9]* `"`

> Hex string literal, of type `string`.

`{` (*statement*)* (*expression*)? `}`

> Codeblock expression. The *statement*s are executed in order, 
> and all locals defined within the codeblock are only valid within the codeblock.
> If *expression* is present, the result of *expression* with equivalent type. 
> Otherwise, the codeblock returns *void*.

`(` *expression* `)`

> Equivalent to *expression*.

## Path syntax

*ident* (`::` *ident*)*

> This represents a path with the leftmost identifier being the widest scope.
> In particular, a valid path may contain a file name followed by a identifier,
> or it may be a library specifier, either `std` or `core`, followed by a file name and identifier.
> In the case of a file name followed by a identifier, the file name represents the mini file within the source folder with that name.
> So if `"arb_os"` is the compile target `name` would represent `arb_os/name.mini`, and the identifier would represent the function or type of the same name in that file.
> If it starts with a library prefix, the base folder is determined by the prefix, with
> `std` corresponding to the `std` folder and `core` corresponding to the `builtin` folder.
> The second and third parts of the path work the same as mentioned previously.
> A path represented in this format will be called a virtual path, to distinguish it from paths in use by the filesystem.

## Shadowing

In mini, new variables can be created with the same name as old variables, this is called shadowing.
To prevent ambiguity when dealing with shadowed variables, a reference to the shadowed name is treated as referring to
the most recently declared variable that is currently in scope. So if a variable is declared inside a codeblock with the same name as another variable outside the codeblock,
references to the name inside the codeblock will refer to the inner variable, and references to the name after the end of the codeblock refer to the outer variable.

## AVM Values

An AVM value may be one of the following:

1. `Int`: a 256 bit integer
1. `CodePoint`: a codepoint, points to an instruction in the code.
1. `Buffer`: a buffer that contains bytes.
1. `Tuple(`...`)`: up to 8 AVM values.

More complex values are usually encoded by storing multiple values inside a `Tuple`, included nested tuples.

## Default Value

Every type has a default value that is used for the initial value of globals with that type.
The default values for each type are as follows:

* `uint`, `int`, `bytes32`, `address`:  
  `0`, represented in `AVM` as an `Int`.
* `bool`:  
  `false`, this is also a `0` `Int` in the underlying AVM.
* `buffer`:  
  A buffer with length `0`, attempting to read any bytes past the end of a buffer will be treated as reading `0` values.
* `any`:  
  A length `0` tuple, `()`.
* `union`:  
  The default value of a union type is the default value of its leftmost variant.
* `tuple`:  
  The default value of a tuple is a wrapping tuple, with leaves equal to the default values of each field type respectively.
  The structure of a wrapping tuple will be described below in the Wrapping Tuple section.
* `struct`:  
  The default value of a struct is a wrapping tuple, with leaves equal to the default values of each field type respectively.
* `option`:  
  The default value of all `option` types is the `None` variant, in AVM this is represented as a length 1 tuple containing an `Int` value `0`
* `func`:  
  The default value of all `func` types is a `CodePoint` 
* `map`:  
  An empty map, in AVM this is represented as a length 2 `Tuple` with both fields as an `Int` with value `0`.
* `array`:  
  An `array` of *type* has a default value with the following AVM representation:
  `Tuple(Int(1), Int(1), Tuple(Int(1), Int(1), Tuple(`*default*, *default*, *default*, *default*, *default*, *default*, *default*, *default*`)))`
  Where *default* represents the default value for type *type*. 
  This is intended to be an array with length 1 containing the default value of *type*, however in practice it has an additional outer 3 `Tuple` surrounding it.
* `fixed array`:  
  A default value for a `fixed array` of type *type* and length *length* is a fixed array of length *length*, where every array element has a value of the default value of *type*.
  See the AVM representation section for more information of how the memory for this value is layed out.
* `nominal`, `specialized generic`:  
  The default value for a nominal or specialized generic type is the default value of its representation.

While non-specialized generic types should never be created, if they are they will have a default value of `()`.

## AVM Representation

Types in mini are internally represented as AVM values.

1. `uint`:  
   Always an `Int`, any `Int` value is allowable for this type.
1. `int`:  
   Always an `Int`, any `Int` value is allowable for this type. 
   These `Int`s are interpreted in twos complement notation to derive any integer value from `-2^255` to `2^255-1`.
1. `bool`:  
   Always an `Int`, the values `0` and `1` are allowed for this type, but nothing else.
   The value `0` is interpreted as `false`, and `1` as `true`.
1. `buffer`:  
   Always a `Buffer`, any value is allowable for this type.
1. `bytes32`:  
   Always an `Int`, any value is allowable for this type.
1. `address`:  
   Always an `Int`, only values from `0` to `2^160-1` are allowed for this type.
1. `void`:  
   Represents lack of a value, whenever a value of type `void` is present, then it is assumed there is no AVM value there.
   This manifests as a bug in various contexts.
1. `struct`:  
   Represented by a wrapping tuple containing values of the types of each of its fields, in order from top to bottom.
   Field names have no impact on the allowable AVM values for the type.
1. `tuple`:  
   Identical to structs, represented by a wrapping tuple containing values of the types of each of its fields, in order left to right.
1. `unsized array` of *type*:  
   The depth of an unsized array is the smallest integer `x` such that 8^`x` >= *length*, with a minimum depth of `1`, where *length* is the current length of the array.
   An array of depth `N` has a value of `Tuple(Int, Int,` a `Tuple` of length 8 of sized arrays of depth *N-1* `)`, where depth `0` represents an AVM value valid for *type*.
   The first `Int` in the outer tuple is the *length* of the array, and the second integer is `8^(N-1)`.
1. `sized array` of *type* and length *length*:  
   A series of nested 8 tuples, each leaf of which contains a AVM value valid for *type*.
   The nested tuples are at uniform depth, and are the minimum depth such that there are enough slots for *length* values.
   The minimum depth is a single 8 tuple. Sized arrays of length 0 allocate a single tuple with 8 slots.
   
   The leaves of a depth *N* tuple are ordered, starting with 0, by concatenating the ordering of each *N-1* depth tuple from left to right,
   in the case of a depth `1` tuple, the tuple fields are ordered left to right.
   
   The AVM value located at index *n* corresponds to the item in the array at index *n*, with any *n* >= *size* not intended for access.
   These values are initialized as the default value for *type*, and can generally be assumed to contain those values.
1. `map` with keys of *keytype* and values of *valuetype*:  
   All maps consist of `Tuple(` *KvsNode* `, Int)`, where the *KvsNode* holds the key value pairs, and the `Int` corresponds to the size of the map, ie, how many key value pairs are present. 
   The valid values for *KvsNode* will be explained below.
   A *KvsNode* may be one of 3 types, it may be an integer of value `0`, a `Tuple` of length 2, or a `Tuple` of length 8.
   In the case of a 2 `Tuple`, the first value in the tuple must be valid for *keytype*, and the second must be valid for `option<`*valuetype*`>`.
   In the case of an 8 tuple, each value in the tuple must be a valid *KvsNode*.
   These correspond to mini values in the following way. `0` represents a branch with no key value pairs stored, the 2 `Tuple` represents a single key value pair, and an 8 `Tuple` represents
   an intermediate *KvsNode* containing 8 other *KvsNode*s.
   The location of a specific key value pair is determined by the hash of the key, cast to a `uint`, we will refer to this has as *hash*. See the `hash` expression for more details on how this is calculated.
   The specific tuple field of a value inserted into a map is `(`*hash*`/8^`*depth*`)` `% 8`, where *depth* is the number of subtrees above the current one.
   Intuitively, this checks the lowest three bits of *hash* at the top level, and at each subsequent level moves 3 bits to the left.
   An 8 `Tuple` will always be present when at least two keys in the map share a path to that location in the tree.
1. `string`:  
   As a string is equivalent to `(uint, buffer)`, the valid AVM values are those values that are valid for `(uint, buffer)`,
   in other words all values must be a `Tuple(Int, Buffer)`. Any possible value for this representation is allowable,
   although strings are always initialized such that `Int` represents the length of the `Buffer`.
   The contents of the buffer are the string contents interpreted as utf8. 
1. `any`:  
   Any AVM value may be used for any, the compiler makes no assumptions about the value of this type.
1. `option<`*type*`>`:  
   Either a length 1 tuple containing `0`, which corresponds to the mini value `None<`*type*`>`,
   or a length 2 tuple where the first field is `1` and the second field is a valid value *value* for *type*, this corresponds to `Some(`*value*`)`.
1. `every`:  
   There are no valid AVM values for this type, the compiler assumes it will never exist.
1. `union<`*type1*`,` *type2*`,` ... `,` *typeN*`>`:  
   The valid AVM values for this type are the union of valid values for *type1*, *type2*, ... , *typeN*. 
   In other words, an AVM value is valid for this type if and only if there is some type in *type1*, *type2*, ... , *typeN* for which the value is valid.
   It is not in general possible to determine the exact mini value that a value of a `union` type derives from.
   When downcasting to a specific variant, the AVM value is unchanged, and it is treated according to the rules of that specific variant.
1. `func`:  
   All function types are represented internally as a `CodePoint` pointing to the start of the function in memory.
1. `nominal`/`specialized generic`:  
   A nominal type has the same AVM representation as its representation.
   The representation of a nominal type is the type on the right half of the type declaration.
   This also applies to specialized generic types, where the AVM representation is equivalent to that of the type resulting from
   replacing each type variable with the concrete type specified in the specialization.

## Wrapping tuple

Mini uses the concept of a wrapping tuple to express tuples and structs of length greater than 8. These are expressed by nesting `Tuple`s
inside of other types, in order to allow more "slots" to fit in a single value.
The structure of a wrapping tuple is a single `Tuple` from length 0 to 8.
If the length is 9 or greater, the base of the wrapping tuple of length *length*, is a nested `Tuple` of uniform depth *N*.
Where *N*, is the greatest integer such that `8^`*N* `<=` *length*. A nested tuple of depth 1 is a single length 8 `Tuple`,
and for all greater *N* it is a length 8 `Tuple` whose fields are all depth *N*`-1` tuples.

The order of the fields in a nested `Tuple` is the left to right order in a depth 1 tuple, and in a length *N* tuple,
it is the order of the fields in each depth *N*`-1`, concatenated in left to right order.

A wrapping tuple of length *length* is a depth *N* nested tuple, where *N* is the greatest integer that `8^`*N*`<=`*length*,
where, if `8^`*N*`=/=`*length*, the last *j* fields excepting the final field replaced by 8 `Tuple`s, 
where *j*`=⌊(length-8^`*N*`)/7⌋` and the last field replaced by a tuple of length *k*`=((length-8^`*N*`)%7)+1`, 
unless *k*`=1`, in which case the last field is replaced by a length *8* `Tuple`. 
If `8^`*N*`=`*length*, the wrapping tuple is simply a depth *N* nested tuple.