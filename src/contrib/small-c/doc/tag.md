Tags Explained
==============

In the context of writing a C compiler,
a *tag* is a specific kind of
identifier used primarily in the
declaration of structures, unions, and
enumerations to label these types.  Tags
facilitate the definition,
redeclaration, and referencing of these
compound data types throughout a C
program.

### Purpose and Functionality

#### 1. Definition and Declaration

* Structures and Unions: For `struct`
  and `union` types, a tag declares a
  new data type that allows the program
  to handle a physically grouped list of
  variables under one name.  This new
  compound data type can then be used to
  declare variables of this structured
  type.
  
  ```c
  struct point { int x; int y; };  // 'point' is a tag
  struct point p1, p2;
  ```

* Enumerations: For `enum`, a tag
  defines a set of named integer
  constants and introduces a new data
  type named by the tag.  This is used
  to handle variables that can hold any
  of the enumerated values.

  This version of Small-C does not yet
  support enums.

  ```c
  enum color { red, green, blue };  // 'color' is a tag
  enum color primary, secondary;
  ```

#### 2. Use in Type Declarations

Tags are used in declarations to specify
the type of other identifiers, which
means that once a tag has been defined
(e.g., a `struct` tag), it can be used
elsewhere in the program to declare more
variables of that type without needing
to redefine the type.

```c
struct point {
  int x;
  int y;
};

struct point makepoint(int x, int y) {
  struct point temp;

  temp.x = x;
  temp.y = y;
  return temp;
}
```

In the example above, `point` is used as a tag in the function `makepoint` to create and return a `struct point` type variable.

#### 3. Scoping of Tags

Tags have their own unique namespace,
meaning that the same tag name can be
used for different types (such as a
`struct` and an `enum`) without
conflict.  This is particularly useful
in large programs to avoid name
collisions in the global namespace.

```c
struct tag { int x; };   // 'tag' is a tag for a struct
enum tag { NEGATIVE, POSITIVE };  // 'tag' is also a tag for an enum

struct tag s1;
enum tag e1 = POSITIVE;
```

Here, `tag` is used as the name for both
a struct and an enum, which is allowed
because each type of compound data
(struct, union, enum) maintains its own
namespace for tags.

#### 4. Advantages of Using Tags

* Clarity and Maintainability: Tags help
  organize code clearly by associating
  variable declarations with visually
  identifiable names.
* Type Safety: Using tags with compound
  types enhances type safety by ensuring
  that only compatible types are
  assigned or operated upon.
* Abstraction: They allow for the
  abstraction of data handling, as the
  specifics of data structures can be
  encapsulated behind a type name.

### Compiler Implementation for Tags

* **Symbol Table Management**: Storing
  tags in a symbol table with
  appropriate scope and visibility
  considerations.
* Type Checking: Ensuring variables
  declared with these tags conform to
  the type definitions, including all
  operations and function calls.
* Code Generation: Correctly translating
  tagged types into the target machine
  code, taking into account alignment,
  size, and layout of data structures.
