## A Typeful Binding Environment

`Record` is a **type-indexed, key-aware environment** for Scala 3. It provides a *precise, type-safe representation of binding contexts* that generalizes and subsumes many common programming structures.

Where typical `Map` collections flatten keys and values to common types, `Record` retains **the exact keys and their corresponding types at compile time**. This enables far more precise reasoning, safer APIs, and a foundation for more principled language and program design.

### Analogy with HList

Just as **HList (or Scala 3's `Tuple`) is a more typeful and precise version of List**, `Record` is a more typeful and precise version of Dict (or `Map` in Scala).
While List flattens all element types to a common supertype, `HList` tracks the exact type of each element. Similarly, while Dict tracks only the key and value *types* at runtime, `Record` preserves the **exact keys and their corresponding types** at compile time.
In this sense, `Record` is to Dict what HList is to List: a **fully typed version of a dictionary** that enables more type-safe, expressive, and generic programming.


### Motivation

In most languages:

* **Tuples** track position but discard names.
* **Maps** track names but collapse value types.
* **Environments** (in interpreters, type checkers, etc.) are typically untyped or loosely typed.

`Record` unifies these structures:

* Keys and their types are tracked **at the type level**.
* The structure of the environment is **part of the type**.
* It allows **fine-grained compile-time reasoning** about the presence and type of each binding.

This generalizes:

* Tuples → special case: keys as integers corresponding to positions
* Positional arguments → special case: keys as integers
* Keyword arguments → natural fit: keys as symbols/IDs
* Records → environments with statically known keys
* Function scopes, configuration sets, etc.
* Variable environments

### Relation to Scala 3 Named Tuples

`Record` is what [Scala 3 named tuples](https://dotty.epfl.ch/docs/reference/other-new-features/named-tuples.html) should have been.


### Key Features

* **Type-Safe Lookup:** The compiler knows which keys are present and what types their values have.
* **Precise Keys:** Keys are tracked at the type level, not just as runtime values.
* **Extensible:** Can model function arguments, records, type environments, and more.
* **Composable:** Environments can be combined and manipulated with guaranteed structural integrity.


### Why Record Matters

By recognizing that *positional arguments, keyword arguments, records, named tuples, and function environments* are **all special cases of `Record`**, we can:

* Unify language design around a smaller, more principled core
* Improve type safety by tracking key structures explicitly
* Enable more expressive, verifiable, and composable programs


### Status

This project is in early development. Contributions, feedback, and discussion are welcome!
