# ManagedDotnetProfiler

Implementation of a .NET profiler in C# with NativeAOT, fork of work by [Kevin Gosse](https://github.com/kevingosse)

This fork uses an adapted version of [F#](https://github.com/dotnet/fsharp)'s 
[AbstractIL](https://github.com/dotnet/fsharp/tree/main/src/Compiler/AbstractIL) library to 
parse and transform the method bodies from the profile.

The adaptations to AbstractIL are necessary as, like most IL processing libraries, AbstractIL 
expects to process a whole assembly, where as profiler transformation must work with just the 
bytes from the method body.

Quite a lot of work is necessary to recover the tokens that are needed to insert into the IL
from the COM interfaces provided by the profiling API.

I find it easiest to test from two consoles, one to compile the native library, one to run 
the test app.


AOT compile the native library

```
cd ManagedDotnetProfiler
publish.cmd
```

Run the test app (compile via VS studio first).

```
cd TestApp\bin\Debug\net7.0
lanuch.cmd
```

