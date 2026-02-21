# leaf-shader transpiler

Leaf Shader Transpiler is an ambitious project aimed at making GLSL shaders **deterministic and safe**.  

The goal is to **automatically insert explicit layout qualifiers** (`layout(location=...)`, `layout(binding=..., set=...)`)
into GLSL shaders, so that shader resource bindings and input/output locations are fully controlled by the C++ compiler.  

Rather than relying on runtime reflection or driver-assigned locations, **C++ definitions act as the source of truth**. 
The transpiler infers all layout numbers from your C++ code, ensuring that shaders are always synchronized with your application logic.  

---
## Why This Matters

In modern graphics programming, especially with Vulkan, shader-host synchronization is a persistent source of bugs and
developer frustration. Mismatched bindings, incorrect set numbers, or driver-assigned locations can silently break 
rendering or cause subtle runtime errors that are difficult to diagnose.  

Leaf Shader Transpiler addresses this by making the C++ application the **single source of truth** for all shader layouts.
All bindings, descriptor sets, and input/output locations are automatically inferred and defined at compile-time, and injected into GLSL at run-time.  

The result is:  
- **Compile-time safety**: errors are caught before the program even runs, preventing costly runtime debugging.  
- **Deterministic shader layouts**: no magic numbers, no driver surprises, fully predictable behavior across platforms.  
- **Improved developer productivity**: developers spend less time managing boilerplate and more time focusing on rendering logic.  

In short, this system turns one of the most error-prone parts of Vulkan development into a **safe, automated, and maintainable workflow**, 
giving engineers confidence and speed in shader development.

---
## Contributing

This is my **first public repo**, so contributions, suggestions, or feedback are **warmly welcome**.  

---
