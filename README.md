# leaf-shader transpiler

Leaf Shader Transpiler is an ambitious project aimed at making GLSL shaders **deterministic, safe, and backend-ready**.  

The goal is to **automatically insert explicit layout qualifiers** (`layout(location=...)`, `layout(binding=..., set=...)`) into GLSL shaders, so that shader resource bindings and input/output locations are fully controlled and consistent across platforms.  

Rather than relying on runtime reflection or driver-assigned locations, **C++ definitions act as the source of truth**. The transpiler infers all layout numbers from your C++ code, ensuring that shaders are always synchronized with your application logic.  

---

## Motivation

Modern graphics APIs like Vulkan and DirectX 12 require **explicit resource layouts**. OpenGL allows implicit locations and bindings, which is convenient but can be unpredictable and unsafe when targeting multiple backends.  

Leaf Shader Transpiler bridges this gap by providing:  

- Deterministic shader layouts, consistent across platforms  
- Automatic layout insertion based on C++ definitions  
- Preservation of original GLSL formatting and semantics  
- A foundation for multi-backend shader support  

---

## Vision

- **C++ as the source of truth**: developers define resource layouts in code, and the transpiler infers GLSL layout qualifiers automatically.  
- **Multi-backend support**: shaders remain valid GLSL, but can also be compiled for Vulkan, DX12, or other strict pipelines.  
- **Safe and scalable**: deterministic layouts eliminate magic numbers, collisions, and driver-specific surprises.  
- **Future-proof**: support for descriptor sets, automatic binding assignments, and complex shader pipelines.  

---

## Contributing

This is my **first public repo**, so contributions, suggestions, or feedback are **warmly welcome**.  

---
