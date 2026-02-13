#define LEAF_SHADER_IMPLEMENTATION
#include <leaf/shader_transpiler.hpp>

#include <chrono>
#include <iostream>
#include <string>

static void print_header(const char* title) {
	std::cout << "\n============================================================\n";
	std::cout << title << "\n";
	std::cout << "============================================================\n";
}

int main() {
	try {
		const char* vertex_source = R"GLSL(
// Vertex shader (no explicit locations for varyings)
layout() in vec3 aPos;
in vec2 aTex;
in vec4 aCol;

out vec3 vPos;
out vec2 vTex;
out vec4 vCol;

void main() {
	vPos = aPos;
	vTex = aTex;
	vCol = aCol;
}
)GLSL";

		const char* fragment_source = R"GLSL(
// Fragment shader (no explicit locations for varyings)
in vec3 vPos;
in vec2 vTex;
in vec4 vCol;
out vec4 oColor;

void main() {
	oColor = vCol;
}
)GLSL";

		print_header("Original Vertex Shader");
		std::cout << vertex_source << "\n";

		print_header("Original Fragment Shader");
		std::cout << fragment_source << "\n";


		// 1) Link stages (assigns matching layout(location=...) to vert out / frag in)
		{
			print_header("LinkShaderStages (Vertex + Fragment)");

			auto start = std::chrono::high_resolution_clock::now();
			auto linked = lf::LinkShaderStages(vertex_source, fragment_source);
			auto end = std::chrono::high_resolution_clock::now();

			std::chrono::duration<double, std::milli> elapsed = end - start;

			print_header("Linked Vertex Shader");
			std::cout << linked[0] << "\n";

			print_header("Linked Fragment Shader");
			std::cout << linked[1] << "\n";

			std::cout << "\nLinkShaderStages time: " << elapsed.count() << " ms\n";
		}

		return 0;
	}
	catch (const std::exception& e) {
		std::cerr << "Fatal error: " << e.what() << "\n";
		return 1;
	}
}