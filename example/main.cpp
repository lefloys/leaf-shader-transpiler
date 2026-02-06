#define LEAF_SHADER_IMPLEMENTATION
#include <leaf/shader.hpp>
#include <iostream>
#include <chrono>
#include <exception>

int main() {
	const char* source = R"(

in vec3 aPos;
layout(location=3) in vec4 aCo\
l;
out vec4 bCol; // Test comment
void foo () /* Test Block */ ;

void main() {
	bla;
	/* Test Block */

  }
void foo2 () 
{ // comment

}
    )";

	std::cout << "PreProcessed : \n" << source << "\n\n";

	// Start timer
	auto start = std::chrono::high_resolution_clock::now();
	std::string processed = lf::ProcessShader(source);

	// Stop timer
	auto end = std::chrono::high_resolution_clock::now();
	std::chrono::duration<double, std::milli> elapsed = end - start;

	std::cout << "Processed : \n" << processed << "\n\n";
	std::cout << "Processing took: " << elapsed.count() << " ms\n";
}
