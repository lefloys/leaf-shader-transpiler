#include <leaf/shader.hpp>
int main(){
    const char* source = R"(
 
in vec3 aPos;
layout(location=3) in vec4 aCo\
l;
out vec4 bCol; // Test comment
void main() {
/* Test Block */

}
        )";
    std::cout << source << "\n\n";
    std::cout << lf::ProcessShader(source) << "\n\n";
}
