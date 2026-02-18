#define LEAF_SHADER_LAYOUT_IMPLEMENTATION
#include <leaf/shader_layout.hpp>
#define LEAF_SHADER_TRANSPILER_IMPLEMENTATION
#include <leaf/shader_transpiler.hpp>

struct Vertex1 {
	glm::vec3 pos;
	glm::vec2 uv;
	glm::vec4 color;

	static inline auto layout = lf::VertAttrLayout::Make<Vertex1>(
		lf::VertAttr::Make("aPos", &Vertex1::pos),
		lf::VertAttr::Make("aTex", &Vertex1::uv),
		lf::VertAttr::Make("aCol", &Vertex1::color)
	);
};

struct Vertex2 {
	glm::vec2 a;
	glm::vec3 b;

	static inline auto layout = lf::VertAttrLayout::Make<Vertex2>(
		lf::VertAttr::Make("a", &Vertex2::a),
		lf::VertAttr::Make("b", &Vertex2::b)
	);
};
inline auto vertex_layout = lf::VertLayout::Make(Vertex2::layout, Vertex1::layout);

static inline auto set0 = lf::DescSetLayout::Make(
	lf::DescBinding::Make("u_state", lf::descriptor_type::UniformBuffer, lf::shader_stage_flags::vertex_bit, 1),
	lf::DescBinding::Make("bar", lf::descriptor_type::CombinedImageSampler, lf::shader_stage_flags::fragment_bit, 1)
);

static inline auto set1 = lf::DescSetLayout::Make(
	lf::DescBinding::Make("foo", lf::descriptor_type::StorageBuffer, lf::shader_stage_flags::all, 4),
	lf::DescBinding::Make("zoo", lf::descriptor_type::SampledImage, lf::shader_stage_flags::fragment_bit, 1)
);

// Global shader descriptor layout
static inline auto shader_desc_layout = lf::DescLayout::Make(set0, set1);


inline auto pipeline_layout = lf::PipelineLayout::Make(vertex_layout, shader_desc_layout);

int main() {
	pipeline_layout.print_info();

	auto [set, binding] = pipeline_layout.find_binding("foo");
	std::cout << "set " << set << ", binding " << binding << "\n";
	return 0;
}
