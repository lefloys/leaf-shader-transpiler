/*!
** @file leaf/shader_layout.hpp
** @author lefloysi
*/
#pragma once

#include <cstdint>
#include <concepts>
#include <algorithm>
#include <tuple>
#include <iostream>
#include <glm/glm.hpp>
#include <type_traits>

#pragma region =[ Utility ]=

using u16 = std::uint16_t;
using u32 = std::uint32_t;
using cstr = const char*;

namespace lf {
	template<typename T> struct is_bitfield_enum : std::false_type {};
	template<typename T> constexpr bool is_bitfield_enum_v = is_bitfield_enum<T>::value;
	template<typename T> concept bitfield_enum = is_bitfield_enum<T>::value;
}

template<lf::bitfield_enum T> constexpr T operator~(T lhs) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(~static_cast<U>(lhs));
}
template<lf::bitfield_enum T> constexpr T operator&(T lhs, T rhs) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(static_cast<U>(lhs) & static_cast<U>(rhs));
}
template<lf::bitfield_enum T> constexpr T operator|(T lhs, T rhs) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(static_cast<U>(lhs) | static_cast<U>(rhs));
}
template<lf::bitfield_enum T> constexpr T operator^(T lhs, T rhs) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(static_cast<U>(lhs) ^ static_cast<U>(rhs));
}
template<lf::bitfield_enum T> constexpr T& operator&=(T& lhs, T rhs) noexcept { return lhs = lhs & rhs; }
template<lf::bitfield_enum T> constexpr T& operator|=(T& lhs, T rhs) noexcept { return lhs = lhs | rhs; }
template<lf::bitfield_enum T> constexpr T& operator^=(T& lhs, T rhs) noexcept { return lhs = lhs ^ rhs; }
template<lf::bitfield_enum T> constexpr T operator*(T lhs, bool b) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(static_cast<U>(lhs) * static_cast<U>(b));
}
template<lf::bitfield_enum T> constexpr T& operator*=(T& lhs, bool b) noexcept { lhs = lhs * b; return lhs; }

#pragma endregion

namespace lf {
	// ---------- Vertex attribute traits ----------
	template<typename T>
	struct VertexAttributeTraits;

	template<> struct VertexAttributeTraits<float> { static constexpr cstr glsl_type = "float"; static constexpr u32 byte_size = sizeof(float); static constexpr u32 component_count = 1; };
	template<> struct VertexAttributeTraits<glm::vec2> { static constexpr cstr glsl_type = "vec2"; static constexpr u32 byte_size = sizeof(glm::vec2); static constexpr u32 component_count = 2; };
	template<> struct VertexAttributeTraits<glm::vec3> { static constexpr cstr glsl_type = "vec3"; static constexpr u32 byte_size = sizeof(glm::vec3); static constexpr u32 component_count = 3; };
	template<> struct VertexAttributeTraits<glm::vec4> { static constexpr cstr glsl_type = "vec4"; static constexpr u32 byte_size = sizeof(glm::vec4); static constexpr u32 component_count = 4; };

	// ---------- Vertex attribute ----------
	struct VertAttr;
	template<typename T, typename Member> struct VertAttrTyped;

	struct VertAttr {
		cstr name;
		cstr type;
		u32 offset;
		u32 location;

		template<typename T, typename Member> static VertAttrTyped<T, Member> Make(cstr name, Member T::* member);
	};

	template<typename T, typename Member>
	struct VertAttrTyped {
		using vertex_t = T;
		using member_t = Member;

		VertAttrTyped(cstr name, Member T::* member)
			: member(member) {
			base.name = name;
			base.offset = static_cast<u32>(reinterpret_cast<std::uintptr_t>(&(reinterpret_cast<T const volatile*>(nullptr)->*member)));
			base.type = VertexAttributeTraits<Member>::glsl_type;
			base.location = 0; // placeholder, will be set by VertLayout
		}

		Member T::* member;
		VertAttr base;
	};

	template<typename T, typename Member>
	VertAttrTyped<T, Member> VertAttr::Make(cstr name, Member T::* member) {
		return VertAttrTyped<T, Member>(name, member);
	}

	// ---------- Vertex attribute layout ----------
	template<typename T, typename... Attrs>
	struct VertAttrLayoutTyped {
		using vertex_t = T;
		std::tuple<Attrs...> attrs;

		VertAttrLayoutTyped(Attrs... a) : attrs(std::make_tuple(a...)) {}

		template<typename Func>
		void for_each_attr(Func&& f) const {
			std::apply([&](auto const&... attr) { ((f(attr)), ...); }, attrs);
		}

		void print_info() const {
			for_each_attr([](auto const& attr) {
				std::cout << "Attribute: " << attr.base.name
					<< ", Type: " << attr.base.type
					<< ", Offset: " << attr.base.offset
					<< ", Location: " << attr.base.location
					<< "\n";
				});
		}
	};

	struct VertAttrLayout {
		template<typename T, typename... Attrs>
		static VertAttrLayoutTyped<T, Attrs...> Make(Attrs... attrs) {
			return VertAttrLayoutTyped<T, Attrs...>(attrs...); // pass parameter pack directly
		}
	};

	// ---------- Vertex layout ----------
	template<typename... Layouts>
	struct VertLayoutTyped {
		std::tuple<Layouts...> layouts;

		VertLayoutTyped(Layouts... layouts_) : layouts(std::make_tuple(layouts_...)) {
			assign_locations(); // assign global sequential locations
		}

		void print_info() const {
			std::apply([](auto const&... layout) {
				((std::cout << "Vertex: " << typeid(typename std::remove_reference<decltype(layout)>::type::vertex_t).name() << "\n",
					layout.print_info()), ...);
				}, layouts);
		}

	private:
		void assign_locations() {
			u32 current_location = 0;
			std::apply([&](auto&... layout) {
				((layout_for_each_attr(layout, current_location)), ...);
				}, layouts);
		}

		template<typename Layout>
		static void layout_for_each_attr(Layout& layout, u32& location_counter) {
			layout.for_each_attr([&](auto& attr) {
				const_cast<u32&>(attr.base.location) = location_counter++;
				});
		}
	};

	struct VertLayout {
		template<typename... Layouts>
		static VertLayoutTyped<Layouts...> Make(Layouts... layouts) {
			return { layouts... };
		}
	};
}

namespace lf {
	enum class descriptor_type : u16 {
		UniformBuffer,
		StorageBuffer,
		CombinedImageSampler,
		SampledImage,
		StorageImage,
		Sampler,
		EnumMax
	};


	enum class shader_stage_flags : u16 {
		none = 0,
		vertex_bit = (1 << 0),
		fragment_bit = (1 << 1),
		all = vertex_bit | fragment_bit,
	};
	template<> struct is_bitfield_enum<shader_stage_flags> : std::true_type {};

	struct DescBinding {
		cstr name;
		descriptor_type type;
		shader_stage_flags stage = shader_stage_flags::all;
		u32 count = 1;
		u32 binding = 0;

		static DescBinding Make(cstr n, descriptor_type t, shader_stage_flags s = shader_stage_flags::all, u32 c = 1) {
			return DescBinding{ n, t, s, c, 0 };
		}
	};

	template<typename... Bindings>
	struct DescSetLayoutTyped {
		std::tuple<Bindings...> bindings;
		u32 set_number = 0;

		DescSetLayoutTyped(Bindings... b) : bindings(std::make_tuple(b...)) {
			assign_binding_numbers();
		}

		template<typename Func>
		void for_each_binding(Func&& f) const {
			std::apply([&](auto const&... b) { ((f(b)), ...); }, bindings);
		}

		void print_info() const {
			std::cout << "Set: " << set_number << "\n";
			for_each_binding([](auto const& b) {
				std::cout << "  Binding: " << b.binding
					<< ", Name: " << b.name
					<< ", Type: " << static_cast<int>(b.type)
					<< ", Stage: " << static_cast<int>(b.stage)
					<< ", Count: " << b.count
					<< "\n";
				});
		}

	private:
		void assign_binding_numbers() {
			u32 current_binding = 0;
			std::apply([&](auto&... b) { ((const_cast<u32&>(b.binding) = current_binding++), ...); }, bindings);
		}
	};


	struct DescSetLayout {
		template<typename... Bindings>
		static DescSetLayoutTyped<Bindings...> Make(Bindings... b) {
			return DescSetLayoutTyped<Bindings...>(b...);
		}
	};

	template<typename... Sets>
	struct DescLayoutTyped {
		std::tuple<Sets...> sets;

		DescLayoutTyped(Sets... s) : sets(std::make_tuple(s...)) { assign_set_numbers(); }

		template<typename Func>
		void for_each_set(Func&& f) const { std::apply([&](auto const&... s) { ((f(s)), ...); }, sets); }

		void print_info() const { std::apply([](auto const&... s) { ((s.print_info()), ...); }, sets); }

	private:
		void assign_set_numbers() {
			u32 current_set = 0;
			std::apply([&](auto&... s) { ((s.set_number = current_set++), ...); }, sets);
		}
	};

	struct DescLayout {
		template<typename... Sets>
		static DescLayoutTyped<Sets...> Make(Sets... s) {
			return DescLayoutTyped<Sets...>(s...);
		}
	};


	template<typename Layout, cstr Name>
	struct DescLookup;

	template<cstr Name, typename... Bindings>
	struct DescLookup<DescSetLayoutTyped<Bindings...>, Name> {
	private:
		template<std::size_t Index = 0>
		static consteval u32 find_binding(const std::tuple<Bindings...>& b) {
			if constexpr (Index == sizeof...(Bindings)) {
				static_assert(Index < sizeof...(Bindings), "Descriptor name not found in set!");
				return 0;
			}
			else {
				if constexpr (std::string_view(std::get<Index>(b).name) == std::string_view(Name))
					return std::get<Index>(b).binding;
				else
					return find_binding<Index + 1>(b);
			}
		}

	public:
		static consteval u32 binding(const DescSetLayoutTyped<Bindings...>& s) {
			return find_binding(s.bindings);
		}
	};
}

namespace lf {
	template<typename VertLayout, typename DescLayout>
	struct PipelineLayoutTyped {
		VertLayout vertex_layout;
		DescLayout descriptor_layout;

		PipelineLayoutTyped(VertLayout v, DescLayout d)
			: vertex_layout(v), descriptor_layout(d) {}

		void print_vertex_info() const {
			vertex_layout.print_info();
		}

		void print_descriptor_info() const {
			descriptor_layout.print_info();
		}

		void print_info() const {
			print_vertex_info();
			print_descriptor_info();
		}

		std::pair<u32, u32> find_binding(cstr name) const {
			std::pair<u32, u32> result{ 0, 0 };
			bool found = false;

			descriptor_layout.for_each_set([&](auto const& set) {
				set.for_each_binding([&](auto const& b) {
					if (!found && std::strcmp(b.name, name) == 0) {
						result = { set.set_number, b.binding };
						found = true;
					}
					});
				});

			if (!found) throw std::runtime_error("Descriptor name not found!");
			return result;
		}
	};

	struct PipelineLayout {
		template<typename VertLayout, typename DescLayout>
		static PipelineLayoutTyped<VertLayout, DescLayout> Make(VertLayout v, DescLayout d) {
			return PipelineLayoutTyped<VertLayout, DescLayout>(v, d);
		}
	};
}

#ifdef __INTELLISENSE__
#define LEAF_SHADER_LAYOUT_IMPLEMENTATION
#endif
#ifdef LEAF_SHADER_LAYOUT_IMPLEMENTATION
#endif
