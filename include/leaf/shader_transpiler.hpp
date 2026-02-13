/*
** leaf/shader.hpp
*/
#pragma once
#include <cctype>
#include <cstdint>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>
#include <array>

#include <glm/glm.hpp>

#ifdef __INTELLISENSE__
#define LEAF_SHADER_IMPLEMENTATION
#endif

namespace lf {
	using u32 = std::uint32_t;

	template<typename T>
	struct VertexAttributeTraits;

	template<>
	struct VertexAttributeTraits<glm::vec2> {
		static constexpr const char* glsl_type = "vec2";
		static constexpr u32 component_count = 2;
		static constexpr u32 byte_size = sizeof(glm::vec2);
	};

	template<>
	struct VertexAttributeTraits<glm::vec3> {
		static constexpr const char* glsl_type = "vec3";
		static constexpr u32 component_count = 3;
		static constexpr u32 byte_size = sizeof(glm::vec3);
	};

	template<>
	struct VertexAttributeTraits<glm::vec4> {
		static constexpr const char* glsl_type = "vec4";
		static constexpr u32 component_count = 4;
		static constexpr u32 byte_size = sizeof(glm::vec4);
	};

	struct VertexAttributeInfo {
		const char* name;
		const char* glsl_type;
		u32 offset;
		u32 size;
		u32 component_count;
	};

	struct VertexLayout {
		const char* vertex_type_name;
		u32 stride;
		u32 attribute_count;
		const VertexAttributeInfo* attributes;

		const VertexAttributeInfo& operator[](u32 index) const {
			return attributes[index];
		}
	};

	template<typename VertexType, typename MemberType>
	struct AttributeDescriptor {
		const char* name;
		MemberType VertexType::* member_ptr;

		constexpr AttributeDescriptor(const char* n, MemberType VertexType::* ptr)
			: name(n), member_ptr(ptr) {}

		VertexAttributeInfo to_info() const {
			return {
				name,
				VertexAttributeTraits<MemberType>::glsl_type,
				static_cast<u32>(
					reinterpret_cast<const char*>(&(static_cast<const VertexType*>(nullptr)->*member_ptr)) -
					reinterpret_cast<const char*>(static_cast<const VertexType*>(nullptr))
				),
				VertexAttributeTraits<MemberType>::byte_size,
				VertexAttributeTraits<MemberType>::component_count
			};
		}
	};

	template<typename VertexType, typename MemberType>
	constexpr auto attribute(const char* name, MemberType VertexType::* ptr) {
		return AttributeDescriptor<VertexType, MemberType>(name, ptr);
	}

	template<typename VertexType, typename... Attributes>
	struct VertexLayoutTyped {
		static constexpr u32 attribute_count = sizeof...(Attributes);
		VertexAttributeInfo attributes_storage[attribute_count];

		VertexLayoutTyped(Attributes... attrs)
			: attributes_storage{ attrs.to_info()... } {}

		VertexLayout to_layout() const {
			return VertexLayout{
				typeid(VertexType).name(),
				static_cast<u32>(sizeof(VertexType)),
				attribute_count,
				attributes_storage
			};
		}

		operator VertexLayout() const {
			return to_layout();
		}

		const VertexAttributeInfo& operator[](u32 index) const {
			return attributes_storage[index];
		}
	};

	template<typename VertexType, typename... Attributes>
	auto make_vertex_layout(Attributes... attrs) {
		return VertexLayoutTyped<VertexType, Attributes...>(attrs...);
	}

	struct SimpleVertex {
		glm::vec3 pos;
		glm::vec2 tex;
		glm::vec4 col;

		static inline auto layout = make_vertex_layout<SimpleVertex>(
			attribute("aPos", &SimpleVertex::pos),
			attribute("aTex", &SimpleVertex::tex),
			attribute("aCol", &SimpleVertex::col)
		);
	};

	template<typename T>
	std::string InjectVertexLayout(std::string_view source);

	std::array<std::string, 2> LinkShaderStages(std::string_view vert_source, std::string_view frag_source);

	std::string ProcessShader(std::string_view source);
}

#ifdef LEAF_SHADER_IMPLEMENTATION

namespace lf {
	using uchar = unsigned char;

	enum class TokenType : uchar {
		cComment,       // // or /* */
		cComma,         // ,
		cSemicolon,     // ;
		cWhitespace,    // space/newline/tab
		cParenOpen,     // (
		cParenClose,    // )
		cBraceOpen,     // {
		cBraceClose,    // }
		cBracketOpen,   // [
		cBracketClose,  // ]
		cSlash,         // /
		cAssign,        // =

		kwBinding,      // binding
		kwBuffer,       // buffer
		kwIn,           // in
		kwLayout,       // layout
		kwLocation,     // location
		kwOut,          // out
		kwSet,          // set
		kwUniform,      // uniform

		lNumber,        // 123
		Identifier,
		EndOfFile,
		EnumMax,
	};

	enum class NodeType : uchar {
		Blob,
		Binding,
		Declaration,
		FunctionBody,
		FunctionDecl,
		FunctionArgs,
		Identifier,
		LayoutQualifier,
		Literal,
		Location,
		StorageQualifier,
		TranslationUnit,
		TypeSpecifier,
		Set,
		EndOfFile,
		EnumMax,
	};

	struct Token {
		Token() = default;
		Token(TokenType t, const std::string& c) : type(t), content(c) {}
		Token(TokenType t, std::string&& c) : type(t), content(std::move(c)) {}
		Token(TokenType t, uchar c) : type(t), content(1, static_cast<char>(c)) {}

		TokenType type{};
		std::string content;
	};

	using TokenStream = std::vector<Token>;

	struct Node {
		NodeType type{};
		std::string value;
		std::vector<Node> children;
		std::string leadingWhitespace;
	};

	struct Lexer {
		explicit Lexer(const std::string& source)
			: content(source) {}

		uchar peek(size_t steps = 0) const {
			const size_t idx = cursor + steps;
			if (idx >= content.size()) { return '\0'; }
			return static_cast<uchar>(content[idx]);
		}

		uchar next() {
			if (cursor >= content.size()) { return '\0'; }
			return static_cast<uchar>(content[cursor++]);
		}

		void skip(size_t amount) {
			cursor += amount;
			if (cursor > content.size()) cursor = content.size();
		}

		bool eof() const {
			return cursor >= content.size();
		}

		size_t cursor = 0;
		const std::string& content;
	};

	struct Parser {
		const Token& peek(size_t offset = 0) const {
			if (cursor + offset >= tokens.size()) throw std::runtime_error("Unexpected EOF");
			return tokens[cursor + offset];
		}

		const Token& next() {
			if (cursor >= tokens.size()) throw std::runtime_error("Unexpected EOF");
			return tokens[cursor++];
		}

		const Token& peek_significant(size_t num = 0) const {
			size_t idx = cursor;
			size_t sigCount = 0;

			while (idx < tokens.size()) {
				const Token& tok = tokens[idx];
				if (tok.type != TokenType::cWhitespace && tok.type != TokenType::cComment) {
					if (sigCount == num) return tok;
					++sigCount;
				}
				++idx;
			}

			throw std::runtime_error("No significant token found");
		}

		const Token& next_significant() {
			while (cursor < tokens.size()) {
				const Token& tok = next();
				if (tok.type != TokenType::cWhitespace && tok.type != TokenType::cComment) {
					return tok;
				}
			}
			throw std::runtime_error("No significant token found");
		}

		bool eof() const {
			return peek_significant().type == TokenType::EndOfFile;
		}

		size_t cursor = 0;
		const TokenStream& tokens;
	};

	struct Emitter {
		const Token& next() {
			if (cursor >= stream.size()) throw std::runtime_error("Unexpected EOF in emitter");
			return stream[cursor++];
		}

		size_t cursor = 0;
		const TokenStream& stream;
	};

	using LexFn = void(*)(Lexer&, TokenStream&);
	using ParseFn = void(*)(Parser&, Node&);
	using SerializeFn = void(*)(const Node&, TokenStream&);
	using EmitFn = void(*)(Emitter&, std::stringstream&);

	static const std::unordered_map<std::string, TokenType> keywords = {
		{ "binding", TokenType::kwBinding },
		{ "buffer", TokenType::kwBuffer },
		{ "in", TokenType::kwIn },
		{ "layout", TokenType::kwLayout },
		{ "location", TokenType::kwLocation },
		{ "out", TokenType::kwOut },
		{ "set", TokenType::kwSet },
		{ "uniform", TokenType::kwUniform },
	};

	namespace detail {
		static void lex_assign(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cAssign, lexer.next()); }
		static void lex_brace_close(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cBraceClose, lexer.next()); }
		static void lex_brace_open(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cBraceOpen, lexer.next()); }
		static void lex_bracket_open(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cBracketOpen, lexer.next()); }
		static void lex_bracket_close(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cBracketClose, lexer.next()); }
		static void lex_comma(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cComma, lexer.next()); }
		static void lex_paren_open(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cParenOpen, lexer.next()); }
		static void lex_paren_close(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cParenClose, lexer.next()); }
		static void lex_semicolon(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cSemicolon, lexer.next()); }

		static void lex_whitespace(Lexer& lexer, TokenStream& tokens) {
			if (std::isspace(lexer.peek())) tokens.emplace_back(TokenType::cWhitespace, lexer.next());
		}

		static void lex_number(Lexer& lexer, TokenStream& tokens) {
			std::string num;
			while (std::isdigit(lexer.peek())) num.push_back(static_cast<char>(lexer.next()));
			tokens.emplace_back(TokenType::lNumber, std::move(num));
		}

		static void lex_identifier(Lexer& lexer, TokenStream& tokens) {
			std::string id;
			while (std::isalnum(lexer.peek()) || lexer.peek() == '_') id.push_back(static_cast<char>(lexer.next()));

			auto it = keywords.find(id);
			const TokenType type = (it != keywords.end()) ? it->second : TokenType::Identifier;
			tokens.emplace_back(type, std::move(id));
		}

		static void lex_slash(Lexer& lexer, TokenStream& tokens) {
			const uchar next = lexer.peek(1);
			if (next == '/') {
				lexer.skip(2);
				std::string comment = "//";
				while (!lexer.eof() && lexer.peek() != '\n') comment.push_back(static_cast<char>(lexer.next()));
				tokens.emplace_back(TokenType::cComment, std::move(comment));
				return;
			}

			if (next == '*') {
				lexer.skip(2);
				std::string comment = "/*";
				while (!lexer.eof()) {
					const char c = static_cast<char>(lexer.next());
					comment.push_back(c);
					if (c == '*' && lexer.peek() == '/') {
						comment.push_back(static_cast<char>(lexer.next()));
						break;
					}
				}
				tokens.emplace_back(TokenType::cComment, std::move(comment));
				return;
			}

			tokens.emplace_back(TokenType::cSlash, lexer.next());
		}

		static constexpr auto build_lex_table() {
			std::array<LexFn, 256> table{};
			for (auto& fn : table) {
				fn = [](Lexer& lexer, TokenStream&) {
					throw std::runtime_error(std::string("Unexpected character: '") + static_cast<char>(lexer.peek()) + "'");
					};
			}

			table[static_cast<uchar>('\t')] = lex_whitespace;
			table[static_cast<uchar>('\n')] = lex_whitespace;
			table[static_cast<uchar>('\r')] = lex_whitespace;
			table[static_cast<uchar>(' ')] = lex_whitespace;

			table[static_cast<uchar>('(')] = lex_paren_open;
			table[static_cast<uchar>(')')] = lex_paren_close;
			table[static_cast<uchar>(',')] = lex_comma;
			table[static_cast<uchar>('/')] = lex_slash;
			table[static_cast<uchar>(';')] = lex_semicolon;
			table[static_cast<uchar>('=')] = lex_assign;

			table[static_cast<uchar>('{')] = lex_brace_open;
			table[static_cast<uchar>('}')] = lex_brace_close;
			table[static_cast<uchar>('[')] = lex_bracket_open;
			table[static_cast<uchar>(']')] = lex_bracket_close;

			for (char c = '0'; c <= '9'; ++c) table[static_cast<uchar>(c)] = lex_number;
			for (char c = 'A'; c <= 'Z'; ++c) table[static_cast<uchar>(c)] = lex_identifier;
			for (char c = 'a'; c <= 'z'; ++c) table[static_cast<uchar>(c)] = lex_identifier;
			table[static_cast<uchar>('_')] = lex_identifier;

			return table;
		}

		static constexpr auto LexTable = build_lex_table();

		static std::string consume_whitespace(Parser& parser) {
			std::string ws;
			while (!parser.eof()) {
				const Token& tok = parser.peek();
				if (tok.type == TokenType::cWhitespace || tok.type == TokenType::cComment) {
					ws += parser.next().content;
				}
				else {
					break;
				}
			}
			return ws;
		}

		static void parse_declaration(Parser& parser, Node& out);
		static void parse_end_of_file(Parser& parser, Node& out);
		static void parse_function(Parser& parser, Node& out);
		static void parse_layout(Parser& parser, Node& out);
		static void parse_identifier(Parser& parser, Node& out);
		static void parse_type(Parser& parser, Node& out);

		static constexpr auto build_parse_table() {
			std::array<ParseFn, static_cast<size_t>(TokenType::EnumMax)> table{};
			for (auto& fn : table) fn = nullptr;

			table[static_cast<size_t>(TokenType::kwLayout)] = parse_layout;
			table[static_cast<size_t>(TokenType::kwIn)] = parse_declaration;
			table[static_cast<size_t>(TokenType::kwOut)] = parse_declaration;
			table[static_cast<size_t>(TokenType::kwUniform)] = parse_declaration;
			table[static_cast<size_t>(TokenType::Identifier)] = parse_identifier;
			table[static_cast<size_t>(TokenType::EndOfFile)] = parse_end_of_file;

			return table;
		}

		static constexpr auto ParseTable = build_parse_table();

		static void parse_layout(Parser& parser, Node& out) {
			out.type = NodeType::LayoutQualifier;
			out.leadingWhitespace = consume_whitespace(parser);
			out.value = parser.next_significant().content; // layout

			if (parser.peek_significant().type != TokenType::cParenOpen) {
				throw std::runtime_error("Expected '(' after layout");
			}
			parser.next_significant(); // (

			while (!parser.eof() && parser.peek_significant().type != TokenType::cParenClose) {
				const Token& tok = parser.next_significant();

				Node& child = out.children.emplace_back();
				child.leadingWhitespace = consume_whitespace(parser);

				switch (tok.type) {
				case TokenType::kwLocation: child.type = NodeType::Location; break;
				case TokenType::kwBinding: child.type = NodeType::Binding; break;
				case TokenType::kwSet: child.type = NodeType::Set; break;
				default: child.type = NodeType::Identifier; break;
				}
				child.value = tok.content;

				if (!parser.eof() && parser.peek_significant().type == TokenType::cAssign) {
					Node& assignNode = child.children.emplace_back();
					assignNode.type = NodeType::Identifier;
					assignNode.value = parser.next_significant().content; // '='

					Node& valueNode = child.children.emplace_back();
					parse_identifier(parser, valueNode);
				}

				if (!parser.eof() && parser.peek_significant().type == TokenType::cComma) {
					parser.next_significant();
				}
			}

			if (!parser.eof() && parser.peek_significant().type == TokenType::cParenClose) {
				parser.next_significant();
			}
			else {
				throw std::runtime_error("Expected ')' to close layout");
			}
		}

		static void parse_type(Parser& parser, Node& out) {
			out.leadingWhitespace = consume_whitespace(parser);

			const Token& tok = parser.next_significant();
			if (tok.type != TokenType::Identifier) {
				throw std::runtime_error("Expected type identifier, got '" + tok.content + "'");
			}

			out.type = NodeType::TypeSpecifier;
			out.value = tok.content;
		}

		static void parse_function(Parser& parser, Node& out) {
			out.type = NodeType::FunctionDecl;

			// Return type
			Node& returnNode = out.children.emplace_back();
			parse_type(parser, returnNode);

			// Name
			Node& nameNode = out.children.emplace_back();
			parse_identifier(parser, nameNode);

			// '(' already checked by parse_identifier
			std::string leadingWsBeforeParen = consume_whitespace(parser);
			const Token& openParen = parser.next_significant();
			if (openParen.type != TokenType::cParenOpen) {
				throw std::runtime_error("Expected '(' after function name");
			}

			Node& args = out.children.emplace_back();
			args.type = NodeType::FunctionArgs;
			args.leadingWhitespace = leadingWsBeforeParen;

			int parenCount = 1;
			std::stringstream paramContent;
			while (parenCount > 0) {
				const Token& tok = parser.next();
				if (tok.type == TokenType::cParenOpen) parenCount++;
				else if (tok.type == TokenType::cParenClose) parenCount--;

				if (parenCount > 0) paramContent << tok.content;
			}
			args.value = paramContent.str();

			std::string trailingWs = consume_whitespace(parser);

			Node& body = out.children.emplace_back();
			body.type = NodeType::FunctionBody;
			body.leadingWhitespace = trailingWs;

			const Token& nextTok = parser.peek_significant();
			if (nextTok.type == TokenType::cBraceOpen) {
				parser.next_significant(); // {

				const size_t startCursor = parser.cursor;
				int braceCount = 1;

				while (braceCount > 0) {
					const Token& tok = parser.next_significant();
					if (tok.type == TokenType::cBraceOpen) braceCount++;
					else if (tok.type == TokenType::cBraceClose) braceCount--;
				}

				const size_t endCursor = parser.cursor - 1;

				std::stringstream bodyContent;
				for (size_t i = startCursor; i < endCursor; ++i) {
					bodyContent << parser.tokens[i].content;
				}
				body.value = bodyContent.str();
				return;
			}

			if (nextTok.type == TokenType::cSemicolon) {
				parser.next_significant();
				return;
			}

			throw std::runtime_error("Expected ';' or '{' after function declaration");
		}

		static void parse_identifier(Parser& parser, Node& out) {
			// function? (type name '(') at significant token stream
			if (parser.peek_significant(0).type == TokenType::Identifier &&
				parser.peek_significant(1).type == TokenType::Identifier &&
				parser.peek_significant(2).type == TokenType::cParenOpen) {
				parse_function(parser, out);
				return;
			}

			out.leadingWhitespace = consume_whitespace(parser);

			const Token& tok = parser.next_significant();
			out.type = NodeType::Identifier;
			out.value = tok.content;
		}

		static void parse_declaration(Parser& parser, Node& out) {
			out.type = NodeType::Declaration;
			out.leadingWhitespace = consume_whitespace(parser);

			// Always: <layout> <storage> <type> <name>
			Node layoutNode;
			layoutNode.type = NodeType::LayoutQualifier;
			layoutNode.value = "layout";

			// Optional explicit layout(...) in source
			if (!parser.eof() && parser.peek_significant().type == TokenType::kwLayout) {
				parse_layout(parser, layoutNode);
			}

			out.children.push_back(std::move(layoutNode));

			Node storageNode;
			storageNode.type = NodeType::StorageQualifier;
			storageNode.leadingWhitespace = consume_whitespace(parser);
			storageNode.value = parser.next_significant().content;
			out.children.push_back(std::move(storageNode));

			Node typeNode;
			parse_type(parser, typeNode);
			out.children.push_back(std::move(typeNode));

			Node nameNode;
			parse_identifier(parser, nameNode);
			out.children.push_back(std::move(nameNode));

			if (!parser.eof() && parser.peek_significant().type == TokenType::cSemicolon) {
				parser.next_significant();
			}
			else {
				throw std::runtime_error("Expected ';' after declaration");
			}
		}

		static void parse_end_of_file(Parser& parser, Node& out) {
			(void)parser;
			out.type = NodeType::EndOfFile;
		}

		static void serialize_blob(const Node& node, TokenStream& out);
		static void serialize_declaration(const Node& node, TokenStream& out);
		static void serialize_end_of_file(const Node& node, TokenStream& out);
		static void serialize_function(const Node& node, TokenStream& out);
		static void serialize_layout(const Node& node, TokenStream& out);
		static void serialize_literal(const Node& node, TokenStream& out);
		static void serialize_identifier(const Node& node, TokenStream& out);
		static void serialize_storage_qualifier(const Node& node, TokenStream& out);
		static void serialize_translation_unit(const Node& node, TokenStream& out);
		static void serialize_type_specifier(const Node& node, TokenStream& out);
		static void serialize_location(const Node& node, TokenStream& out);
			static void serialize_binding(const Node& node, TokenStream& out);
			static void serialize_set(const Node& node, TokenStream& out);

		static constexpr auto build_serialize_table() {
			std::array<SerializeFn, static_cast<size_t>(NodeType::EnumMax)> table{};
			for (auto& fn : table) fn = serialize_blob;

			table[static_cast<size_t>(NodeType::Declaration)] = serialize_declaration;
			table[static_cast<size_t>(NodeType::EndOfFile)] = serialize_end_of_file;
			table[static_cast<size_t>(NodeType::FunctionDecl)] = serialize_function;
			table[static_cast<size_t>(NodeType::LayoutQualifier)] = serialize_layout;
			table[static_cast<size_t>(NodeType::Identifier)] = serialize_identifier;
			table[static_cast<size_t>(NodeType::Literal)] = serialize_literal;
			table[static_cast<size_t>(NodeType::StorageQualifier)] = serialize_storage_qualifier;
			table[static_cast<size_t>(NodeType::TranslationUnit)] = serialize_translation_unit;
			table[static_cast<size_t>(NodeType::TypeSpecifier)] = serialize_type_specifier;
			table[static_cast<size_t>(NodeType::Location)] = serialize_location;
			table[static_cast<size_t>(NodeType::Binding)] = serialize_binding;
			table[static_cast<size_t>(NodeType::Set)] = serialize_set;
			return table;
		}

		static constexpr auto SerializeTable = build_serialize_table();

		static void serialize_blob(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);
		}

		static void serialize_type_specifier(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);
		}

		static void serialize_storage_qualifier(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);
		}

		static void serialize_identifier(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}

		static void serialize_literal(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::lNumber, node.value);
		}

		static void serialize_layout(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);

			// If empty layout, emit nothing.
			if (node.children.empty()) return;

			out.emplace_back(TokenType::kwLayout, node.value);
			out.emplace_back(TokenType::cParenOpen, "(");

			for (size_t i = 0; i < node.children.size(); ++i) {
				const Node& child = node.children[i];
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for layout child type");
				fn(child, out);

				// Children represent tokens like: "location" "=" "0" stored in the child node itself.
				// Commas are not stored; we emit them.
				if (i + 1 < node.children.size()) {
					out.emplace_back(TokenType::cComma, ",");
				}
			}

			out.emplace_back(TokenType::cParenClose, ")");
		}

		static void serialize_declaration(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);

				// Ensure a space between emitted tokens of declaration children
				// (layout may emit nothing if empty)
				out.emplace_back(TokenType::cWhitespace, " ");
			}

			// Remove the last space if we added one
			if (!out.empty() && out.back().type == TokenType::cWhitespace) {
				out.pop_back();
			}

			out.emplace_back(TokenType::cSemicolon, ";");
		}

		static void serialize_function(const Node& node, TokenStream& out) {
			SerializeTable[static_cast<size_t>(node.children[0].type)](node.children[0], out);
			SerializeTable[static_cast<size_t>(node.children[1].type)](node.children[1], out);

			const Node& argsNode = node.children[2];
			if (!argsNode.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, argsNode.leadingWhitespace);
			out.emplace_back(TokenType::cParenOpen, "(");
			if (!argsNode.value.empty()) out.emplace_back(TokenType::Identifier, argsNode.value);
			out.emplace_back(TokenType::cParenClose, ")");

			const Node& bodyNode = node.children[3];
			if (!bodyNode.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, bodyNode.leadingWhitespace);

			if (!bodyNode.value.empty()) {
				out.emplace_back(TokenType::cBraceOpen, "{");
				out.emplace_back(TokenType::Identifier, bodyNode.value);
				out.emplace_back(TokenType::cBraceClose, "}");
			}
			else {
				out.emplace_back(TokenType::cSemicolon, ";");
			}
		}

		static void serialize_translation_unit(const Node& node, TokenStream& out) {
			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}

		static void serialize_end_of_file(const Node&, TokenStream& out) {
			out.emplace_back(TokenType::EndOfFile, "");
		}
		static void serialize_location(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwLocation, node.value);

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}
		static void serialize_binding(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwBinding, node.value);

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}
		static void serialize_set(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwSet, node.value);

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}


		static void emit_assign(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_brace_close(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_brace_open(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_bracket_close(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_bracket_open(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_comment(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_identifier(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_keyword(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_number(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_paren_close(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_paren_open(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_semicolon(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_slash(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_whitespace(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		static void emit_comma(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }

		static constexpr auto build_emit_table() {
			std::array<EmitFn, static_cast<size_t>(TokenType::EnumMax)> table{};
			for (auto& fn : table) fn = [](Emitter&, std::stringstream&) {};

			table[static_cast<size_t>(TokenType::cComment)] = emit_comment;
			table[static_cast<size_t>(TokenType::cComma)] = emit_comma;
			table[static_cast<size_t>(TokenType::cSemicolon)] = emit_semicolon;
			table[static_cast<size_t>(TokenType::cWhitespace)] = emit_whitespace;
			table[static_cast<size_t>(TokenType::cParenOpen)] = emit_paren_open;
			table[static_cast<size_t>(TokenType::cParenClose)] = emit_paren_close;
			table[static_cast<size_t>(TokenType::cBraceOpen)] = emit_brace_open;
			table[static_cast<size_t>(TokenType::cBraceClose)] = emit_brace_close;
			table[static_cast<size_t>(TokenType::cBracketOpen)] = emit_bracket_open;
			table[static_cast<size_t>(TokenType::cBracketClose)] = emit_bracket_close;
			table[static_cast<size_t>(TokenType::cSlash)] = emit_slash;
			table[static_cast<size_t>(TokenType::cAssign)] = emit_assign;

			table[static_cast<size_t>(TokenType::kwBinding)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwBuffer)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwIn)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwLayout)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwLocation)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwOut)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwSet)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwUniform)] = emit_keyword;

			table[static_cast<size_t>(TokenType::lNumber)] = emit_number;
			table[static_cast<size_t>(TokenType::Identifier)] = emit_identifier;

			return table;
		}

		static constexpr auto EmitTable = build_emit_table();

		static std::string preprocess(std::string_view src) {
			std::string out;
			out.reserve(src.size());
			for (size_t i = 0; i < src.size(); ++i) {
				if (src[i] == '\\' && i + 1 < src.size() && src[i + 1] == '\n') {
					++i;
					continue;
				}
				out.push_back(src[i]);
			}
			return out;
		}

		static TokenStream lexical_analisys(std::string source) {
			Lexer lexer(source);
			TokenStream tokens;

			while (!lexer.eof()) {
				const uchar c = lexer.peek();
				LexTable[c](lexer, tokens);
			}

			tokens.emplace_back(TokenType::EndOfFile, "");
			return tokens;
		}

		static Node parse(const TokenStream& tokens) {
			Parser parser{ 0, tokens };

			Node ast;
			ast.type = NodeType::TranslationUnit;

			while (!parser.eof()) {
				const Token& tok = parser.peek_significant();
				Node& child = ast.children.emplace_back();
				auto fn = ParseTable[static_cast<size_t>(tok.type)];
				if (!fn) throw std::runtime_error("No parse function for token type");
				fn(parser, child);
			}

			return ast;
		}

		static TokenStream serialize(const Node& ast) {
			TokenStream out;
			SerializeTable[static_cast<size_t>(ast.type)](ast, out);
			return out;
		}

		static std::string emit_token_stream(const TokenStream& stream) {
			std::stringstream out;
			Emitter emitter{ 0, stream };

			for (size_t i = 0; i < stream.size(); ++i) {
				emitter.cursor = i;
				const Token& token = emitter.stream[i];
				EmitTable[static_cast<size_t>(token.type)](emitter, out);
			}

			return out.str();
		}

		static bool try_parse_u32(std::string_view s, u32& out) {
			if (s.empty()) return false;
			u32 v = 0;
			for (char c : s) {
				if (c < '0' || c > '9') return false;
				v = (v * 10u) + static_cast<u32>(c - '0');
			}
			out = v;
			return true;
		}

		struct DeclView {
			Node* decl;
			std::string_view storage;
			std::string_view type;
			std::string_view name;
		};

		static bool is_decl_canonical(const Node& n) {
			return (n.type == NodeType::Declaration &&
				n.children.size() == 4 &&
				n.children[0].type == NodeType::LayoutQualifier &&
				n.children[1].type == NodeType::StorageQualifier &&
				n.children[2].type == NodeType::TypeSpecifier &&
				n.children[3].type == NodeType::Identifier);
		}

		static bool decl_view(Node& n, DeclView& out) {
			if (!is_decl_canonical(n)) return false;

			out.decl = &n;
			out.storage = n.children[1].value;
			out.type = n.children[2].value;
			out.name = n.children[3].value;
			return true;
		}

		static bool get_location(const Node& decl, u32& out_location) {
			const Node& layout = decl.children[0];
			for (const Node& item : layout.children) {
				if (item.type != NodeType::Location) continue;
				if (item.children.size() < 2) continue;

				const Node& valueNode = item.children[1];
				u32 loc{};
				if (try_parse_u32(valueNode.value, loc)) {
					out_location = loc;
					return true;
				}
			}
			return false;
		}

		static void set_location(Node& decl, u32 location) {
			Node& layout = decl.children[0];

			std::vector<Node> kept;
			kept.reserve(layout.children.size());
			for (Node& c : layout.children) {
				if (c.type == NodeType::Location) continue;
				kept.push_back(std::move(c));
			}
			layout.children = std::move(kept);

			Node loc;
			loc.type = NodeType::Location;
			loc.value = "location";

			Node assign;
			assign.type = NodeType::Identifier;
			assign.value = "=";

			Node value;
			value.type = NodeType::Literal;
			value.leadingWhitespace = " ";
			value.value = std::to_string(location);

			loc.children.push_back(std::move(assign));
			loc.children.push_back(std::move(value));

			layout.children.push_back(std::move(loc));
		}
		static void collect_decls(Node& root, std::vector<DeclView>& out) {
			std::vector<Node*> stack;
			stack.push_back(&root);

			while (!stack.empty()) {
				Node* n = stack.back();
				stack.pop_back();

				for (Node& c : n->children) {
					stack.push_back(&c);
				}

				DeclView view{};
				if (decl_view(*n, view)) out.push_back(view);
			}
		}

		static void link_varyings(Node& vert_ast, Node& frag_ast) {
			std::vector<DeclView> v_decls;
			std::vector<DeclView> f_decls;
			collect_decls(vert_ast, v_decls);
			collect_decls(frag_ast, f_decls);

			std::unordered_map<std::string, DeclView*> v_out;
			std::unordered_map<std::string, DeclView*> f_in;

			for (auto& d : v_decls) {
				if (d.storage == "out") v_out[std::string(d.name)] = &d;
			}
			for (auto& d : f_decls) {
				if (d.storage == "in") f_in[std::string(d.name)] = &d;
			}

			std::unordered_map<u32, std::string> used_locations;
			std::unordered_map<std::string, u32> final_locations;

			auto claim = [&](const std::string& name, u32 loc) {
				auto it = used_locations.find(loc);
				if (it != used_locations.end() && it->second != name) {
					throw std::runtime_error("LinkShaderStages: location collision at " + std::to_string(loc) +
						" between '" + it->second + "' and '" + name + "'");
				}
				used_locations[loc] = name;
				final_locations[name] = loc;
				};

			for (auto& [name, d] : v_out) {
				u32 loc{};
				if (get_location(*d->decl, loc)) claim(name, loc);
			}
			for (auto& [name, d] : f_in) {
				u32 loc{};
				if (get_location(*d->decl, loc)) claim(name, loc);
			}

			u32 next_loc = 0;
			for (const auto& kv : used_locations) {
				if (kv.first >= next_loc) next_loc = kv.first + 1;
			}

			auto alloc = [&]() -> u32 {
				while (used_locations.find(next_loc) != used_locations.end()) ++next_loc;
				return next_loc++;
				};

			for (auto& [name, v] : v_out) {
				auto it = f_in.find(name);
				if (it == f_in.end()) {
					throw std::runtime_error("LinkShaderStages: vertex out '" + name + "' has no matching fragment in");
				}

				DeclView* fin = it->second;

				if (std::string(v->type) != std::string(fin->type)) {
					throw std::runtime_error("LinkShaderStages: type mismatch for '" + name +
						"': vertex out '" + std::string(v->type) + "' vs fragment in '" + std::string(fin->type) + "'");
				}

				if (final_locations.find(name) == final_locations.end()) {
					claim(name, alloc());
				}
			}

			for (auto& [name, fin] : f_in) {
				auto it = v_out.find(name);
				if (it == v_out.end()) {
					throw std::runtime_error("LinkShaderStages: fragment in '" + name + "' has no matching vertex out");
				}

				if (final_locations.find(name) == final_locations.end()) {
					claim(name, alloc());
				}
			}

			for (auto& [name, loc] : final_locations) {
				if (auto it = v_out.find(name); it != v_out.end()) set_location(*it->second->decl, loc);
				if (auto it = f_in.find(name); it != f_in.end()) set_location(*it->second->decl, loc);
			}
		}
	}

	std::string ProcessShader(std::string_view source) {
		Node ast = detail::parse(detail::lexical_analisys(detail::preprocess(source)));
		return detail::emit_token_stream(detail::serialize(ast));
	}

	template<typename T>
	std::string InjectVertexLayout(std::string_view source) {
		(void)T {};
		// Not implemented yet.
		return std::string(source);
	}

	std::array<std::string, 2> LinkShaderStages(std::string_view vert_source, std::string_view frag_source) {
		Node v = detail::parse(detail::lexical_analisys(detail::preprocess(vert_source)));
		Node f = detail::parse(detail::lexical_analisys(detail::preprocess(frag_source)));

		detail::link_varyings(v, f);

		return {
			detail::emit_token_stream(detail::serialize(v)),
			detail::emit_token_stream(detail::serialize(f))
		};
	}
}

#endif