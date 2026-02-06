/*
** leaf/shader.hpp
*/
#pragma once
#include <string>

namespace lf {
	std::string ProcessShader(std::string_view source);
}

#ifdef __INTELLISENSE__
#define LEAF_SHADER_IMPLEMENTATION
#endif
#ifdef LEAF_SHADER_IMPLEMENTATION

#include <array>
#include <cctype>
#include <sstream>
#include <climits>
#include <iostream>
#include <string_view>
#include <unordered_map>
#include <vector>

/*
** Function node layout: <type> <name> <args body> <func body>
*/


namespace lf {
	using uchar = unsigned char;

	enum class TokenType : uchar {
		cComment,       // "//"
		cComma,			// ','
		cSemicolon,     // ';'
		cWhitespace,    // ' ' '\n' '\t'
		cParenOpen,     // '('
		cParenClose,    // ')'
		cBraceOpen,     // '{'
		cBraceClose,    // '}'
		cBracketOpen,   // '['
		cBracketClose,  // ']'
		cSlash,         // '/'
		cAssign,        // '='
		kwBinding,      // "binding"
		kwBuffer,       // "buffer"
		kwIn,           // "in"
		kwLayout,       // "layout"
		kwLocation,     // "location"
		kwOut,          // "out"
		kwSet,          // "set"
		kwUniform,      // "uniform"

		lNumber,        // "123"
		Identifier,
		EndOfFile,
		EnumMax,
	};

	enum class NodeType {
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
		Token(TokenType t, std::string&& c) : type(t), content(c) {}
		Token(TokenType t, uchar c) : type(t), content(1, static_cast<char>(c)) {}
		TokenType type;
		std::string content;
	};

	using TokenStream = std::vector<Token>;

	struct Node {
		NodeType type;
		std::string value;
		std::vector<Node> children;
		std::string leadingWhitespace;
	};

	// string -> TokenStream
	struct Lexer {
		Lexer(std::string& source) : content(source) {}
		uchar peek(size_t steps = 0) const {
			size_t idx = cursor + steps;
			if (idx >= content.size()) { return '\0'; }
			return static_cast<uchar>(content.at(idx));
		}
		uchar next() {
			if (cursor >= content.size()) { return '\0'; }
			return static_cast<uchar>(content.at(cursor++));
		}
		void skip(size_t amount) {
			cursor += amount;
			if (cursor > content.size()) { cursor = content.size(); }
		}
		bool eof() const { return cursor >= content.size(); }

		size_t cursor = 0;
		const std::string& content;
	};

	// TokenStream -> AST
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
					if (sigCount == num) { return tok; }
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
			if (cursor >= stream.size()) { throw std::runtime_error(""); }
			return stream.at(cursor++);
		}
		size_t cursor = 0;
		const TokenStream& stream;
	};

	using LexFn = void(*)(Lexer&, TokenStream&);
	using ParseFn = void(*)(Parser&, Node&);
	using SerializeFn = void(*)(const Node&, TokenStream&);
	using EmitFn = void(*)(Emitter&, std::stringstream&);

	const std::unordered_map<std::string, TokenType> keywords = {
		{ "binding", TokenType::kwBinding },
		{ "buffer", TokenType::kwBuffer},
		{ "in", TokenType::kwIn},
		{ "layout", TokenType::kwLayout},
		{ "location", TokenType::kwLocation },
		{ "out", TokenType::kwOut},
		{ "set", TokenType::kwSet },
		{ "uniform", TokenType::kwUniform }
	};

	// --- detail: lexing ---
	namespace detail {
		void lex_assign(Lexer& lexer, TokenStream& tokens);
		void lex_brace_close(Lexer& lexer, TokenStream& tokens);
		void lex_brace_open(Lexer& lexer, TokenStream& tokens);
		void lex_bracket_open(Lexer& lexer, TokenStream& tokens);
		void lex_bracket_close(Lexer& lexer, TokenStream& tokens);
		void lex_comma(Lexer& lexer, TokenStream& tokens);
		void lex_semicolon(Lexer& lexer, TokenStream& tokens);
		void lex_paren_open(Lexer& lexer, TokenStream& tokens);
		void lex_paren_close(Lexer& lexer, TokenStream& tokens);
		void lex_whitespace(Lexer& lexer, TokenStream& tokens);
		void lex_identifier(Lexer& lexer, TokenStream& tokens);
		void lex_number(Lexer& lexer, TokenStream& tokens);
		void lex_slash(Lexer& lexer, TokenStream& tokens);

		constexpr auto build_lex_table() {
			std::array<LexFn, 256> table;
			for (int i = 0; i < 256; i++) {
				table[i] = [](Lexer& lexer, TokenStream&) {
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
			for (char c = '0'; c <= '9'; c++) table[static_cast<uchar>(c)] = lex_number;
			for (char c = 'A'; c <= 'Z'; c++) table[static_cast<uchar>(c)] = lex_identifier;
			for (char c = 'a'; c <= 'z'; c++) table[static_cast<uchar>(c)] = lex_identifier;
			table[static_cast<uchar>('_')] = lex_identifier;
			table[static_cast<uchar>('{')] = lex_brace_open;
			table[static_cast<uchar>('}')] = lex_brace_close;
			table[static_cast<uchar>('[')] = lex_bracket_open;
			table[static_cast<uchar>(']')] = lex_bracket_close;
			return table;
		}
	}
	constexpr auto LexTable = detail::build_lex_table();
	namespace detail {
		void lex_assign(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cAssign, lexer.next() }); }
		void lex_brace_close(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cBraceClose, lexer.next() }); }
		void lex_brace_open(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cBraceOpen, lexer.next() }); }
		void lex_bracket_open(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cBracketOpen, lexer.next() }); }
		void lex_bracket_close(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cBracketClose, lexer.next() }); }
		void lex_comma(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cComma, lexer.next() }); }
		void lex_semicolon(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cSemicolon, std::string(1, lexer.next()) }); }
		void lex_paren_open(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cParenOpen, lexer.next() }); }
		void lex_paren_close(Lexer& lexer, TokenStream& tokens) { tokens.push_back({ TokenType::cParenClose, lexer.next() }); }
		void lex_whitespace(Lexer& lexer, TokenStream& tokens) {
			// Only ever 0 or 1 whitespace, so no while loop
			if (std::isspace(lexer.peek())) tokens.push_back({ TokenType::cWhitespace, std::string(1, lexer.next()) });
		}
		void lex_identifier(Lexer& lexer, TokenStream& tokens) {
			std::string id;
			while (std::isalnum(lexer.peek()) || lexer.peek() == '_') id += lexer.next();
			auto it = keywords.find(id);
			TokenType type = (it != keywords.end()) ? it->second : TokenType::Identifier;
			tokens.push_back({ type, std::move(id) });
		}
		void lex_number(Lexer& lexer, TokenStream& tokens) {
			std::string num;
			while (std::isdigit(lexer.peek())) num += lexer.next();
			tokens.push_back({ TokenType::lNumber, std::move(num) });
		}
		void lex_slash(Lexer& lexer, TokenStream& tokens) {
			uchar next = lexer.peek(1);
			if (next == '/') {
				lexer.skip(2);
				std::string comment = "//";
				while (!lexer.eof() && lexer.peek() != '\n') comment += lexer.next();
				tokens.push_back({ TokenType::cComment, std::move(comment) });
			}
			else if (next == '*') {
				lexer.skip(2);
				std::string comment = "/*";
				while (!lexer.eof()) {
					char c = lexer.next();
					comment += c;
					if (c == '*' && lexer.peek() == '/') {
						comment += lexer.next();
						break;
					}
				}
				tokens.push_back({ TokenType::cComment, std::move(comment) });
			}
			else {
				tokens.push_back({ TokenType::cSlash, lexer.next() });
			}
		}
	}
	namespace detail {
		void parse_declaration(Parser& parser, Node& out);
		void parse_end_of_file(Parser& parser, Node& out);
		void parse_function(Parser& parser, Node& out);
		void parse_layout(Parser& parser, Node& out);
		void parse_identifier(Parser& parser, Node& out);
		void parse_type(Parser& parser, Node& out);

		constexpr auto build_parse_table() {
			std::array<ParseFn, static_cast<size_t>(TokenType::EnumMax)> table;
			for (auto& fn : table) fn = nullptr;
			table[static_cast<size_t>(TokenType::kwLayout)] = parse_layout;
			table[static_cast<size_t>(TokenType::kwIn)] = parse_declaration;
			table[static_cast<size_t>(TokenType::kwOut)] = parse_declaration;
			table[static_cast<size_t>(TokenType::kwUniform)] = parse_declaration;
			table[static_cast<size_t>(TokenType::Identifier)] = parse_identifier;
			table[static_cast<size_t>(TokenType::EndOfFile)] = parse_end_of_file;
			return table;
		}
	}
	constexpr auto ParseTable = detail::build_parse_table();
	namespace detail {
		std::string consume_whitespace(Parser& parser) {
			std::string ws;
			while (!parser.eof()) {
				const Token& tok = parser.peek();
				if (tok.type == TokenType::cWhitespace || tok.type == TokenType::cComment) {
					ws += parser.next().content; // append all whitespace/comments
				}
				else {
					break;
				}
			}
			return ws;
		}

		void parse_declaration(Parser& parser, Node& out) {
			out.type = NodeType::Declaration;
			out.leadingWhitespace = consume_whitespace(parser);

			// Storage qualifier
			Node& storageNode = out.children.emplace_back();
			storageNode.type = NodeType::StorageQualifier;
			storageNode.leadingWhitespace = consume_whitespace(parser);
			storageNode.value = parser.next_significant().content;

			// Type
			Node& typeNode = out.children.emplace_back();
			parse_type(parser, typeNode);

			// Variable name
			Node& identNode = out.children.emplace_back();
			parse_identifier(parser, identNode);

			// Optional semicolon
			if (!parser.eof() && parser.peek_significant().type == TokenType::cSemicolon) {
				parser.next_significant();
			}
			else {
				throw std::runtime_error("Expected ';' after declaration");
			}
		}
		void parse_end_of_file(Parser& parser, Node& out) {
			out.type = NodeType::EndOfFile;
		}
		void parse_function(Parser& parser, Node& out) {
			out.type = NodeType::FunctionDecl;

			// --- Return type ---
			Node& returnNode = out.children.emplace_back();
			parse_type(parser, returnNode);

			// --- Function name ---
			Node& nameNode = out.children.emplace_back();
			parse_identifier(parser, nameNode);

			// --- Parameter list ---
			std::string leadingWsBeforeParen = consume_whitespace(parser); // preserve whitespace before '('
			const Token& openParen = parser.next_significant();
			if (openParen.type != TokenType::cParenOpen) {
				throw std::runtime_error("Expected '(' after function name");
			}

			Node& paramsNode = out.children.emplace_back();
			paramsNode.type = NodeType::FunctionArgs;
			paramsNode.leadingWhitespace = leadingWsBeforeParen;

			int parenCount = 1;
			std::stringstream paramContent;

			while (parenCount > 0) {
				const Token& tok = parser.next(); // consume all tokens, including whitespace/comments
				if (tok.type == TokenType::cParenOpen) parenCount++;
				else if (tok.type == TokenType::cParenClose) parenCount--;

				if (parenCount > 0) paramContent << tok.content; // everything inside ()
			}

			paramsNode.value = paramContent.str(); // full blob of parameters

			// --- Capture whitespace after ')' ---
			std::string trailingWs = consume_whitespace(parser);

			// --- Push a FunctionBody child always ---
			Node& bodyNode = out.children.emplace_back();
			bodyNode.type = NodeType::FunctionBody;
			bodyNode.leadingWhitespace = trailingWs;

			// --- Check what comes next ---
			const Token& nextTok = parser.peek_significant();
			if (nextTok.type == TokenType::cBraceOpen) {
				// Function definition → parse body
				parser.next_significant(); // consume '{'
				size_t startCursor = parser.cursor;
				int braceCount = 1;

				while (braceCount > 0) {
					const Token& tok = parser.next_significant();
					if (tok.type == TokenType::cBraceOpen) braceCount++;
					else if (tok.type == TokenType::cBraceClose) braceCount--;
				}

				size_t endCursor = parser.cursor - 1; // stop before closing '}'

				std::stringstream bodyContent;
				for (size_t i = startCursor; i < endCursor; ++i) {
					bodyContent << parser.tokens[i].content;
				}
				bodyNode.value = bodyContent.str();
			}
			else if (nextTok.type == TokenType::cSemicolon) {
				// Forward declaration → just consume ';', leave value empty
				parser.next_significant();
			}
			else {
				throw std::runtime_error("Expected ';' for forward declaration or '{' for function body");
			}
		}

		void parse_layout(Parser& parser, Node& out) {
			out.type = NodeType::LayoutQualifier;
			out.leadingWhitespace = consume_whitespace(parser);

			out.value = parser.next_significant().content; // 'layout'

			if (parser.peek_significant().type != TokenType::cParenOpen) {
				throw std::runtime_error("Expected '(' after layout");
			}
			parser.next_significant();

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

				// Optional = value
				if (!parser.eof() && parser.peek_significant().type == TokenType::cAssign) {
					Node& assignNode = child.children.emplace_back();
					assignNode.type = NodeType::Identifier;
					assignNode.value = parser.next_significant().content;

					Node& valueNode = child.children.emplace_back();
					parse_identifier(parser, valueNode);
				}

				// Optional comma
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
		void parse_identifier(Parser& parser, Node& out) {
			// recognized a function
			if (parser.peek_significant(0).type == TokenType::Identifier && parser.peek_significant(1).type == TokenType::Identifier && parser.peek_significant(2).type == TokenType::cParenOpen) {
				parse_function(parser, out);
				return;
			}

			out.leadingWhitespace = consume_whitespace(parser);
			const Token& tok = parser.next_significant();
			out.type = NodeType::Identifier;
			out.value = tok.content;
			
		}
		void parse_type(Parser& parser, Node& out) {
			out.leadingWhitespace = consume_whitespace(parser);

			const Token& tok = parser.next_significant();
			if (tok.type == TokenType::Identifier) {
				out.type = NodeType::TypeSpecifier;
				out.value = tok.content;
			}
			else throw std::runtime_error("Expected a type identifier, got '" + tok.content + "'");
		}

	}

	namespace detail {
		void serialize_blob(const Node& node, TokenStream& out);
		void serialize_declaration(const Node& node, TokenStream& out);
		void serialize_end_of_file(const Node& node, TokenStream& out);
		void serialize_function(const Node& node, TokenStream& out);
		void serialize_layout(const Node& node, TokenStream& out);
		void serialize_literal(const Node& node, TokenStream& out);
		void serialize_identifier(const Node& node, TokenStream& out);
		void serialize_storage_qualifier(const Node& node, TokenStream& out);
		void serialize_translation_unit(const Node& node, TokenStream& out);
		void serialize_type_specifier(const Node& node, TokenStream& out);
		constexpr auto build_serialize_table() {
			std::array<SerializeFn, static_cast<size_t>(NodeType::EnumMax)> table;
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
			return table;
		}
	}
	constexpr auto SerializeTable = detail::build_serialize_table();
	namespace detail {
		void serialize_blob(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value.empty() ? std::string("\'Undefined NodeType:") + std::to_string(static_cast<size_t>(node.type)) + std::string("\'") : node.value);
		}
		void serialize_declaration(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
			out.emplace_back(TokenType::cSemicolon, ";");
		}
		void serialize_end_of_file(const Node& node, TokenStream& out) {
			out.emplace_back(TokenType::EndOfFile, "/* END OF FILE */");
		}
		void serialize_function(const Node& node, TokenStream& out) {
			// --- Return type ---
			SerializeTable[static_cast<size_t>(node.children[0].type)](node.children[0], out);

			// --- Function name ---
			SerializeTable[static_cast<size_t>(node.children[1].type)](node.children[1], out);

			// --- Arguments blob ---
			const Node& argsNode = node.children[2];
			if (!argsNode.leadingWhitespace.empty()) { out.emplace_back(TokenType::cWhitespace, argsNode.leadingWhitespace); }
			out.emplace_back(TokenType::cParenOpen, "(");
			if (!argsNode.value.empty()) {
				// treat the blob as a single token to preserve everything inside ()
				out.emplace_back(TokenType::Identifier, argsNode.value);
			}
			out.emplace_back(TokenType::cParenClose, ")");

			// --- Function body ---
			const Node& bodyNode = node.children[3];
			if (!bodyNode.leadingWhitespace.empty()) { 
				out.emplace_back(TokenType::cWhitespace, bodyNode.leadingWhitespace);
			}

			if (!bodyNode.value.empty()) {
				// Function definition
				out.emplace_back(TokenType::cBraceOpen, "{");
				out.emplace_back(TokenType::Identifier, bodyNode.value);
				out.emplace_back(TokenType::cBraceClose, "}");
			}
			else {
				// Forward declaration
				out.emplace_back(TokenType::cSemicolon, ";");
			}
		}


		void serialize_layout(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwLayout, node.value);
			out.emplace_back(TokenType::cParenOpen, "(");
			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
			out.emplace_back(TokenType::cParenClose, ")");
		}
		void serialize_literal(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::lNumber, node.value);
		}
		void serialize_identifier(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);
		}
		void serialize_storage_qualifier(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);
		}
		void serialize_translation_unit(const Node& node, TokenStream& out) {
			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}
		void serialize_type_specifier(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) { out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace); }
			out.emplace_back(TokenType::Identifier, node.value);
		}
	}


	namespace detail {
		void emit_assign(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_brace_close(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_brace_open(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_bracket_close(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_bracket_open(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_comment(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_identifier(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_keyword(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_number(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_paren_close(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_paren_open(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_semicolon(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_slash(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }
		void emit_whitespace(Emitter& emitter, std::stringstream& out) { out << emitter.next().content; }

		constexpr auto build_emit_table() {
			std::array<EmitFn, static_cast<size_t>(TokenType::EnumMax)> table;
			for (size_t i = 0; i < table.size(); i++) table[i] = [](Emitter&, std::stringstream&) {};
			table[static_cast<size_t>(TokenType::cComment)] = emit_comment;
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
	}

	constexpr auto EmitTable = detail::build_emit_table();
	void normalize_layout_on_interfaces(Node& node) {
		// Recurse first
		for (Node& child : node.children) {
			normalize_layout_on_interfaces(child);
		}

		if (node.type != NodeType::Declaration) return;
		if (node.children.empty()) return;

		// Check if the first child is a LayoutQualifier
		if (node.children[0].type == NodeType::LayoutQualifier) return;

		// The first child should be storage qualifier
		if (node.children[0].type != NodeType::StorageQualifier) return;

		Node& storage = node.children[0];

		// Only modify 'in' or 'out'
		if (storage.value != "in" && storage.value != "out") return;

		// Insert empty LayoutQualifier at the front
		Node layout;
		layout.type = NodeType::LayoutQualifier;
		layout.value = "layout";

		node.children.insert(node.children.begin(), std::move(layout));

		// After insert, the storage node has shifted to index 1
		Node& shifted_storage = node.children[1];

		// Add a leading space only if it doesn't already have one
		if (shifted_storage.leadingWhitespace.empty()) {
			shifted_storage.leadingWhitespace = " ";
		}
	}




	std::string preprocess(std::string_view src) {
		std::string out;
		out.reserve(src.size());
		for (size_t i = 0; i < src.size(); ++i) {
			if (src[i] == '\\' && i + 1 < src.size() && src[i + 1] == '\n') { ++i; continue; }
			out += src[i];
		}
		return out;
	}
	TokenStream lexical_analisys(std::string source) {
		Lexer lexer = { source };
		TokenStream tokens;
		while (!lexer.eof()) {
			uchar c = lexer.peek();
			LexTable[c](lexer, tokens);
		}
		tokens.emplace_back(TokenType::EndOfFile, "End of File");
		return tokens;
	}
	Node parse(const TokenStream& tokens) {
		Parser parser = { 0, tokens };
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
	TokenStream serialize(const Node& ast) {
		TokenStream out;
		SerializeTable[static_cast<size_t>(ast.type)](ast, out);
		return out;
	}
	std::string emit_token_stream(const TokenStream& stream) {
		std::stringstream out;
		Emitter emitter{ 0, stream };
		for (size_t i = 0; i < stream.size(); ++i) {
			const Token& token = emitter.stream[i];
			emitter.cursor = i;
			EmitTable[static_cast<size_t>(token.type)](emitter, out);
		}
		return out.str();
	}

	std::string ProcessShader(std::string_view source) {
		Node ast = parse(lexical_analisys(preprocess(source)));
		normalize_layout_on_interfaces(ast);
		return emit_token_stream(serialize(ast));
	}

}

#endif
