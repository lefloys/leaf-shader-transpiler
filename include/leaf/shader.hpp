#pragma once

#include <string>

namespace lf {
    std::string ProcessShader(const std::string& source);
}

#define LEAF_SHADER_IMPLEMENTATION // For development
#ifdef LEAF_SHADER_IMPLEMENTATION

#include <vector>
#include <cctype>
#include <iostream>
#include <climits>
#include <unordered_map>
#include <array>

namespace lf {
    using uchar = unsigned char;
    enum class TokenType : uchar {
        cComma,         // ','
        cComment,       // "//"
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
        EnumMax,
    };
    struct Token {
        Token() = default;
        Token(TokenType t, std::string&& c) : type(t), content(c) {}
        Token(TokenType t, uchar c) : type(t), content(1, static_cast<char>(c)) {}
        TokenType type;
        std::string content;
    };   
    using TokenStream = std::vector<Token>;
    struct Lexer {
        Lexer(const std::string& source) : content(source) {}
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
        std::string content;
    };   
    using LexFn = void(*)(Lexer&, TokenStream&);
    using EmitFn = void(*)(std::string&, TokenStream&, size_t);
    inline const std::unordered_map<std::string, TokenType> keywords = {
        { "binding", TokenType::kwBinding },
        { "buffer", TokenType::kwBuffer},
        { "in", TokenType::kwIn},
        { "layout", TokenType::kwLayout},
        { "location", TokenType::kwLocation },
        { "out", TokenType::kwOut},
        { "set", TokenType::kwSet },
        { "uniform", TokenType::kwUniform }
    };

    namespace detail {
        void lex_whitespace(Lexer& lexer, TokenStream& tokens) {
            std::string ws;
            while (std::isspace(lexer.peek())) { ws += lexer.next(); }
            tokens.push_back({ TokenType::cWhitespace, std::move(ws) });
        }
        void lex_semicolon(Lexer& lexer, TokenStream& tokens) {
            tokens.push_back({ TokenType::cSemicolon, std::string(1, lexer.next()) });
        }
        void lex_identifier(Lexer& lexer, TokenStream& tokens) {
            uchar c = lexer.peek();

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
        void lex_assign(Lexer& lexer, TokenStream& tokens) {
            tokens.push_back({ TokenType::cAssign, lexer.next() });
        }
        void lex_paren_open(Lexer& lexer, TokenStream& tokens) {
            tokens.push_back({ TokenType::cParenOpen, lexer.next() } );
        }
        void lex_paren_close(Lexer& lexer, TokenStream& tokens) {
            tokens.push_back({ TokenType::cParenClose, lexer.next() });
        }
        void lex_brace_open(Lexer& lexer, TokenStream& tokens) {
            tokens.push_back({ TokenType::cBraceOpen, lexer.next() });
        }
        void lex_brace_close(Lexer& lexer, TokenStream& tokens) {
            tokens.push_back({ TokenType::cBraceClose, lexer.next()});
        }
        void lex_bracket_open(Lexer& lexer, TokenStream& tokens) {
            tokens.push_back({ TokenType::cBracketOpen, lexer.next()});
        }
        void lex_bracket_close(Lexer& lexer, TokenStream& tokens) {
            tokens.push_back({ TokenType::cBracketClose, lexer.next()});
        }
        void lex_slash(Lexer& lexer, TokenStream& tokens) {
            uchar next = lexer.peek(1);

            if (next == '/') {
                // Line comment
                lexer.skip(2);

                std::string comment = "//";
                while (!lexer.eof() && lexer.peek() != '\n') {
                    comment += lexer.next();
                }

                tokens.push_back({ TokenType::cComment, std::move(comment) });
            }
            else if (next == '*') {
                // Block comment
                lexer.skip(2);

                std::string comment = "/*";
                while (!lexer.eof()) {
                    char c = lexer.next();
                    comment += c;
                    if (c == '*' && lexer.peek() == '/') {
                        comment += lexer.next(); // consume closing '/'
                        break;
                    }
                }

                tokens.push_back({ TokenType::cComment, std::move(comment) });
            }
            else {
                // Just a slash
                tokens.push_back({ TokenType::cSlash, lexer.next() });
            }
        }

        constexpr std::array<LexFn, 256> build_lex_table() {
            std::array<LexFn, 256> table;

            // 1) set all entries to default
            for (int i = 0; i < 256; i++) {
                table[i] = [](Lexer& lexer, TokenStream&) { 
                    std::cout << lexer.peek() << "\n";
                    throw std::runtime_error(std::string("Unexpected character: '") + static_cast<char>(lexer.peek()) + "'"); };
            }

            table[static_cast<uchar>('\t')] = lex_whitespace;
            table[static_cast<uchar>('\n')] = lex_whitespace;
            table[static_cast<uchar>('\r')] = lex_whitespace;
            table[static_cast<uchar>('(')] = lex_paren_open;
            table[static_cast<uchar>(')')] = lex_paren_close;
            table[static_cast<uchar>('/')] = lex_slash;
            table[static_cast<uchar>(' ')] = lex_whitespace;
            for (char c = '0'; c <= '9'; c++) table[static_cast<uchar>(c)] = lex_number;
            table[static_cast<uchar>(';')] = lex_semicolon;
            table[static_cast<uchar>('=')] = lex_assign;
            for (char c = 'A'; c <= 'Z'; c++) table[static_cast<uchar>(c)] = lex_identifier;
            table[static_cast<uchar>('_')] = lex_identifier;
            for (char c = 'a'; c <= 'z'; c++) table[static_cast<uchar>(c)] = lex_identifier;
            table[static_cast<uchar>('[')] = lex_bracket_open;
            table[static_cast<uchar>(']')] = lex_bracket_close;
            table[static_cast<uchar>('{')] = lex_brace_open;
            table[static_cast<uchar>('}')] = lex_brace_close;

            return table;
        }
    }
    constexpr auto LexTable = detail::build_lex_table();

    namespace detail {
        void emit_whitespace(std::string& out, TokenStream& stream, size_t i) {

        }

        constexpr std::array<EmitFn, static_cast<size_t>(TokenType::EnumMax)> build_emit_table() {
            std::array<EmitFn, static_cast<size_t>(TokenType::EnumMax)> table;
            for (size_t i = 0; i < table.size(); i++) table[i] = [](std::string&, TokenStream&, size_t) {};
            table[static_cast<size_t>(TokenType::cWhitespace)] = emit_whitespace;
            return table;
        }

    }
    constexpr auto EmitTable = detail::build_emit_table();


    std::string preprocess(const std::string& src) {
        std::string out;
        out.reserve(src.size());
        for (size_t i = 0; i < src.size(); ++i) {
            if (src[i] == '\\' && i + 1 < src.size() && src[i + 1] == '\n') {
                // skip both '\' and '\n'
                ++i;
                continue;
            }
            out += src[i];
        }
        return out;
    }
    TokenStream LexicalAnalisys(const std::string& source) {
        Lexer lexer = { source };
        TokenStream tokens;
        while (!lexer.eof()) {
            uchar c = lexer.peek();
            LexTable[c](lexer, tokens);
        }
        return tokens;
    }
    std::string EmitTokenStream(const TokenStream& stream) {
        return "";
    }

    std::string ProcessShader(const std::string& source) {
        // 1) Lex into vector<Token>
        TokenStream tokens = LexicalAnalisys(preprocess(source));
        // 2) Build AST
        // AbstractSyntaxTree ast = { tokens };
        // 3) Inject layout() declarations
        // ??
        // 4) deconstruct AST into vector<Token>
        // tokens = DeconstructAst(ast);
        // 5) Emit tokens as text
        // return EmitAsSource(tokens);

        return std::string(source);
    }
}
#endif
