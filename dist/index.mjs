// src/lexer.ts
var TokenType = /* @__PURE__ */ ((TokenType2) => {
  TokenType2["Number"] = "Number";
  TokenType2["String"] = "String";
  TokenType2["Identifier"] = "Identifier";
  TokenType2["Keyword"] = "Keyword";
  TokenType2["Operator"] = "Operator";
  TokenType2["Punctuation"] = "Punctuation";
  TokenType2["Comment"] = "Comment";
  TokenType2["Eol"] = "Eol";
  return TokenType2;
})(TokenType || {});
var keywords = /* @__PURE__ */ new Set([
  "fn",
  "let",
  "global",
  "if",
  "else",
  "while",
  "for",
  "loop",
  "return",
  "true",
  "false",
  "once",
  "module",
  "extern",
  "import"
]);
var operators = /* @__PURE__ */ new Set([
  "..",
  "+",
  "-",
  "*",
  "/",
  "%",
  "=",
  "==",
  "!=",
  "<",
  ">",
  "<=",
  ">=",
  "&&",
  "||",
  "&",
  "|",
  "!",
  ".",
  "->",
  "+=",
  "-=",
  "*=",
  "/=",
  "%=",
  "..=",
  "++",
  "--"
]);
var punctuation = /* @__PURE__ */ new Set(["(", ")", "{", "}", "[", "]", ",", ":", "@"]);
var LexerError = class extends Error {
  constructor(message, line, column) {
    super(`${message} at ${line}:${column}`);
    this.line = line;
    this.column = column;
    this.name = "LexerError";
  }
};
var Lexer = class {
  constructor(input) {
    this.input = input;
  }
  pos = 0;
  line = 1;
  column = 1;
  reset() {
    this.pos = 0;
    this.line = 1;
    this.column = 1;
  }
  isAtEnd() {
    return this.pos >= this.input.length;
  }
  peek() {
    return this.input[this.pos];
  }
  advance() {
    const char = this.input[this.pos++];
    if (char === "\n") {
      this.line++;
      this.column = 1;
    } else {
      this.column++;
    }
    return char;
  }
  skipWhitespace() {
    while (!this.isAtEnd() && /[ \t\r]/.test(this.peek())) {
      this.advance();
    }
  }
  readNumber() {
    const start = this.pos;
    const startColumn = this.column;
    if (this.peek() === "0" && this.pos + 1 < this.input.length) {
      const nextChar = this.input[this.pos + 1];
      if (nextChar === "x" || nextChar === "X") {
        this.advance();
        this.advance();
        while (!this.isAtEnd() && /[0-9a-fA-F]/.test(this.peek())) {
          this.advance();
        }
        return {
          type: "Number" /* Number */,
          value: this.input.slice(start, this.pos),
          line: this.line,
          column: startColumn
        };
      }
    }
    while (!this.isAtEnd() && /\d/.test(this.peek())) {
      this.advance();
    }
    if (!this.isAtEnd() && this.peek() === "." && this.pos + 1 < this.input.length && /\d/.test(this.input[this.pos + 1])) {
      this.advance();
      while (!this.isAtEnd() && /\d/.test(this.peek())) {
        this.advance();
      }
    }
    if (!this.isAtEnd() && /[eE]/.test(this.peek())) {
      this.advance();
      if (!this.isAtEnd() && /[+-]/.test(this.peek())) {
        this.advance();
      }
      if (this.isAtEnd() || !/\d/.test(this.peek())) {
        throw new LexerError(
          `Invalid scientific notation: missing exponent digits`,
          this.line,
          this.column
        );
      }
      while (!this.isAtEnd() && /\d/.test(this.peek())) {
        this.advance();
      }
    }
    return {
      type: "Number" /* Number */,
      value: this.input.slice(start, this.pos),
      line: this.line,
      column: startColumn
    };
  }
  readIdentifier() {
    const start = this.pos;
    const startColumn = this.column;
    while (!this.isAtEnd() && /[\p{L}\p{N}_]/u.test(this.peek())) {
      this.advance();
    }
    const value = this.input.slice(start, this.pos);
    return {
      type: keywords.has(value) ? "Keyword" /* Keyword */ : "Identifier" /* Identifier */,
      value,
      line: this.line,
      column: startColumn
    };
  }
  readString() {
    const quoteType = this.advance();
    const startColumn = this.column - 1;
    let value = "";
    while (!this.isAtEnd() && this.peek() !== quoteType) {
      if (this.peek() === "\\") {
        this.advance();
        if (!this.isAtEnd()) {
          const escapeChar = this.advance();
          switch (escapeChar) {
            case "n":
              value += "\n";
              break;
            case "t":
              value += "	";
              break;
            case "r":
              value += "\r";
              break;
            case '"':
              value += '"';
              break;
            case "'":
              value += "'";
              break;
            case "\\":
              value += "\\";
              break;
            case "u": {
              let hex = "";
              for (let i = 0; i < 4; i++) {
                if (this.isAtEnd() || !/[0-9a-fA-F]/.test(this.peek())) {
                  throw new LexerError(
                    `Invalid Unicode escape sequence`,
                    this.line,
                    this.column
                  );
                }
                hex += this.advance();
              }
              value += String.fromCharCode(parseInt(hex, 16));
              break;
            }
            default:
              value += escapeChar;
              break;
          }
        }
      } else {
        value += this.advance();
      }
    }
    if (this.isAtEnd()) {
      throw new LexerError(`Unterminated string`, this.line, startColumn);
    }
    this.advance();
    return {
      type: "String" /* String */,
      value,
      line: this.line,
      column: startColumn
    };
  }
  readSingleLineComment() {
    const startColumn = this.column;
    this.advance();
    this.advance();
    let value = "";
    while (!this.isAtEnd() && this.peek() !== "\n") {
      value += this.advance();
    }
    return {
      type: "Comment" /* Comment */,
      value: "//" + value,
      line: this.line,
      column: startColumn
    };
  }
  readMultiLineComment() {
    const startColumn = this.column;
    const startLine = this.line;
    this.advance();
    this.advance();
    let value = "";
    let nestingLevel = 1;
    while (!this.isAtEnd() && nestingLevel > 0) {
      const char = this.peek();
      if (char === "/" && this.pos + 1 < this.input.length && this.input[this.pos + 1] === "*") {
        value += this.advance();
        value += this.advance();
        nestingLevel++;
      } else if (char === "*" && this.pos + 1 < this.input.length && this.input[this.pos + 1] === "/") {
        value += this.advance();
        value += this.advance();
        nestingLevel--;
      } else {
        value += this.advance();
      }
    }
    if (nestingLevel > 0) {
      throw new LexerError(
        `Unterminated multi-line comment`,
        startLine,
        startColumn
      );
    }
    return {
      type: "Comment" /* Comment */,
      value: "/*" + value,
      line: startLine,
      column: startColumn
    };
  }
  readOperatorOrPunctuation() {
    const startColumn = this.column;
    let value = this.advance();
    while (!this.isAtEnd()) {
      const nextChar = this.peek();
      const potentialOp = value + nextChar;
      if (operators.has(potentialOp)) {
        value = potentialOp;
        this.advance();
      } else {
        break;
      }
    }
    if (operators.has(value)) {
      return {
        type: "Operator" /* Operator */,
        value,
        line: this.line,
        column: startColumn
      };
    } else if (punctuation.has(value)) {
      return {
        type: "Punctuation" /* Punctuation */,
        value,
        line: this.line,
        column: startColumn
      };
    } else {
      throw new LexerError(
        `Unknown operator or punctuation '${value}'`,
        this.line,
        startColumn
      );
    }
  }
  next() {
    this.skipWhitespace();
    if (this.isAtEnd()) {
      return {
        type: "Eol" /* Eol */,
        value: "\n",
        line: this.line,
        column: this.column
      };
    }
    const char = this.peek();
    if (char === "\n" || char === ";") {
      const res = {
        type: "Eol" /* Eol */,
        value: char,
        line: this.line,
        column: this.column
      };
      this.advance();
      return res;
    }
    if (char === "/") {
      if (this.pos + 1 < this.input.length) {
        const nextChar = this.input[this.pos + 1];
        if (nextChar === "/") {
          return this.readSingleLineComment();
        }
        if (nextChar === "*") {
          return this.readMultiLineComment();
        }
      }
    }
    if (/\d/.test(char)) {
      return this.readNumber();
    }
    if (/[\p{L}_]/u.test(char)) {
      return this.readIdentifier();
    }
    if (char === '"' || char === "'") {
      return this.readString();
    }
    if (operators.has(char) || punctuation.has(char)) {
      return this.readOperatorOrPunctuation();
    }
    const errorLine = this.line;
    const errorColumn = this.column;
    this.advance();
    throw new LexerError(
      `Unexpected character '${char}'`,
      errorLine,
      errorColumn
    );
  }
};

// src/parser.ts
import { ErrorList, sanitize } from "@scratch-fuse/utility";
var ParserError = class extends Error {
  constructor(message, token) {
    super(
      `${message} at ${token.line}:${token.column}-${token.line + token.value.length}`
    );
    this.token = token;
    this.name = "ParserError";
  }
};
var Parser = class {
  constructor(lexer) {
    this.lexer = lexer;
    this.loadNextToken();
    this.currentToken = this.nextToken;
    this.loadNextToken();
  }
  previousToken = null;
  currentToken = null;
  nextToken = null;
  reset(lexer) {
    this.previousToken = null;
    this.currentToken = null;
    this.nextToken = null;
    if (lexer) {
      this.lexer = lexer;
    }
    this.lexer.reset();
    this.loadNextToken();
    this.currentToken = this.nextToken;
    this.loadNextToken();
  }
  loadNextToken() {
    do {
      if (this.lexer.isAtEnd()) {
        this.nextToken = null;
        return;
      }
      this.nextToken = this.lexer.next();
    } while (this.nextToken.type === "Comment" /* Comment */ || this.nextToken.type === "Eol" /* Eol */ && this.nextToken.value !== ";");
  }
  isEof() {
    return this.currentToken === null && this.lexer.isAtEnd();
  }
  peek() {
    if (this.isEof()) {
      return {
        type: "Eol" /* Eol */,
        value: "",
        line: 0,
        column: 0
      };
    }
    return this.currentToken;
  }
  previous() {
    if (!this.previousToken) {
      throw new ParserError(
        "Cannot get previous token at beginning of input",
        this.peek()
      );
    }
    return this.previousToken;
  }
  advance() {
    const prev = this.currentToken;
    if (!this.isEof()) {
      this.previousToken = this.currentToken;
      this.currentToken = this.nextToken;
      this.loadNextToken();
    }
    return prev || this.peek();
  }
  skipComments() {
  }
  _matchBase(type, values) {
    if (this._checkBase(type, values)) {
      this.advance();
      return true;
    }
    return false;
  }
  match(...types) {
    if (this.check(...types)) {
      this.advance();
      return true;
    }
    return false;
  }
  matchOperator(...values) {
    return this._matchBase("Operator" /* Operator */, values);
  }
  matchIdentifier(...values) {
    return this._matchBase("Identifier" /* Identifier */, values);
  }
  matchKeyword(...values) {
    return this._matchBase("Keyword" /* Keyword */, values);
  }
  matchPunctuation(...values) {
    return this._matchBase("Punctuation" /* Punctuation */, values);
  }
  _checkBase(type, values) {
    if (!this.check(type)) return false;
    for (const value of values) {
      if (this.peek().value === value) {
        return true;
      }
    }
    return false;
  }
  // private checkKeyword(...values: string[]): boolean {
  //   return this._checkBase(TokenType.Keyword, values);
  // }
  // private checkOperator(...values: string[]): boolean {
  //   return this._checkBase(TokenType.Operator, values);
  // }
  check(...types) {
    if (this.isEof()) return false;
    for (const type of types) {
      if (this.peek().type === type) {
        return true;
      }
    }
    return false;
  }
  synchronize() {
    if (!this.isEof()) {
      this.advance();
    }
    while (!this.isEof()) {
      if (this.previousToken && this.previous().type === "Eol" /* Eol */) return;
      if (!this.isEof()) {
        switch (this.peek().type) {
          case "Keyword" /* Keyword */:
            const keyword = this.peek().value;
            if ([
              "fn",
              "let",
              "global",
              "if",
              "while",
              "for",
              "until",
              "loop",
              "return",
              "extern",
              "module",
              "import"
            ].includes(keyword)) {
              return;
            }
            break;
        }
      }
      this.advance();
    }
  }
  consume(type, message) {
    if (this.check(type)) {
      return this.advance();
    }
    const token = this.peek();
    throw new ParserError(
      `${message}. Got ${token.type} (${token.value})`,
      token
    );
  }
  _consumeBase(type, value, message) {
    if (this.isEof()) {
      throw new ParserError(
        `${message}. Unexpected end of input`,
        this.previous()
      );
    }
    const v = this.peek();
    if (!this.check(type) || v.value !== value) {
      throw new ParserError(`${message}. Got ${v.value}`, v);
    }
    this.advance();
    return v;
  }
  consumeKeyword(value, message) {
    return this._consumeBase("Keyword" /* Keyword */, value, message);
  }
  consumeEol(value, message) {
    return this._consumeBase("Eol" /* Eol */, value, message);
  }
  consumeOperator(value, message) {
    return this._consumeBase("Operator" /* Operator */, value, message);
  }
  consumeIdentifier(value, message) {
    return this._consumeBase("Identifier" /* Identifier */, value, message);
  }
  consumePunctuation(value, message) {
    return this._consumeBase("Punctuation" /* Punctuation */, value, message);
  }
  // Expression parsing
  parseExpression() {
    return this.parseLogicalOr();
  }
  parseLogicalOr() {
    let expr = this.parseLogicalAnd();
    while (this.matchOperator("||")) {
      const operator = this.previous().value;
      const right = this.parseLogicalAnd();
      expr = {
        type: "BinaryExpression",
        left: expr,
        operator,
        right,
        line: expr.line,
        column: expr.column
      };
    }
    return expr;
  }
  parseLogicalAnd() {
    let expr = this.parseEquality();
    while (this.matchOperator("&&")) {
      const operator = this.previous().value;
      const right = this.parseEquality();
      expr = {
        type: "BinaryExpression",
        left: expr,
        operator,
        right,
        line: expr.line,
        column: expr.column
      };
    }
    return expr;
  }
  parseEquality() {
    let expr = this.parseComparison();
    while (this.matchOperator("==", "!=")) {
      const op = this.previous().value;
      const right = this.parseComparison();
      expr = {
        type: "BinaryExpression",
        left: expr,
        operator: op,
        right,
        line: expr.line,
        column: expr.column
      };
    }
    return expr;
  }
  parseComparison() {
    let expr = this.parseTerm();
    while (this.matchOperator(">", ">=", "<", "<=")) {
      const op = this.previous().value;
      const right = this.parseTerm();
      expr = {
        type: "BinaryExpression",
        left: expr,
        operator: op,
        right,
        line: expr.line,
        column: expr.column
      };
    }
    return expr;
  }
  parseTerm() {
    let expr = this.parseFactor();
    while (this.matchOperator("+", "-", "..")) {
      const op = this.previous().value;
      const right = this.parseFactor();
      expr = {
        type: "BinaryExpression",
        left: expr,
        operator: op,
        right,
        line: expr.line,
        column: expr.column
      };
    }
    return expr;
  }
  parseFactor() {
    let expr = this.parseUnary();
    while (this.matchOperator("*", "/", "%")) {
      const op = this.previous().value;
      const right = this.parseUnary();
      expr = {
        type: "BinaryExpression",
        left: expr,
        operator: op,
        right,
        line: expr.line,
        column: expr.column
      };
    }
    return expr;
  }
  parseUnary() {
    if (this.matchOperator("!", "-", "+")) {
      const op = this.previous().value;
      const operand = this.parseUnary();
      if (operand.type === "Literal" && (op === "+" || op === "-")) {
        if (typeof operand.value === "number") {
          const foldedValue = op === "-" ? -operand.value : +operand.value;
          return {
            type: "Literal",
            value: foldedValue,
            raw: foldedValue.toString(),
            line: operand.line,
            column: operand.column
          };
        }
      }
      return {
        type: "UnaryExpression",
        operator: op,
        operand,
        line: this.previous().line,
        column: this.previous().column
      };
    }
    return this.parseCall();
  }
  parseCall() {
    let expr = this.parsePrimary();
    while (true) {
      if (this.matchPunctuation("(")) {
        expr = this.finishCall(expr);
        if (this.check("Punctuation" /* Punctuation */) && this.peek().value === "{") {
          const thenBlock = this.parseBlockStatement();
          expr.then = thenBlock;
        }
      } else if (this.matchOperator(".")) {
        const name = this.consume(
          "Identifier" /* Identifier */,
          "Expected property name after '.'"
        );
        expr = {
          type: "MemberExpression",
          object: expr,
          property: {
            type: "Identifier",
            name: name.value,
            line: name.line,
            column: name.column
          },
          computed: false,
          line: expr.line,
          column: expr.column
        };
      } else if (this.matchPunctuation("[")) {
        const property = this.parseExpression();
        this.consumePunctuation("]", "Expected ']' after array access");
        expr = {
          type: "MemberExpression",
          object: expr,
          property,
          computed: true,
          line: expr.line,
          column: expr.column
        };
      } else if (this.check("Punctuation" /* Punctuation */) && this.peek().value === "{") {
        const thenBlock = this.parseBlockStatement();
        expr = {
          type: "CallExpression",
          callee: expr,
          arguments: [],
          then: thenBlock,
          line: expr.line,
          column: expr.column
        };
      } else {
        break;
      }
    }
    return expr;
  }
  finishCall(callee) {
    const args = [];
    if (!this.check("Punctuation" /* Punctuation */) || this.peek().value !== ")") {
      do {
        args.push(this.parseExpression());
      } while (this.matchPunctuation(","));
    }
    this.consumePunctuation(")", "Expected ')' after arguments");
    return {
      type: "CallExpression",
      callee,
      arguments: args,
      line: callee.line,
      column: callee.column
    };
  }
  parsePrimary() {
    if (this.matchKeyword("true")) {
      const keyword = this.previous();
      return {
        type: "Literal",
        value: true,
        raw: "true",
        line: keyword.line,
        column: keyword.column
      };
    }
    if (this.matchKeyword("false")) {
      const keyword = this.previous();
      return {
        type: "Literal",
        value: false,
        raw: "false",
        line: keyword.line,
        column: keyword.column
      };
    }
    if (this.match("Number" /* Number */)) {
      const token2 = this.previous();
      return {
        type: "Literal",
        value: Number(token2.value),
        raw: token2.value,
        line: token2.line,
        column: token2.column
      };
    }
    if (this.match("String" /* String */)) {
      const token2 = this.previous();
      return {
        type: "Literal",
        value: token2.value,
        raw: `"${token2.value}"`,
        line: token2.line,
        column: token2.column
      };
    }
    if (this.matchIdentifier("Infinity", "NaN")) {
      const token2 = this.previous();
      return {
        type: "Literal",
        value: token2.value === "Infinity" ? Infinity : NaN,
        raw: token2.value,
        line: token2.line,
        column: token2.column
      };
    }
    if (this.match("Identifier" /* Identifier */)) {
      const token2 = this.previous();
      return {
        type: "Identifier",
        name: token2.value,
        line: token2.line,
        column: token2.column
      };
    }
    if (this.matchPunctuation("(")) {
      const expr = this.parseExpression();
      this.consumePunctuation(")", "Expected ')' after expression");
      return expr;
    }
    if (this.matchPunctuation("[")) {
      return this.parseArrayLiteral();
    }
    const token = this.peek();
    throw new ParserError(
      `Unexpected token ${token.type}: ${token.value}`,
      token
    );
  }
  parseArrayLiteral() {
    const start = this.previous();
    const elements = [];
    if (!this.check("Punctuation" /* Punctuation */) || this.peek().value !== "]") {
      do {
        elements.push(this.parseExpression());
      } while (this.matchPunctuation(","));
    }
    this.consumePunctuation("]", "Expected ']' after array elements");
    return {
      type: "ArrayExpression",
      elements,
      line: start.line,
      column: start.column
    };
  }
  // Statement parsing
  parseStatement() {
    try {
      if (this.match("Keyword" /* Keyword */)) {
        const keyword = this.previous().value;
        switch (keyword) {
          case "let":
          case "global":
            return this.parseVariableDeclaration(keyword === "global");
          case "if":
            return this.parseIfStatement();
          case "while":
            return this.parseWhileStatement();
          case "for":
            return this.parseForStatement();
          case "loop":
            return this.parseLoopStatement();
          case "return":
            return this.parseReturnStatement();
          case "fn":
            return this.parseFunctionDeclaration();
          case "extern":
            return this.parseExternDeclaration();
          case "module":
            return this.parseModuleDeclaration();
          case "import":
            return this.parseImportStatement();
        }
      }
      if (this.matchPunctuation("@")) {
        return this.parseDecoratorStatement();
      }
      if (this.check("Eol" /* Eol */) && this.peek().value === ";") {
        const token = this.advance();
        return {
          type: "NoopStatement",
          line: token.line,
          column: token.column
        };
      }
      if (this.matchOperator("++", "--")) {
        const operator = this.previous().value;
        const expr2 = this.parseExpression();
        return {
          type: "IncrementStatement",
          operator,
          target: expr2,
          line: expr2.line,
          column: expr2.column
        };
      }
      const expr = this.parseExpression();
      if (this.matchOperator("=", "+=", "-=", "*=", "/=", "%=", "..=")) {
        const operator = this.previous().value;
        const right = this.parseExpression();
        return {
          type: "AssignmentStatement",
          left: expr,
          operator,
          right,
          line: expr.line,
          column: expr.column
        };
      } else if (this.matchOperator("++", "--")) {
        const operator = this.previous().value;
        return {
          type: "IncrementStatement",
          operator,
          target: expr,
          line: expr.line,
          column: expr.column
        };
      }
      return {
        type: "ExpressionStatement",
        expression: expr,
        line: expr.line,
        column: expr.column
      };
    } catch (error) {
      this.synchronize();
      throw error;
    }
  }
  parseDecoratorStatement() {
    const nameToken = this.consume(
      "Identifier" /* Identifier */,
      "Expected decorator name after @"
    );
    const name = {
      type: "Identifier",
      name: nameToken.value,
      line: nameToken.line,
      column: nameToken.column
    };
    const args = [];
    if (this.matchPunctuation("(")) {
      if (!this.check("Punctuation" /* Punctuation */) || this.peek().value !== ")") {
        do {
          if (this.match("Number" /* Number */)) {
            const token = this.previous();
            args.push({
              type: "Literal",
              value: Number(token.value),
              raw: token.value,
              line: token.line,
              column: token.column
            });
          } else if (this.match("String" /* String */)) {
            const token = this.previous();
            args.push({
              type: "Literal",
              value: token.value,
              raw: sanitize(token.value),
              line: token.line,
              column: token.column
            });
          } else if (this.match("Keyword" /* Keyword */)) {
            const token = this.previous();
            if (token.value === "true" || token.value === "false") {
              args.push({
                type: "Literal",
                value: token.value === "true",
                raw: token.value,
                line: token.line,
                column: token.column
              });
            } else {
              throw new ParserError(
                `Invalid decorator argument ${token.value}`,
                token
              );
            }
          } else {
            const token = this.peek();
            throw new ParserError(`Invalid decorator argument`, token);
          }
        } while (this.matchPunctuation(","));
      }
      this.consumePunctuation(")", "Expected ')' after decorator arguments");
    }
    const decorated = this.parseStatement();
    return {
      type: "DecoratorStatement",
      name,
      arguments: args,
      target: decorated,
      line: name.line,
      column: name.column
    };
  }
  parseVariableDeclaration(isGlobal) {
    const name = this.consume(
      "Identifier" /* Identifier */,
      "Expected variable name"
    ).value;
    this.consumeOperator("=", "Variable requires a initial value");
    const initializer = this.parseExpression();
    return {
      type: "VariableDeclaration",
      name,
      isGlobal,
      initializer,
      line: this.previous().line,
      column: this.previous().column
    };
  }
  parseIfStatement() {
    this.consumePunctuation("(", "Expected '(' after 'if'");
    const condition = this.parseExpression();
    this.consumePunctuation(")", "Expected ')' after if condition");
    const then = this.parseStatementOrBlock();
    let elseClause;
    if (this.matchKeyword("else")) {
      elseClause = this.parseStatementOrBlock();
    }
    return {
      type: "IfStatement",
      condition,
      then,
      else: elseClause,
      line: condition.line,
      column: condition.column
    };
  }
  parseWhileStatement() {
    this.consumePunctuation("(", "Expected '(' after 'while'");
    const condition = this.parseExpression();
    this.consumePunctuation(")", "Expected ')' after while condition");
    const body = this.parseStatementOrBlock();
    return {
      type: "WhileStatement",
      condition,
      body,
      line: condition.line,
      column: condition.column
    };
  }
  parseForStatement() {
    const start = this.previous();
    this.consumePunctuation("(", "Expected '(' after 'for'");
    let init;
    if (!this.check("Eol" /* Eol */) || this.peek().value !== ";") {
      init = this.parseStatement();
      if (!["AssignmentStatement", "IncrementStatement"].includes(init.type)) {
        throw new ParserError(
          "For loop init must be an assignment or increment statement",
          start
        );
      }
      this.consumeEol(";", "Expected ';' after for init");
    } else {
      this.advance();
    }
    let condition;
    if (!this.check("Eol" /* Eol */) || this.peek().value !== ";") {
      condition = this.parseExpression();
      this.consumeEol(";", "Expected ';' after for condition");
    } else {
      this.advance();
    }
    let increment;
    if (!this.check("Eol" /* Eol */) || this.peek().value !== ")") {
      increment = this.parseStatement();
      if (!["AssignmentStatement", "IncrementStatement"].includes(increment.type)) {
        throw new ParserError(
          "For loop increment must be an assignment or increment statement",
          start
        );
      }
    }
    this.consumePunctuation(")", "Expected ')' after for increment");
    const body = this.parseStatementOrBlock();
    return {
      type: "ForStatement",
      init,
      condition,
      increment,
      body,
      line: start.line,
      column: start.column
    };
  }
  parseLoopStatement() {
    const body = this.parseStatementOrBlock();
    return {
      type: "LoopStatement",
      body,
      line: body.line,
      column: body.column
    };
  }
  parseReturnStatement() {
    const start = this.previous();
    let value;
    if (!this.isEof() && !this.check("Eol" /* Eol */)) {
      const currentToken = this.peek();
      if (currentToken.type !== "Punctuation" /* Punctuation */ || currentToken.value !== "}" && currentToken.value !== ")") {
        value = this.parseExpression();
      }
    }
    return {
      type: "ReturnStatement",
      value,
      line: start.line,
      column: start.column
    };
  }
  parseBlockStatement() {
    const start = this.consumePunctuation("{", "Expected '{'");
    const statements = [];
    while (!this.check("Punctuation" /* Punctuation */) || this.peek().value !== "}") {
      if (this.isEof()) {
        throw new ParserError(`Unterminated block`, start);
      }
      statements.push(this.parseStatement());
    }
    this.consumePunctuation("}", "Expected '}'");
    return {
      type: "BlockStatement",
      body: statements,
      line: start.line,
      column: start.column
    };
  }
  parseExpressionStatement() {
    const expr = this.parseExpression();
    return {
      type: "ExpressionStatement",
      expression: expr,
      line: expr.line,
      column: expr.column
    };
  }
  parseStatementOrBlock() {
    if (this.check("Punctuation" /* Punctuation */) && this.peek().value === "{") {
      return this.parseBlockStatement();
    }
    if (this.check("Eol" /* Eol */) && this.peek().value === ";") {
      const token = this.advance();
      return {
        type: "NoopStatement",
        line: token.line,
        column: token.column
      };
    }
    return this.parseStatement();
  }
  // Function declaration parsing
  parseFunctionDeclaration() {
    const token = this.consume(
      "Identifier" /* Identifier */,
      "Expected function name"
    );
    const name = {
      type: "Identifier",
      name: token.value,
      line: token.line,
      column: token.column
    };
    this.consumePunctuation("(", "Expected '(' after function name");
    const parameters = [];
    if (!this.check("Punctuation" /* Punctuation */) || this.peek().value !== ")") {
      do {
        const paramName = this.consume(
          "Identifier" /* Identifier */,
          "Expected parameter name"
        );
        this.consumePunctuation(":", "Expected ':' after parameter name");
        const paramType = this.consume(
          "Identifier" /* Identifier */,
          "Expected parameter type"
        );
        parameters.push({
          name: {
            type: "Identifier",
            name: paramName.value,
            line: paramName.line,
            column: paramName.column
          },
          type: {
            type: "Identifier",
            name: paramType.value,
            line: paramType.line,
            column: paramType.column
          }
        });
      } while (this.matchPunctuation(","));
    }
    this.consumePunctuation(")", "Expected ')' after parameters");
    let once = false;
    if (this.matchKeyword("once")) {
      once = true;
    }
    this.consumeOperator("->", "Expected -> before return type");
    const returnType = this.consume(
      "Identifier" /* Identifier */,
      "Expected return type"
    );
    const body = this.parseBlockStatement();
    return {
      type: "FunctionDeclaration",
      name,
      parameters,
      returnType: {
        type: "Identifier",
        name: returnType.value,
        line: returnType.line,
        column: returnType.column
      },
      once,
      body,
      line: this.previous().line,
      column: this.previous().column
    };
  }
  // Extern declaration parsing
  parseExternDeclaration() {
    const name = this.consume(
      "Identifier" /* Identifier */,
      "Expected extern variable name"
    );
    this.consumeOperator("=", "Expected '=' after extern name");
    const value = this.parseJSONValue();
    return {
      type: "ExternDeclaration",
      name: {
        type: "Identifier",
        name: name.value,
        line: name.line,
        column: name.column
      },
      value,
      line: this.previous().line,
      column: this.previous().column
    };
  }
  // Module declaration parsing
  parseModuleDeclaration() {
    const start = this.previous();
    const nameToken = this.consume("Identifier" /* Identifier */, "Expected module name");
    if (this.matchOperator("=")) {
      const alias = this.parseTypeExpression();
      return {
        type: "ModuleDeclaration",
        name: {
          type: "Identifier",
          name: nameToken.value,
          line: nameToken.line,
          column: nameToken.column
        },
        alias,
        line: start.line,
        column: start.column
      };
    }
    this.consumePunctuation("{", "Expected '{' after module name");
    const body = [];
    while (!this.check("Punctuation" /* Punctuation */) || this.peek().value !== "}") {
      if (this.isEof()) {
        throw new ParserError("Unterminated module", start);
      }
      body.push(this.parseStatement());
    }
    this.consumePunctuation("}", "Expected '}' after module body");
    return {
      type: "ModuleDeclaration",
      name: {
        type: "Identifier",
        name: nameToken.value,
        line: nameToken.line,
        column: nameToken.column
      },
      body,
      line: start.line,
      column: start.column
    };
  }
  // Parse type expression (identifier with optional member access like A.B.C)
  // Used for module aliases, impl extends, and as operator
  parseTypeExpression() {
    const start = this.consume("Identifier" /* Identifier */, "Expected type name");
    let expr = {
      type: "Identifier",
      name: start.value,
      line: start.line,
      column: start.column
    };
    while (this.matchOperator(".")) {
      const property = this.consume(
        "Identifier" /* Identifier */,
        "Expected property name after '.'"
      );
      expr = {
        type: "MemberExpression",
        object: expr,
        property: {
          type: "Identifier",
          name: property.value,
          line: property.line,
          column: property.column
        },
        computed: false,
        line: expr.line,
        column: expr.column
      };
    }
    return expr;
  }
  // Import statement parsing
  parseImportStatement() {
    const start = this.previous();
    const pathToken = this.consume(
      "String" /* String */,
      "Expected string literal path after import"
    );
    return {
      type: "ImportStatement",
      path: {
        type: "Literal",
        value: pathToken.value,
        raw: sanitize(pathToken.value),
        line: pathToken.line,
        column: pathToken.column
      },
      line: start.line,
      column: start.column
    };
  }
  // Helper method to parse JSON-like values for extern
  parseJSONValue() {
    if (this.matchPunctuation("{")) {
      const obj = {};
      while (!this.check("Punctuation" /* Punctuation */) || this.peek().value !== "}") {
        this.skipComments();
        if (this.check("Punctuation" /* Punctuation */) && this.peek().value === "}") {
          break;
        }
        let key;
        if (this.match("String" /* String */)) {
          key = this.previous().value;
        } else if (this.match("Identifier" /* Identifier */)) {
          key = this.previous().value;
        } else {
          const token2 = this.peek();
          throw new ParserError("Expected property key", token2);
        }
        this.consumePunctuation(":", "Expected ':' after key");
        const value = this.parseJSONValue();
        obj[key] = value;
        if (this.check("Punctuation" /* Punctuation */) && this.peek().value === ",") {
          this.advance();
        }
        this.skipComments();
      }
      this.consumePunctuation("}", "Expected '}' after object");
      return obj;
    }
    if (this.matchPunctuation("[")) {
      const arr = [];
      while (!this.check("Punctuation" /* Punctuation */) || this.peek().value !== "]") {
        arr.push(this.parseJSONValue());
        if (this.check("Punctuation" /* Punctuation */) && this.peek().value === ",") {
          this.advance();
        }
      }
      this.consumePunctuation("]", "Expected ']' after array");
      return arr;
    }
    if (this.match("String" /* String */)) {
      return this.previous().value;
    }
    if (this.match("Number" /* Number */)) {
      return parseFloat(this.previous().value);
    }
    if (this.matchKeyword("true")) {
      return true;
    }
    if (this.matchKeyword("false")) {
      return false;
    }
    if (this.matchIdentifier("null")) {
      return null;
    }
    const token = this.peek();
    throw new ParserError(
      `Unexpected token in JSON value: ${token.value}`,
      token
    );
  }
  // Error handling and recovery
  parseDeclaration() {
    try {
      return this.parseStatement();
    } catch (error) {
      this.synchronize();
      throw error;
    }
  }
  // Main parsing method
  parse() {
    this.reset();
    const statements = [];
    const errors = [];
    while (!this.isEof()) {
      try {
        if (this.check("Eol" /* Eol */)) {
          this.advance();
          continue;
        }
        const stmt = this.parseDeclaration();
        statements.push(stmt);
      } catch (error) {
        errors.push(error);
        this.synchronize();
      }
    }
    if (errors.length > 0) {
      throw new ErrorList(errors);
    }
    return {
      type: "Program",
      body: statements,
      line: 1,
      column: 1
    };
  }
  // Additional method to parse a single expression (useful for testing)
  parseExpressionOnly() {
    return this.parseExpression();
  }
};
function toSource(node, indent = 2, semi = false) {
  const nl = indent > 0 ? "\n" : "";
  const sp = indent > 0 ? " " : "";
  const ksp = " ";
  const indentStr = indent > 0 ? " ".repeat(indent) : "";
  const useSemi = indent === 0 || semi;
  function getPrecedence(node2) {
    if (node2.type === "BinaryExpression") {
      const op = node2.operator;
      if (op === "||") return 1;
      if (op === "&&") return 2;
      if (op === "==" || op === "!=") return 3;
      if (op === ">" || op === ">=" || op === "<" || op === "<=") return 4;
      if (op === "+" || op === "-" || op === "..") return 5;
      if (op === "*" || op === "/" || op === "%") return 6;
    }
    if (node2.type === "AsExpression") return 3.5;
    if (node2.type === "UnaryExpression") return 7;
    return 100;
  }
  function needsParens(parent, child, isLeft) {
    const parentPrec = getPrecedence(parent);
    const childPrec = getPrecedence(child);
    if (childPrec > parentPrec) return false;
    if (childPrec < parentPrec) return true;
    if (parent.type === "BinaryExpression" && child.type === "BinaryExpression") {
      return !isLeft;
    }
    return false;
  }
  function indentLines(str, level) {
    if (indent === 0) return str;
    const prefix = indentStr.repeat(level);
    return str.split("\n").map((line) => line ? prefix + line : "").join("\n");
  }
  function formatBlock(statements, level) {
    if (statements.length === 0) return "";
    return statements.map((stmt) => {
      const source = toSourceImpl(stmt, level);
      if (indent === 0) return source;
      return indentStr.repeat(level) + source;
    }).join(nl);
  }
  function toSourceImpl(node2, level = 0) {
    switch (node2.type) {
      // Expressions
      case "Literal": {
        const lit = node2;
        if (typeof lit.value === "string") {
          return lit.raw;
        }
        return sanitize(lit.value);
      }
      case "Identifier": {
        const id = node2;
        return id.name;
      }
      case "BinaryExpression": {
        const bin = node2;
        let left = toSourceImpl(bin.left, level);
        let right = toSourceImpl(bin.right, level);
        if (needsParens(bin, bin.left, true)) {
          left = `(${left})`;
        }
        if (needsParens(bin, bin.right, false)) {
          right = `(${right})`;
        }
        return `${left}${sp}${bin.operator}${sp}${right}`;
      }
      case "UnaryExpression": {
        const unary = node2;
        let operand = toSourceImpl(unary.operand, level);
        if (unary.operand.type === "BinaryExpression") {
          operand = `(${operand})`;
        }
        return `${unary.operator}${operand}`;
      }
      case "CallExpression": {
        const call = node2;
        const callee = toSourceImpl(call.callee, level);
        const args = call.arguments.map((arg) => toSourceImpl(arg, level)).join(`,${sp}`);
        let result = `${callee}${call.arguments.length === 0 && level === 0 ? `` : `(${args})`}`;
        if (call.then) {
          result += `${sp}${toSourceImpl(call.then, level)}`;
        }
        return result;
      }
      case "MemberExpression": {
        const member = node2;
        const object = toSourceImpl(member.object, level);
        if (member.computed) {
          const property = toSourceImpl(member.property, level);
          return `${object}[${property}]`;
        } else {
          const property = toSourceImpl(member.property, level);
          return `${object}.${property}`;
        }
      }
      case "ArrayExpression": {
        const arr = node2;
        const elements = arr.elements.map((el) => toSourceImpl(el, level)).join(`,${sp}`);
        return `[${elements}]`;
      }
      // Statements
      case "NoopStatement": {
        return ";";
      }
      case "ExpressionStatement": {
        const exprStmt = node2;
        const expr = toSourceImpl(exprStmt.expression, level);
        return useSemi ? expr + ";" : expr;
      }
      case "VariableDeclaration": {
        const varDecl = node2;
        const keyword = varDecl.isGlobal ? "global" : "let";
        const init = toSourceImpl(varDecl.initializer, level);
        const stmt = `${keyword}${ksp}${varDecl.name}${sp}=${sp}${init}`;
        return useSemi ? stmt + ";" : stmt;
      }
      case "AssignmentStatement": {
        const assign = node2;
        const left = toSourceImpl(assign.left, level);
        const right = toSourceImpl(assign.right, level);
        const stmt = `${left}${sp}${assign.operator}${sp}${right}`;
        return useSemi ? stmt + ";" : stmt;
      }
      case "IncrementStatement": {
        const inc = node2;
        const target = toSourceImpl(inc.target, level);
        const stmt = `${target}${inc.operator}`;
        return useSemi ? stmt + ";" : stmt;
      }
      case "IfStatement": {
        const ifStmt = node2;
        const condition = toSourceImpl(ifStmt.condition, level);
        const thenPart = toSourceImpl(ifStmt.then, level);
        let result = `if${sp}(${condition})${sp}${thenPart}`;
        if (ifStmt.else) {
          const elsePart = toSourceImpl(ifStmt.else, level);
          result += `${sp}else${sp}${elsePart}`;
        }
        return result;
      }
      case "WhileStatement": {
        const whileStmt = node2;
        const condition = toSourceImpl(whileStmt.condition, level);
        const body = toSourceImpl(whileStmt.body, level);
        return `while${sp}(${condition})${sp}${body}`;
      }
      case "ForStatement": {
        const forStmt = node2;
        const init = forStmt.init ? toSourceImpl(forStmt.init, level).replace(/;$/, "") : "";
        const condition = forStmt.condition ? toSourceImpl(forStmt.condition, level) : "";
        const increment = forStmt.increment ? toSourceImpl(forStmt.increment, level).replace(/;$/, "") : "";
        const body = toSourceImpl(forStmt.body, level);
        return `for${sp}(${init};${sp}${condition};${sp}${increment})${sp}${body}`;
      }
      case "LoopStatement": {
        const loopStmt = node2;
        const body = toSourceImpl(loopStmt.body, level);
        return `loop${sp}${body}`;
      }
      case "ReturnStatement": {
        const ret = node2;
        let stmt;
        if (ret.value) {
          const value = toSourceImpl(ret.value, level);
          stmt = `return${ksp}${value}`;
        } else {
          stmt = "return";
        }
        return useSemi ? stmt + ";" : stmt;
      }
      case "BlockStatement": {
        const block = node2;
        if (block.body.length === 0) {
          return `{}`;
        }
        const body = formatBlock(block.body, level + 1);
        return `{${nl}${body}${nl}${indentLines("}", level)}`;
      }
      case "DecoratorStatement": {
        const decorator = node2;
        const name = toSourceImpl(decorator.name, level);
        const args = decorator.arguments.map((arg) => toSourceImpl(arg, level)).join(`,${sp}`);
        const target = toSourceImpl(decorator.target, level);
        if (decorator.arguments.length > 0) {
          return `@${name}(${args})${sp}${target}`;
        }
        return `@${name}${ksp}${target}`;
      }
      case "FunctionDeclaration": {
        const fn = node2;
        const name = toSourceImpl(fn.name, level);
        const params = fn.parameters.map(
          (p) => `${toSourceImpl(p.name, level)}:${sp}${toSourceImpl(p.type, level)}`
        ).join(`,${sp}`);
        const returnType = toSourceImpl(fn.returnType, level);
        const once = fn.once ? `${sp}once` : "";
        const body = toSourceImpl(fn.body, level);
        return `fn${ksp}${name}(${params})${once}${sp}->${sp}${returnType}${sp}${body}`;
      }
      case "ExternDeclaration": {
        const ext = node2;
        const value = formatJSONValue(ext.value, level + 1);
        const stmt = `extern${ksp}${ext.name.name}${sp}=${sp}${value}`;
        return useSemi ? stmt + ";" : stmt;
      }
      case "ModuleDeclaration": {
        const mod = node2;
        if (mod.alias) {
          const aliasStr = toSourceImpl(mod.alias, level);
          const stmt = `module${ksp}${mod.name.name}${sp}=${sp}${aliasStr}`;
          return useSemi ? stmt + ";" : stmt;
        }
        if (!mod.body || mod.body.length === 0) {
          return `module${ksp}${mod.name.name}${sp}{}`;
        }
        const body = formatBlock(mod.body, level + 1);
        return `module${ksp}${mod.name.name}${sp}{${nl}${body}${nl}${indentLines("}", level)}`;
      }
      case "ImportStatement": {
        const imp = node2;
        const stmt = `import${ksp}${imp.path.raw}`;
        return useSemi ? stmt + ";" : stmt;
      }
      case "Program": {
        const program = node2;
        return formatBlock(program.body, 0);
      }
      default:
        return `/* Unknown node type: ${node2.type} */`;
    }
  }
  function formatJSONValue(value, level) {
    if (value === null) return "null";
    if (value === true) return "true";
    if (value === false) return "false";
    if (typeof value === "number") return String(value);
    if (typeof value === "string") {
      return `"${value.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
    }
    if (Array.isArray(value)) {
      const elements = value.map((v) => formatJSONValue(v, level)).join(`,${sp}`);
      return `[${elements}]`;
    }
    if (typeof value === "object") {
      const entries = Object.entries(value).map(([k, v]) => {
        const formattedValue = formatJSONValue(v, level + 1);
        const needsQuotes = !/^[a-zA-Z_$][a-zA-Z0-9_$]*$/.test(k);
        const keyStr = needsQuotes ? `"${k}"` : k;
        const line = `${keyStr}:${sp}${formattedValue}`;
        return indent === 0 ? line : indentStr.repeat(level) + line;
      }).join(`,${nl}`);
      if (entries.length === 0) return "{}";
      const closing = indent === 0 ? "}" : indentStr.repeat(level - 1) + "}";
      return `{${nl}${entries}${nl}${closing}`;
    }
    return String(value);
  }
  return toSourceImpl(node, 0);
}
export {
  Lexer,
  LexerError,
  Parser,
  ParserError,
  TokenType,
  toSource
};
