import { ErrorList } from './util/error'

export enum TokenType {
  Number = 'Number',
  String = 'String',
  Identifier = 'Identifier',
  Keyword = 'Keyword',
  Operator = 'Operator',
  Punctuation = 'Punctuation',
  Comment = 'Comment',
  Eol = 'Eol'
}

export interface Token {
  type: TokenType
  value: string
  line: number
  column: number
}

const keywords = new Set([
  'fn',
  'let',
  'global',
  'if',
  'else',
  'while',
  'for',
  'loop',
  'return',
  'true',
  'false',
  'once',
  'namespace'
])

const operators = new Set([
  '..',
  '+',
  '-',
  '*',
  '/',
  '%',
  '=',
  '==',
  '!=',
  '<',
  '>',
  '<=',
  '>=',
  '&&',
  '||',
  '!',
  '.',
  '->',
  '+=',
  '-=',
  '*=',
  '/=',
  '%=',
  '..=',
  '++',
  '--'
])

const punctuation = new Set(['(', ')', '{', '}', '[', ']', ',', ':', '@'])

export class LexerError extends Error {
  constructor(
    message: string,
    public line: number,
    public column: number
  ) {
    super(`${message} at ${line}:${column}`)
    this.name = 'LexerError'
  }
}

export class Lexer {
  private pos: number = 0
  private line: number = 1
  private column: number = 1

  constructor(private input: string) {}

  public reset() {
    this.pos = 0
    this.line = 1
    this.column = 1
  }

  public all(): Token[] {
    const tokens: Token[] = []
    const errors: Error[] = []
    while (true) {
      try {
        const token = this.next()
        tokens.push(token)
      } catch (e) {
        if (e instanceof LexerError) {
          errors.push(e)
        } else {
          throw e
        }
      }
      if (this.isAtEnd()) break
    }
    if (errors.length > 0) {
      throw new ErrorList(errors)
    }
    return tokens
  }

  private isAtEnd(): boolean {
    return this.pos >= this.input.length
  }

  private peek(): string {
    return this.input[this.pos]
  }

  private advance(): string {
    const char = this.input[this.pos++]
    if (char === '\n') {
      this.line++
      this.column = 1
    } else {
      this.column++
    }
    return char
  }

  private skipWhitespace() {
    while (!this.isAtEnd() && /[ \t\r]/.test(this.peek())) {
      this.advance()
    }
  }

  private readNumber(): Token {
    const start = this.pos
    const startColumn = this.column

    // Check for hexadecimal (0x or 0X)
    if (this.peek() === '0' && this.pos + 1 < this.input.length) {
      const nextChar = this.input[this.pos + 1]
      if (nextChar === 'x' || nextChar === 'X') {
        this.advance() // consume '0'
        this.advance() // consume 'x' or 'X'

        // Read hexadecimal digits
        while (!this.isAtEnd() && /[0-9a-fA-F]/.test(this.peek())) {
          this.advance()
        }

        return {
          type: TokenType.Number,
          value: this.input.slice(start, this.pos),
          line: this.line,
          column: startColumn
        }
      }
    }

    // Read decimal part (integer digits)
    while (!this.isAtEnd() && /\d/.test(this.peek())) {
      this.advance()
    }

    // Check for decimal point
    if (
      !this.isAtEnd() &&
      this.peek() === '.' &&
      this.pos + 1 < this.input.length &&
      /\d/.test(this.input[this.pos + 1])
    ) {
      this.advance() // consume '.'

      // Read fractional part
      while (!this.isAtEnd() && /\d/.test(this.peek())) {
        this.advance()
      }
    }

    // Check for scientific notation (e or E)
    if (!this.isAtEnd() && /[eE]/.test(this.peek())) {
      this.advance() // consume 'e' or 'E'

      // Optional sign after e/E
      if (!this.isAtEnd() && /[+-]/.test(this.peek())) {
        this.advance()
      }

      // Exponent digits (required)
      if (this.isAtEnd() || !/\d/.test(this.peek())) {
        throw new LexerError(
          `Invalid scientific notation: missing exponent digits`,
          this.line,
          this.column
        )
      }

      while (!this.isAtEnd() && /\d/.test(this.peek())) {
        this.advance()
      }
    }

    return {
      type: TokenType.Number,
      value: this.input.slice(start, this.pos),
      line: this.line,
      column: startColumn
    }
  }

  private readIdentifier(): Token {
    // UTF-8 identifier support
    const start = this.pos
    const startColumn = this.column
    while (!this.isAtEnd() && /[\p{L}\p{N}_]/u.test(this.peek())) {
      this.advance()
    }
    const value = this.input.slice(start, this.pos)
    return {
      type: keywords.has(value) ? TokenType.Keyword : TokenType.Identifier,
      value,
      line: this.line,
      column: startColumn
    }
  }

  private readString(): Token {
    const quoteType = this.advance() // Consume the opening quote
    const startColumn = this.column - 1
    let value = ''
    while (!this.isAtEnd() && this.peek() !== quoteType) {
      if (this.peek() === '\\') {
        this.advance() // Consume the backslash
        if (!this.isAtEnd()) {
          const escapeChar = this.advance()
          switch (escapeChar) {
            case 'n':
              value += '\n'
              break
            case 't':
              value += '\t'
              break
            case 'r':
              value += '\r'
              break
            case '"':
              value += '"'
              break
            case "'":
              value += "'"
              break
            case '\\':
              value += '\\'
              break
            case 'u': {
              let hex = ''
              for (let i = 0; i < 4; i++) {
                if (this.isAtEnd() || !/[0-9a-fA-F]/.test(this.peek())) {
                  throw new LexerError(
                    `Invalid Unicode escape sequence`,
                    this.line,
                    this.column
                  )
                }
                hex += this.advance()
              }
              value += String.fromCharCode(parseInt(hex, 16))
              break
            }
            default:
              value += escapeChar
              break // Unknown escape, just add the char
          }
        }
      } else {
        value += this.advance()
      }
    }
    if (this.isAtEnd()) {
      throw new LexerError(`Unterminated string`, this.line, startColumn)
    }
    this.advance() // Consume the closing quote
    return {
      type: TokenType.String,
      value,
      line: this.line,
      column: startColumn
    }
  }

  private readSingleLineComment(): Token {
    const startColumn = this.column
    this.advance() // Consume first /
    this.advance() // Consume second /

    let value = ''
    while (!this.isAtEnd() && this.peek() !== '\n') {
      value += this.advance()
    }

    return {
      type: TokenType.Comment,
      value: '//' + value,
      line: this.line,
      column: startColumn
    }
  }

  private readMultiLineComment(): Token {
    const startColumn = this.column
    const startLine = this.line
    this.advance() // Consume first /
    this.advance() // Consume *

    let value = ''
    let nestingLevel = 1 // Support nested comments

    while (!this.isAtEnd() && nestingLevel > 0) {
      const char = this.peek()

      if (
        char === '/' &&
        this.pos + 1 < this.input.length &&
        this.input[this.pos + 1] === '*'
      ) {
        // Found nested comment start
        value += this.advance() // /
        value += this.advance() // *
        nestingLevel++
      } else if (
        char === '*' &&
        this.pos + 1 < this.input.length &&
        this.input[this.pos + 1] === '/'
      ) {
        // Found comment end
        value += this.advance() // *
        value += this.advance() // /
        nestingLevel--
      } else {
        value += this.advance()
      }
    }

    if (nestingLevel > 0) {
      throw new LexerError(
        `Unterminated multi-line comment`,
        startLine,
        startColumn
      )
    }

    return {
      type: TokenType.Comment,
      value: '/*' + value,
      line: startLine,
      column: startColumn
    }
  }

  private readOperatorOrPunctuation(): Token {
    const startColumn = this.column
    let value = this.advance()
    // Check for multi-character operators (greedy matching)
    while (!this.isAtEnd()) {
      const nextChar = this.peek()
      const potentialOp = value + nextChar
      if (operators.has(potentialOp)) {
        value = potentialOp
        this.advance()
      } else {
        break
      }
    }
    if (operators.has(value)) {
      return {
        type: TokenType.Operator,
        value,
        line: this.line,
        column: startColumn
      }
    } else if (punctuation.has(value)) {
      return {
        type: TokenType.Punctuation,
        value,
        line: this.line,
        column: startColumn
      }
    } else {
      throw new LexerError(
        `Unknown operator or punctuation '${value}'`,
        this.line,
        startColumn
      )
    }
  }

  public next(): Token {
    this.skipWhitespace()
    if (this.isAtEnd()) {
      return {
        type: TokenType.Eol,
        value: '\n',
        line: this.line,
        column: this.column
      }
    }
    const char = this.peek()

    // Handle newlines and semicolons as statement separators
    if (char === '\n' || char === ';') {
      const res = {
        type: TokenType.Eol,
        value: char,
        line: this.line,
        column: this.column
      }
      this.advance()
      return res
    }

    // Check for comments first
    if (char === '/') {
      if (this.pos + 1 < this.input.length) {
        const nextChar = this.input[this.pos + 1]
        if (nextChar === '/') {
          return this.readSingleLineComment()
        }
        if (nextChar === '*') {
          return this.readMultiLineComment()
        }
      }
      // If not a comment, fall through to operator handling
    }

    if (/\d/.test(char)) {
      return this.readNumber()
    }
    if (/[\p{L}_]/u.test(char)) {
      // Unicode letter or underscore
      return this.readIdentifier()
    }
    if (char === '"' || char === "'") {
      return this.readString()
    }
    if (operators.has(char) || punctuation.has(char)) {
      return this.readOperatorOrPunctuation()
    }

    // Skip unexpected character and throw error
    const errorLine = this.line
    const errorColumn = this.column
    this.advance() // Skip the unexpected character
    throw new LexerError(
      `Unexpected character '${char}'`,
      errorLine,
      errorColumn
    )
  }
}
