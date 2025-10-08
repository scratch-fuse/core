import { Token, TokenType, Lexer } from './lexer'
import { ErrorList, sanitize } from '@scratch-fuse/utility'

// AST Node Types
export interface ASTNode {
  type: string
  line: number
  column: number
}

export class ParserError extends Error {
  constructor(
    message: string,
    public line: number,
    public column: number
  ) {
    super(`${message} at ${line}:${column}`)
    this.name = 'ParserError'
  }
}

// Expressions
export interface Expression extends ASTNode {}

export interface LiteralExpression extends Expression {
  type: 'Literal'
  value: string | number | boolean
  raw: string
}

export interface IdentifierExpression extends Expression {
  type: 'Identifier'
  name: string
}

export interface BinaryExpression extends Expression {
  type: 'BinaryExpression'
  left: Expression
  operator: string
  right: Expression
}

export interface UnaryExpression extends Expression {
  type: 'UnaryExpression'
  operator: string
  operand: Expression
}

export interface CallExpression extends Expression {
  type: 'CallExpression'
  callee: Expression
  arguments: Expression[]
  then?: BlockStatement // Optional then block for calls followed by block statements
}

export interface MemberExpression extends Expression {
  type: 'MemberExpression'
  object: Expression
  property: Expression
  computed: boolean // true for obj[prop], false for obj.prop
}

export interface ArrayExpression extends Expression {
  type: 'ArrayExpression'
  elements: Expression[]
}

// Statements
export interface Statement extends ASTNode {}

export interface NoopStatement extends Statement {
  type: 'NoopStatement'
}
export interface ExpressionStatement extends Statement {
  type: 'ExpressionStatement'
  expression: Expression
}

export interface VariableDeclaration extends Statement {
  type: 'VariableDeclaration'
  name: string
  isGlobal: boolean
  initializer: Expression
}

export interface AssignmentStatement extends Statement {
  type: 'AssignmentStatement'
  left: Expression
  operator: string
  right: Expression
}

export interface IncrementStatement extends Statement {
  type: 'IncrementStatement'
  operator: string
  target: Expression
}

export interface IfStatement extends Statement {
  type: 'IfStatement'
  condition: Expression
  then: Statement
  else?: Statement
}

export interface WhileStatement extends Statement {
  type: 'WhileStatement'
  condition: Expression
  body: Statement
}

export interface ForStatement extends Statement {
  // for (init; condition; increment) { body } // C-style for, syntactic sugar of while
  type: 'ForStatement'
  init?: AssignmentStatement | IncrementStatement
  condition?: Expression
  increment?: AssignmentStatement | IncrementStatement
  body: Statement
}

export interface LoopStatement extends Statement {
  type: 'LoopStatement'
  body: Statement
}

export interface ReturnStatement extends Statement {
  type: 'ReturnStatement'
  value?: Expression
}

export interface BlockStatement extends Statement {
  type: 'BlockStatement'
  body: Statement[]
}

export interface DecoratorStatement extends Statement {
  type: 'DecoratorStatement'
  name: IdentifierExpression
  arguments: LiteralExpression[]
  target: Statement
}

// Function and Type Definitions
export interface Parameter {
  name: IdentifierExpression
  type: IdentifierExpression
}

export interface FunctionDeclaration extends Statement {
  type: 'FunctionDeclaration'
  name: IdentifierExpression
  parameters: Parameter[]
  returnType: IdentifierExpression
  once: boolean
  body: BlockStatement
}

// Namespace and Event Handling
export interface NamespaceDeclaration extends Statement {
  type: 'NamespaceDeclaration'
  name: string
  body: NamespaceBody
}

export interface NamespaceBody extends ASTNode {
  type: 'NamespaceBody'
  properties: NamespaceProperty[]
}

export interface NamespaceProperty extends ASTNode {
  type: 'NamespaceProperty'
  key: string
  value: any // Can be objects, arrays, etc.
}

// Program root
export interface Program extends ASTNode {
  type: 'Program'
  body: Statement[]
}

// Parser Class
export class Parser {
  private previousToken: Token | null = null
  private currentToken: Token | null = null
  private nextToken: Token | null = null

  constructor(private lexer: Lexer) {
    // Initialize: load first token into current, second into next
    this.loadNextToken() // Load first token into nextToken
    this.currentToken = this.nextToken // Move to currentToken
    this.loadNextToken() // Load second token into nextToken
  }

  public reset(lexer?: Lexer) {
    this.previousToken = null
    this.currentToken = null
    this.nextToken = null
    if (lexer) {
      this.lexer = lexer
    }
    // Reset lexer to beginning
    this.lexer.reset()
    // Initialize: load first token into current, second into next
    this.loadNextToken() // Load first token into nextToken
    this.currentToken = this.nextToken // Move to currentToken
    this.loadNextToken() // Load second token into nextToken
  }

  private loadNextToken(): void {
    // Skip comments and load next meaningful token
    do {
      if (this.lexer.isAtEnd()) {
        this.nextToken = null
        return
      }
      this.nextToken = this.lexer.next()
    } while (
      this.nextToken.type === TokenType.Comment ||
      (this.nextToken.type === TokenType.Eol && this.nextToken.value !== ';')
    )
  }

  private isEof(): boolean {
    return this.currentToken === null && this.lexer.isAtEnd()
  }

  private peek(): Token {
    if (this.isEof()) {
      // Return a dummy EOF token instead of throwing
      return {
        type: TokenType.Eol,
        value: '',
        line: 0,
        column: 0
      }
    }
    return this.currentToken!
  }

  private previous(): Token {
    if (!this.previousToken) {
      throw new ParserError(
        'Cannot get previous token at beginning of input',
        0,
        0
      )
    }
    return this.previousToken
  }

  private advance(): Token {
    const prev = this.currentToken
    if (!this.isEof()) {
      this.previousToken = this.currentToken
      this.currentToken = this.nextToken
      this.loadNextToken()
    }
    // Return the token that was current before advancing
    // This handles the case when previousToken is null at the beginning
    return prev || this.peek()
  }

  private skipComments(): void {
    // Comments are already skipped in loadNextToken
    // This method is kept for compatibility but does nothing
  }

  private _matchBase(type: TokenType, values: string[]): boolean {
    if (this._checkBase(type, values)) {
      this.advance()
      return true
    }
    return false
  }

  private match(...types: TokenType[]): boolean {
    if (this.check(...types)) {
      this.advance()
      return true
    }
    return false
  }

  private matchOperator(...values: string[]): boolean {
    return this._matchBase(TokenType.Operator, values)
  }

  private matchIdentifier(...values: string[]): boolean {
    return this._matchBase(TokenType.Identifier, values)
  }

  private matchKeyword(...values: string[]): boolean {
    return this._matchBase(TokenType.Keyword, values)
  }

  private matchPunctuation(...values: string[]): boolean {
    return this._matchBase(TokenType.Punctuation, values)
  }

  private _checkBase(type: TokenType, values: string[]): boolean {
    if (!this.check(type)) return false
    for (const value of values) {
      if (this.peek().value === value) {
        return true
      }
    }
    return false
  }

  // private checkKeyword(...values: string[]): boolean {
  //   return this._checkBase(TokenType.Keyword, values);
  // }

  // private checkOperator(...values: string[]): boolean {
  //   return this._checkBase(TokenType.Operator, values);
  // }

  private check(...types: TokenType[]): boolean {
    if (this.isEof()) return false
    for (const type of types) {
      if (this.peek().type === type) {
        return true
      }
    }
    return false
  }

  private synchronize(): void {
    if (!this.isEof()) {
      this.advance()
    }
    while (!this.isEof()) {
      // Check if we can safely call previous()
      if (this.previousToken && this.previous().type === TokenType.Eol) return

      if (!this.isEof()) {
        switch (this.peek().type) {
          case TokenType.Keyword:
            const keyword = this.peek().value
            if (
              [
                'fn',
                'let',
                'global',
                'if',
                'while',
                'for',
                'until',
                'loop',
                'return'
              ].includes(keyword)
            ) {
              return
            }
            break
        }
      }
      this.advance()
    }
  }

  private consume(type: TokenType, message: string): Token {
    if (this.check(type)) {
      return this.advance()
    }
    const token = this.peek()
    throw new ParserError(
      `${message}. Got ${token.type} (${token.value})`,
      token.line,
      token.column
    )
  }

  private _consumeBase(type: TokenType, value: string, message: string): Token {
    if (this.isEof()) {
      throw new ParserError(`${message}. Unexpected end of input`, 0, 0)
    }
    const v = this.peek()
    if (!this.check(type) || v.value !== value) {
      throw new ParserError(`${message}. Got ${v.value}`, v.line, v.column)
    }
    this.advance()
    return v
  }

  private consumeKeyword(value: string, message: string): Token {
    return this._consumeBase(TokenType.Keyword, value, message)
  }

  private consumeEol(value: string, message: string): Token {
    return this._consumeBase(TokenType.Eol, value, message)
  }

  private consumeOperator(value: string, message: string): Token {
    return this._consumeBase(TokenType.Operator, value, message)
  }

  private consumeIdentifier(value: string, message: string): Token {
    return this._consumeBase(TokenType.Identifier, value, message)
  }

  private consumePunctuation(value: string, message: string): Token {
    return this._consumeBase(TokenType.Punctuation, value, message)
  }

  // Expression parsing
  private parseExpression(): Expression {
    return this.parseLogicalOr()
  }

  private parseLogicalOr(): Expression {
    let expr = this.parseLogicalAnd()

    while (this.matchOperator('||')) {
      const operator = this.previous().value
      const right = this.parseLogicalAnd()
      expr = {
        type: 'BinaryExpression',
        left: expr,
        operator,
        right,
        line: expr.line,
        column: expr.column
      } as BinaryExpression
    }

    return expr
  }

  private parseLogicalAnd(): Expression {
    let expr = this.parseEquality()

    while (this.matchOperator('&&')) {
      const operator = this.previous().value
      const right = this.parseEquality()
      expr = {
        type: 'BinaryExpression',
        left: expr,
        operator,
        right,
        line: expr.line,
        column: expr.column
      } as BinaryExpression
    }

    return expr
  }

  private parseEquality(): Expression {
    let expr = this.parseComparison()

    while (this.matchOperator('==', '!=')) {
      const op = this.previous().value
      const right = this.parseComparison()
      expr = {
        type: 'BinaryExpression',
        left: expr,
        operator: op,
        right,
        line: expr.line,
        column: expr.column
      } as BinaryExpression
    }

    return expr
  }

  private parseComparison(): Expression {
    let expr = this.parseTerm()

    while (this.matchOperator('>', '>=', '<', '<=')) {
      const op = this.previous().value
      const right = this.parseTerm()
      expr = {
        type: 'BinaryExpression',
        left: expr,
        operator: op,
        right,
        line: expr.line,
        column: expr.column
      } as BinaryExpression
    }

    return expr
  }

  private parseTerm(): Expression {
    let expr = this.parseFactor()

    while (this.matchOperator('+', '-', '..')) {
      const op = this.previous().value
      const right = this.parseFactor()
      expr = {
        type: 'BinaryExpression',
        left: expr,
        operator: op,
        right,
        line: expr.line,
        column: expr.column
      } as BinaryExpression
    }

    return expr
  }

  private parseFactor(): Expression {
    let expr = this.parseUnary()

    while (this.matchOperator('*', '/', '%')) {
      const op = this.previous().value
      const right = this.parseUnary()
      expr = {
        type: 'BinaryExpression',
        left: expr,
        operator: op,
        right,
        line: expr.line,
        column: expr.column
      } as BinaryExpression
    }

    return expr
  }

  private parseUnary(): Expression {
    if (this.matchOperator('!', '-', '+')) {
      const op = this.previous().value
      const operand = this.parseUnary()
      if (operand.type === 'Literal' && (op === '+' || op === '-')) {
        // Constant folding for unary + and -
        if (typeof (operand as LiteralExpression).value === 'number') {
          const foldedValue =
            op === '-'
              ? -(operand as LiteralExpression).value
              : +(operand as LiteralExpression).value
          return {
            type: 'Literal',
            value: foldedValue,
            raw: foldedValue.toString(),
            line: operand.line,
            column: operand.column
          } as LiteralExpression
        }
      }
      return {
        type: 'UnaryExpression',
        operator: op,
        operand,
        line: this.previous().line,
        column: this.previous().column
      } as UnaryExpression
    }

    return this.parseCall()
  }

  private parseCall(): Expression {
    let expr = this.parsePrimary()

    while (true) {
      if (this.matchPunctuation('(')) {
        expr = this.finishCall(expr)
        // Check for then block after call
        if (this.check(TokenType.Punctuation) && this.peek().value === '{') {
          const thenBlock = this.parseBlockStatement()
          ;(expr as CallExpression).then = thenBlock
        }
      } else if (this.matchOperator('.')) {
        const name = this.consume(
          TokenType.Identifier,
          "Expected property name after '.'"
        )
        expr = {
          type: 'MemberExpression',
          object: expr,
          property: {
            type: 'Identifier',
            name: name.value,
            line: name.line,
            column: name.column
          } as IdentifierExpression,
          computed: false,
          line: expr.line,
          column: expr.column
        } as MemberExpression
      } else if (this.matchPunctuation('[')) {
        const property = this.parseExpression()
        this.consumePunctuation(']', "Expected ']' after array access")
        expr = {
          type: 'MemberExpression',
          object: expr,
          property,
          computed: true,
          line: expr.line,
          column: expr.column
        } as MemberExpression
      } else if (
        this.check(TokenType.Punctuation) &&
        this.peek().value === '{'
      ) {
        // Just then, no arguments (don't consume the '{', let parseBlockStatement do it)
        const thenBlock = this.parseBlockStatement()
        expr = {
          type: 'CallExpression',
          callee: expr,
          arguments: [],
          then: thenBlock,
          line: expr.line,
          column: expr.column
        } as CallExpression
      } else {
        break
      }
    }

    return expr
  }

  private finishCall(callee: Expression): CallExpression {
    const args: Expression[] = []

    if (!this.check(TokenType.Punctuation) || this.peek().value !== ')') {
      do {
        args.push(this.parseExpression())
      } while (this.matchPunctuation(','))
    }

    this.consumePunctuation(')', "Expected ')' after arguments")

    return {
      type: 'CallExpression',
      callee,
      arguments: args,
      line: callee.line,
      column: callee.column
    } as CallExpression
  }

  private parsePrimary(): Expression {
    // Check for boolean literals
    if (this.matchKeyword('true')) {
      const keyword = this.previous()
      return {
        type: 'Literal',
        value: true,
        raw: 'true',
        line: keyword.line,
        column: keyword.column
      } as LiteralExpression
    }

    if (this.matchKeyword('false')) {
      const keyword = this.previous()
      return {
        type: 'Literal',
        value: false,
        raw: 'false',
        line: keyword.line,
        column: keyword.column
      } as LiteralExpression
    }

    if (this.match(TokenType.Number)) {
      const token = this.previous()
      return {
        type: 'Literal',
        value: Number(token.value),
        raw: token.value,
        line: token.line,
        column: token.column
      } as LiteralExpression
    }

    if (this.match(TokenType.String)) {
      const token = this.previous()
      return {
        type: 'Literal',
        value: token.value,
        raw: `"${token.value}"`,
        line: token.line,
        column: token.column
      } as LiteralExpression
    }

    if (this.matchIdentifier('Infinity', 'NaN')) {
      const token = this.previous()
      return {
        type: 'Literal',
        value: token.value === 'Infinity' ? Infinity : NaN,
        raw: token.value,
        line: token.line,
        column: token.column
      } as LiteralExpression
    }

    if (this.match(TokenType.Identifier)) {
      const token = this.previous()
      return {
        type: 'Identifier',
        name: token.value,
        line: token.line,
        column: token.column
      } as IdentifierExpression
    }

    if (this.matchPunctuation('(')) {
      const expr = this.parseExpression()
      this.consumePunctuation(')', "Expected ')' after expression")
      return expr
    }

    if (this.matchPunctuation('[')) {
      return this.parseArrayLiteral()
    }

    const token = this.peek()
    throw new ParserError(
      `Unexpected token ${token.type}: ${token.value}`,
      token.line,
      token.column
    )
  }

  private parseArrayLiteral(): ArrayExpression {
    const start = this.previous()
    const elements: Expression[] = []

    if (!this.check(TokenType.Punctuation) || this.peek().value !== ']') {
      do {
        elements.push(this.parseExpression())
      } while (this.matchPunctuation(','))
    }

    this.consumePunctuation(']', "Expected ']' after array elements")

    return {
      type: 'ArrayExpression',
      elements,
      line: start.line,
      column: start.column
    } as ArrayExpression
  }

  // Statement parsing
  private parseStatement(): Statement {
    try {
      if (this.match(TokenType.Keyword)) {
        const keyword = this.previous().value
        switch (keyword) {
          case 'let':
          case 'global':
            return this.parseVariableDeclaration(keyword === 'global')
          case 'if':
            return this.parseIfStatement()
          case 'while':
            return this.parseWhileStatement()
          case 'for':
            return this.parseForStatement()
          case 'loop':
            return this.parseLoopStatement()
          case 'return':
            return this.parseReturnStatement()
          case 'fn':
            return this.parseFunctionDeclaration()
          case 'namespace':
            return this.parseNamespaceDeclaration()
        }
      }

      if (this.matchPunctuation('@')) {
        return this.parseDecoratorStatement()
      }

      if (this.check(TokenType.Eol) && this.peek().value === ';') {
        const token = this.advance()
        return {
          type: 'NoopStatement',
          line: token.line,
          column: token.column
        } as NoopStatement
      }

      // Check for prefix increment/decrement or assignment
      if (this.matchOperator('++', '--')) {
        const operator = this.previous().value
        const expr = this.parseExpression()
        return {
          type: 'IncrementStatement',
          operator,
          target: expr,
          line: expr.line,
          column: expr.column
        } as IncrementStatement
      }

      // Parse expression, then check for assignment or postfix increment
      const expr = this.parseExpression()

      if (this.matchOperator('=', '+=', '-=', '*=', '/=', '%=', '..=')) {
        const operator = this.previous().value
        const right = this.parseExpression()
        return {
          type: 'AssignmentStatement',
          left: expr,
          operator,
          right,
          line: expr.line,
          column: expr.column
        } as AssignmentStatement
      } else if (this.matchOperator('++', '--')) {
        const operator = this.previous().value
        return {
          type: 'IncrementStatement',
          operator,
          target: expr as IdentifierExpression,
          line: expr.line,
          column: expr.column
        } as IncrementStatement
      }

      // Just an expression statement
      return {
        type: 'ExpressionStatement',
        expression: expr,
        line: expr.line,
        column: expr.column
      } as ExpressionStatement
    } catch (error) {
      this.synchronize()
      throw error
    }
  }

  private parseDecoratorStatement(): DecoratorStatement {
    const nameToken = this.consume(
      TokenType.Identifier,
      'Expected decorator name after @'
    )
    const name: IdentifierExpression = {
      type: 'Identifier',
      name: nameToken.value,
      line: nameToken.line,
      column: nameToken.column
    } as IdentifierExpression

    const args: LiteralExpression[] = []
    if (this.matchPunctuation('(')) {
      if (!this.check(TokenType.Punctuation) || this.peek().value !== ')') {
        do {
          if (this.match(TokenType.Number)) {
            const token = this.previous()
            args.push({
              type: 'Literal',
              value: Number(token.value),
              raw: token.value,
              line: token.line,
              column: token.column
            } as LiteralExpression)
          } else if (this.match(TokenType.String)) {
            const token = this.previous()
            args.push({
              type: 'Literal',
              value: token.value,
              raw: sanitize(token.value),
              line: token.line,
              column: token.column
            } as LiteralExpression)
          } else if (this.match(TokenType.Keyword)) {
            const token = this.previous()
            if (token.value === 'true' || token.value === 'false') {
              args.push({
                type: 'Literal',
                value: token.value === 'true',
                raw: token.value,
                line: token.line,
                column: token.column
              } as LiteralExpression)
            } else {
              throw new ParserError(
                `Invalid decorator argument ${token.value}`,
                token.line,
                token.column
              )
            }
          } else {
            const token = this.peek()
            throw new ParserError(
              `Invalid decorator argument`,
              token.line,
              token.column
            )
          }
        } while (this.matchPunctuation(','))
      }
      this.consumePunctuation(')', "Expected ')' after decorator arguments")
    }
    const decorated = this.parseStatement()

    return {
      type: 'DecoratorStatement',
      name,
      arguments: args,
      target: decorated,
      line: name.line,
      column: name.column
    } as DecoratorStatement
  }

  private parseVariableDeclaration(isGlobal: boolean): VariableDeclaration {
    const name = this.consume(
      TokenType.Identifier,
      'Expected variable name'
    ).value

    this.consumeOperator('=', 'Variable requires a initial value')

    const initializer = this.parseExpression()

    // this.consumeStatementTerminator()
    return {
      type: 'VariableDeclaration',
      name: name,
      isGlobal,
      initializer,
      line: this.previous().line,
      column: this.previous().column
    } as VariableDeclaration
  }

  private parseIfStatement(): IfStatement {
    this.consumePunctuation('(', "Expected '(' after 'if'")
    const condition = this.parseExpression()
    this.consumePunctuation(')', "Expected ')' after if condition")

    const then = this.parseStatementOrBlock()
    let elseClause: Statement | undefined

    if (this.matchKeyword('else')) {
      elseClause = this.parseStatementOrBlock()
    }

    return {
      type: 'IfStatement',
      condition,
      then,
      else: elseClause,
      line: condition.line,
      column: condition.column
    } as IfStatement
  }

  private parseWhileStatement(): WhileStatement {
    this.consumePunctuation('(', "Expected '(' after 'while'")
    const condition = this.parseExpression()
    this.consumePunctuation(')', "Expected ')' after while condition")
    const body = this.parseStatementOrBlock()

    return {
      type: 'WhileStatement',
      condition,
      body,
      line: condition.line,
      column: condition.column
    } as WhileStatement
  }

  private parseForStatement(): ForStatement {
    const start = this.previous()
    this.consumePunctuation('(', "Expected '(' after 'for'")

    // Parse init statement (can be variable declaration or assignment)
    let init: Statement | undefined
    if (!this.check(TokenType.Eol) || this.peek().value !== ';') {
      init = this.parseStatement()
      if (!['AssignmentStatement', 'IncrementStatement'].includes(init.type)) {
        throw new ParserError(
          'For loop init must be an assignment or increment statement',
          start.line,
          start.column
        )
      }
      this.consumeEol(';', "Expected ';' after for init")
    } else {
      this.advance() // consume ';'
    }

    // Parse condition expression
    let condition: Expression | undefined
    if (!this.check(TokenType.Eol) || this.peek().value !== ';') {
      condition = this.parseExpression()
      this.consumeEol(';', "Expected ';' after for condition")
    } else {
      this.advance() // consume ';'
    }

    // Parse increment statement
    let increment: Statement | undefined
    if (!this.check(TokenType.Eol) || this.peek().value !== ')') {
      increment = this.parseStatement()
      if (
        !['AssignmentStatement', 'IncrementStatement'].includes(increment.type)
      ) {
        throw new ParserError(
          'For loop increment must be an assignment or increment statement',
          start.line,
          start.column
        )
      }
    }

    this.consumePunctuation(')', "Expected ')' after for increment")
    const body = this.parseStatementOrBlock()

    return {
      type: 'ForStatement',
      init: init as AssignmentStatement | IncrementStatement | undefined,
      condition,
      increment: increment as
        | AssignmentStatement
        | IncrementStatement
        | undefined,
      body,
      line: start.line,
      column: start.column
    } as ForStatement
  }

  private parseLoopStatement(): LoopStatement {
    const body = this.parseStatementOrBlock()
    return {
      type: 'LoopStatement',
      body,
      line: body.line,
      column: body.column
    } as LoopStatement
  }

  private parseReturnStatement(): ReturnStatement {
    const start = this.previous()
    let value: Expression | undefined

    // More careful check for return value
    if (!this.isEof() && !this.check(TokenType.Eol)) {
      const currentToken = this.peek()
      // Only parse expression if we have a valid token that could start an expression
      if (
        currentToken.type !== TokenType.Punctuation ||
        (currentToken.value !== '}' && currentToken.value !== ')')
      ) {
        value = this.parseExpression()
      }
    }

    // this.consumeStatementTerminator()
    return {
      type: 'ReturnStatement',
      value,
      line: start.line,
      column: start.column
    } as ReturnStatement
  }

  private parseBlockStatement(): BlockStatement {
    const start = this.consumePunctuation('{', "Expected '{'")
    const statements: Statement[] = []

    while (!this.check(TokenType.Punctuation) || this.peek().value !== '}') {
      if (this.isEof()) {
        throw new ParserError(`Unterminated block`, start.line, start.column)
      }
      statements.push(this.parseStatement())
    }

    this.consumePunctuation('}', "Expected '}'")
    return {
      type: 'BlockStatement',
      body: statements,
      line: start.line,
      column: start.column
    } as BlockStatement
  }

  private parseExpressionStatement(): ExpressionStatement {
    const expr = this.parseExpression()
    // this.consumeStatementTerminator()
    return {
      type: 'ExpressionStatement',
      expression: expr,
      line: expr.line,
      column: expr.column
    } as ExpressionStatement
  }

  private parseStatementOrBlock(): Statement {
    // If we see a '{', parse as block statement
    if (this.check(TokenType.Punctuation) && this.peek().value === '{') {
      return this.parseBlockStatement()
    }
    // If we see a ';', parse as noop statement
    if (this.check(TokenType.Eol) && this.peek().value === ';') {
      const token = this.advance()
      return {
        type: 'NoopStatement',
        line: token.line,
        column: token.column
      } as NoopStatement
    }
    // Otherwise, parse as single statement
    return this.parseStatement()
  }

  // Function declaration parsing
  private parseFunctionDeclaration(): FunctionDeclaration {
    const token: Token = this.consume(
      TokenType.Identifier,
      'Expected function name'
    )
    const name: IdentifierExpression = {
      type: 'Identifier',
      name: token.value,
      line: token.line,
      column: token.column
    } as IdentifierExpression

    this.consumePunctuation('(', "Expected '(' after function name")
    const parameters: Parameter[] = []

    if (!this.check(TokenType.Punctuation) || this.peek().value !== ')') {
      do {
        const paramName = this.consume(
          TokenType.Identifier,
          'Expected parameter name'
        )

        this.consumePunctuation(':', "Expected ':' after parameter name")

        const paramType = this.consume(
          TokenType.Identifier,
          'Expected parameter type'
        )

        parameters.push({
          name: {
            type: 'Identifier',
            name: paramName.value,
            line: paramName.line,
            column: paramName.column
          } as IdentifierExpression,
          type: {
            type: 'Identifier',
            name: paramType.value,
            line: paramType.line,
            column: paramType.column
          } as IdentifierExpression
        })
      } while (this.matchPunctuation(','))
    }

    this.consumePunctuation(')', "Expected ')' after parameters")

    // Check for 'once' specifier
    let once = false
    if (this.matchKeyword('once')) {
      once = true
    }

    // Check for return type

    this.consumeOperator('->', 'Expected -> before return type')

    const returnType = this.consume(
      TokenType.Identifier,
      'Expected return type'
    )

    const body = this.parseBlockStatement()

    return {
      type: 'FunctionDeclaration',
      name,
      parameters,
      returnType: {
        type: 'Identifier',
        name: returnType.value,
        line: returnType.line,
        column: returnType.column
      } as IdentifierExpression,
      once,
      body,
      line: this.previous().line,
      column: this.previous().column
    } as FunctionDeclaration
  }

  // Namespace and event parsing
  private parseNamespaceDeclaration(): NamespaceDeclaration {
    const name = this.consume(
      TokenType.Identifier,
      'Expected namespace name'
    ).value
    this.consumeOperator('=', "Expected '=' after namespace name")
    this.consumePunctuation('{', "Expected '{' after '='")

    const properties: NamespaceProperty[] = []

    while (!this.check(TokenType.Punctuation) || this.peek().value !== '}') {
      if (this.isEof()) {
        throw new ParserError(
          'Unterminated namespace',
          this.previous().line,
          this.previous().column
        )
      }

      // Skip any newlines before property
      this.skipComments()
      if (this.check(TokenType.Punctuation) && this.peek().value === '}') {
        break
      }

      const key = this.consume(
        TokenType.Identifier,
        'Expected property name'
      ).value
      this.consumeOperator('=', "Expected '=' after property name")

      // Parse the property value (can be complex JSON-like structure)
      const value = this.parseNamespaceValue()

      properties.push({
        type: 'NamespaceProperty',
        key,
        value,
        line: this.previous().line,
        column: this.previous().column
      })
    }

    this.consumePunctuation('}', "Expected '}' after namespace body")

    return {
      type: 'NamespaceDeclaration',
      name,
      body: {
        type: 'NamespaceBody',
        properties,
        line: this.previous().line,
        column: this.previous().column
      },
      line: this.previous().line,
      column: this.previous().column
    } as NamespaceDeclaration
  }

  private parseNamespaceValue(): any {
    if (this.matchPunctuation('{')) {
      // Parse object
      const obj: any = {}

      while (!this.check(TokenType.Punctuation) || this.peek().value !== '}') {
        // Skip any newlines
        this.skipComments()
        if (this.check(TokenType.Punctuation) && this.peek().value === '}') {
          break
        }

        const key = this.consume(
          TokenType.String,
          'Expected property key'
        ).value
        this.consumePunctuation(':', "Expected ':' after key")
        const value = this.parseNamespaceValue()
        obj[key] = value

        // Optional comma and newlines
        if (this.check(TokenType.Punctuation) && this.peek().value === ',') {
          this.advance()
        }
        this.skipComments() // Skip newlines after comma
      }

      this.consumePunctuation('}', "Expected '}' after object")
      return obj
    }

    if (this.matchPunctuation('[')) {
      // Parse array
      const arr: any[] = []

      while (!this.check(TokenType.Punctuation) || this.peek().value !== ']') {
        arr.push(this.parseNamespaceValue())

        if (this.check(TokenType.Punctuation) && this.peek().value === ',') {
          this.advance()
        }
      }

      this.consumePunctuation(']', "Expected ']' after array")
      return arr
    }

    if (this.match(TokenType.String)) {
      return this.previous().value
    }

    if (this.match(TokenType.Number)) {
      return parseFloat(this.previous().value)
    }

    if (this.match(TokenType.Keyword)) {
      const keyword = this.previous().value
      if (keyword === 'true') return true
      if (keyword === 'false') return false
    }

    if (this.matchKeyword('true')) {
      return true
    }

    if (this.matchKeyword('false')) {
      return false
    }

    if (this.matchIdentifier('null')) {
      return null
    }

    const token = this.peek()
    throw new ParserError(
      `Unexpected token in namespace value: ${token.value}`,
      token.line,
      token.column
    )
  }

  // Error handling and recovery
  private parseDeclaration(): Statement {
    try {
      // Check for namespace declaration: identifier = {
      // We need lookahead to distinguish from regular statements
      if (this.check(TokenType.Identifier)) {
        // Peek ahead to see if this looks like: name = {
        // Since we can't backtrack, we check before consuming
        const currentPeek = this.peek()

        // Try to determine if this is a namespace by looking at the pattern
        // For now, let's be conservative and require 'namespace' keyword
        // This avoids the need for complex lookahead
      }

      return this.parseStatement()
    } catch (error) {
      this.synchronize()
      throw error
    }
  }

  // Main parsing method
  public parse(): Program {
    // Reset parser state
    this.reset()

    // Parse program
    const statements: Statement[] = []
    const errors: Error[] = []

    while (!this.isEof()) {
      try {
        if (this.check(TokenType.Eol)) {
          this.advance()
          continue
        }
        const stmt = this.parseDeclaration()
        statements.push(stmt)
      } catch (error) {
        errors.push(error as Error)
        this.synchronize()
      }
    }

    if (errors.length > 0) {
      throw new ErrorList(errors)
    }

    return {
      type: 'Program',
      body: statements,
      line: 1,
      column: 1
    } as Program
  }

  // Additional method to parse a single expression (useful for testing)
  public parseExpressionOnly(): Expression {
    return this.parseExpression()
  }
}

export function toSource(node: ASTNode, indent = 2, semi = false): string {
  const nl = indent > 0 ? '\n' : ''
  const sp = indent > 0 ? ' ' : ''
  const ksp = ' ' // keyword space - always needed even when indent = 0
  const indentStr = indent > 0 ? ' '.repeat(indent) : ''
  const useSemi = indent === 0 || semi // always use semicolons when minified or when semi is true

  // Operator precedence (higher number = higher precedence)
  function getPrecedence(node: ASTNode): number {
    if (node.type === 'BinaryExpression') {
      const op = (node as BinaryExpression).operator
      if (op === '||') return 1
      if (op === '&&') return 2
      if (op === '==' || op === '!=') return 3
      if (op === '>' || op === '>=' || op === '<' || op === '<=') return 4
      if (op === '+' || op === '-' || op === '..') return 5
      if (op === '*' || op === '/' || op === '%') return 6
    }
    if (node.type === 'UnaryExpression') return 7
    return 100 // Other expressions have highest precedence (no parens needed)
  }

  function needsParens(
    parent: ASTNode,
    child: ASTNode,
    isLeft: boolean
  ): boolean {
    const parentPrec = getPrecedence(parent)
    const childPrec = getPrecedence(child)

    // Higher precedence child doesn't need parens
    if (childPrec > parentPrec) return false

    // Lower precedence child always needs parens
    if (childPrec < parentPrec) return true

    // Same precedence: for binary operators, right side needs parens if not associative
    // Most binary operators are left-associative, so right side needs parens
    if (
      parent.type === 'BinaryExpression' &&
      child.type === 'BinaryExpression'
    ) {
      return !isLeft
    }

    return false
  }

  function indentLines(str: string, level: number): string {
    if (indent === 0) return str
    const prefix = indentStr.repeat(level)
    return str
      .split('\n')
      .map(line => (line ? prefix + line : ''))
      .join('\n')
  }

  function formatBlock(statements: Statement[], level: number): string {
    if (statements.length === 0) return ''
    return statements
      .map(stmt => {
        const source = toSourceImpl(stmt, level)
        // Only add indentation if the statement doesn't start with whitespace
        // (which would indicate it's already been indented by a nested block)
        if (indent === 0) return source
        return indentStr.repeat(level) + source
      })
      .join(nl)
  }

  function toSourceImpl(node: ASTNode, level: number = 0): string {
    switch (node.type) {
      // Expressions
      case 'Literal': {
        const lit = node as LiteralExpression
        if (typeof lit.value === 'string') {
          return lit.raw
        }
        return sanitize(lit.value)
      }

      case 'Identifier': {
        const id = node as IdentifierExpression
        return id.name
      }

      case 'BinaryExpression': {
        const bin = node as BinaryExpression
        let left = toSourceImpl(bin.left, level)
        let right = toSourceImpl(bin.right, level)

        // Add parentheses if needed based on precedence
        if (needsParens(bin, bin.left, true)) {
          left = `(${left})`
        }
        if (needsParens(bin, bin.right, false)) {
          right = `(${right})`
        }

        return `${left}${sp}${bin.operator}${sp}${right}`
      }

      case 'UnaryExpression': {
        const unary = node as UnaryExpression
        let operand = toSourceImpl(unary.operand, level)

        // Add parentheses if operand is a binary expression with lower precedence
        if (unary.operand.type === 'BinaryExpression') {
          operand = `(${operand})`
        }

        return `${unary.operator}${operand}`
      }

      case 'CallExpression': {
        const call = node as CallExpression
        const callee = toSourceImpl(call.callee, level)
        const args = call.arguments
          .map(arg => toSourceImpl(arg, level))
          .join(`,${sp}`)
        let result = `${callee}${call.arguments.length === 0 && level === 0 ? `` : `(${args})`}`
        if (call.then) {
          result += `${sp}${toSourceImpl(call.then, level)}`
        }
        return result
      }

      case 'MemberExpression': {
        const member = node as MemberExpression
        const object = toSourceImpl(member.object, level)
        if (member.computed) {
          const property = toSourceImpl(member.property, level)
          return `${object}[${property}]`
        } else {
          const property = toSourceImpl(member.property, level)
          return `${object}.${property}`
        }
      }

      case 'ArrayExpression': {
        const arr = node as ArrayExpression
        const elements = arr.elements
          .map(el => toSourceImpl(el, level))
          .join(`,${sp}`)
        return `[${elements}]`
      }

      // Statements
      case 'NoopStatement': {
        return ';'
      }

      case 'ExpressionStatement': {
        const exprStmt = node as ExpressionStatement
        const expr = toSourceImpl(exprStmt.expression, level)
        return useSemi ? expr + ';' : expr
      }

      case 'VariableDeclaration': {
        const varDecl = node as VariableDeclaration
        const keyword = varDecl.isGlobal ? 'global' : 'let'
        const init = toSourceImpl(varDecl.initializer, level)
        const stmt = `${keyword}${ksp}${varDecl.name}${sp}=${sp}${init}`
        return useSemi ? stmt + ';' : stmt
      }

      case 'AssignmentStatement': {
        const assign = node as AssignmentStatement
        const left = toSourceImpl(assign.left, level)
        const right = toSourceImpl(assign.right, level)
        const stmt = `${left}${sp}${assign.operator}${sp}${right}`
        return useSemi ? stmt + ';' : stmt
      }

      case 'IncrementStatement': {
        const inc = node as IncrementStatement
        const target = toSourceImpl(inc.target, level)
        const stmt = `${target}${inc.operator}`
        return useSemi ? stmt + ';' : stmt
      }

      case 'IfStatement': {
        const ifStmt = node as IfStatement
        const condition = toSourceImpl(ifStmt.condition, level)
        const thenPart = toSourceImpl(ifStmt.then, level)
        let result = `if${sp}(${condition})${sp}${thenPart}`
        if (ifStmt.else) {
          const elsePart = toSourceImpl(ifStmt.else, level)
          result += `${sp}else${sp}${elsePart}`
        }
        return result
      }

      case 'WhileStatement': {
        const whileStmt = node as WhileStatement
        const condition = toSourceImpl(whileStmt.condition, level)
        const body = toSourceImpl(whileStmt.body, level)
        return `while${sp}(${condition})${sp}${body}`
      }

      case 'ForStatement': {
        const forStmt = node as ForStatement
        const init = forStmt.init
          ? toSourceImpl(forStmt.init, level).replace(/;$/, '')
          : ''
        const condition = forStmt.condition
          ? toSourceImpl(forStmt.condition, level)
          : ''
        const increment = forStmt.increment
          ? toSourceImpl(forStmt.increment, level).replace(/;$/, '')
          : ''
        const body = toSourceImpl(forStmt.body, level)
        return `for${sp}(${init};${sp}${condition};${sp}${increment})${sp}${body}`
      }

      case 'LoopStatement': {
        const loopStmt = node as LoopStatement
        const body = toSourceImpl(loopStmt.body, level)
        return `loop${sp}${body}`
      }

      case 'ReturnStatement': {
        const ret = node as ReturnStatement
        let stmt: string
        if (ret.value) {
          const value = toSourceImpl(ret.value, level)
          stmt = `return${ksp}${value}`
        } else {
          stmt = 'return'
        }
        return useSemi ? stmt + ';' : stmt
      }

      case 'BlockStatement': {
        const block = node as BlockStatement
        if (block.body.length === 0) {
          return `{}`
        }
        const body = formatBlock(block.body, level + 1)
        return `{${nl}${body}${nl}${indentLines('}', level)}`
      }

      case 'DecoratorStatement': {
        const decorator = node as DecoratorStatement
        const name = toSourceImpl(decorator.name, level)
        const args = decorator.arguments
          .map(arg => toSourceImpl(arg, level))
          .join(`,${sp}`)
        const target = toSourceImpl(decorator.target, level)
        if (decorator.arguments.length > 0) {
          return `@${name}(${args})${sp}${target}`
        }
        return `@${name}${ksp}${target}`
      }

      case 'FunctionDeclaration': {
        const fn = node as FunctionDeclaration
        const name = toSourceImpl(fn.name, level)
        const params = fn.parameters
          .map(
            p =>
              `${toSourceImpl(p.name, level)}:${sp}${toSourceImpl(p.type, level)}`
          )
          .join(`,${sp}`)
        const returnType = toSourceImpl(fn.returnType, level)
        const once = fn.once ? `${sp}once` : ''
        const body = toSourceImpl(fn.body, level)
        return `fn${ksp}${name}(${params})${once}${sp}->${sp}${returnType}${sp}${body}`
      }

      case 'NamespaceDeclaration': {
        const ns = node as NamespaceDeclaration
        const props = ns.body.properties
          .map(prop => {
            const value = formatNamespaceValue(prop.value, level + 1)
            const line = `${prop.key}${sp}=${sp}${value}`
            return indent === 0 ? line : indentStr.repeat(level + 1) + line
          })
          .join(nl)
        const closing = indent === 0 ? '}' : indentStr.repeat(level) + '}'
        return `namespace${ksp}${ns.name}${sp}=${sp}{${nl}${props}${nl}${closing}`
      }

      case 'Program': {
        const program = node as Program
        return formatBlock(program.body, 0)
      }

      default:
        return `/* Unknown node type: ${node.type} */`
    }
  }

  function formatNamespaceValue(value: any, level: number): string {
    if (value === null) return 'null'
    if (value === true) return 'true'
    if (value === false) return 'false'
    if (typeof value === 'number') return String(value)
    if (typeof value === 'string') {
      return `"${value.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`
    }
    if (Array.isArray(value)) {
      const elements = value
        .map(v => formatNamespaceValue(v, level))
        .join(`,${sp}`)
      return `[${elements}]`
    }
    if (typeof value === 'object') {
      const entries = Object.entries(value)
        .map(([k, v]) => {
          const formattedValue = formatNamespaceValue(v, level + 1)
          const line = `"${k}":${sp}${formattedValue}`
          return indent === 0 ? line : indentStr.repeat(level + 1) + line
        })
        .join(`,${nl}`)
      if (entries.length === 0) return '{}'
      const closing = indent === 0 ? '}' : indentStr.repeat(level) + '}'
      return `{${nl}${entries}${nl}${closing}`
    }
    return String(value)
  }

  return toSourceImpl(node, 0)
}
