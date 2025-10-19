declare enum TokenType {
    Number = "Number",
    String = "String",
    Identifier = "Identifier",
    Keyword = "Keyword",
    Operator = "Operator",
    Punctuation = "Punctuation",
    Comment = "Comment",
    Eol = "Eol"
}
interface Token {
    type: TokenType;
    value: string;
    line: number;
    column: number;
}
declare class LexerError extends Error {
    line: number;
    column: number;
    constructor(message: string, line: number, column: number);
}
declare class Lexer {
    private input;
    private pos;
    private line;
    private column;
    constructor(input: string);
    reset(): void;
    isAtEnd(): boolean;
    private peek;
    private advance;
    private skipWhitespace;
    private readNumber;
    private readIdentifier;
    private readString;
    private readSingleLineComment;
    private readMultiLineComment;
    private readOperatorOrPunctuation;
    next(): Token;
}

interface ASTNode {
    type: string;
    line: number;
    column: number;
}
declare class ParserError extends Error {
    token: Token;
    constructor(message: string, token: Token);
}
interface Expression extends ASTNode {
}
interface LiteralExpression extends Expression {
    type: 'Literal';
    value: string | number | boolean;
    raw: string;
}
interface IdentifierExpression extends Expression {
    type: 'Identifier';
    name: string;
}
interface BinaryExpression extends Expression {
    type: 'BinaryExpression';
    left: Expression;
    operator: string;
    right: Expression;
}
interface UnaryExpression extends Expression {
    type: 'UnaryExpression';
    operator: string;
    operand: Expression;
}
interface CallExpression extends Expression {
    type: 'CallExpression';
    callee: Expression;
    arguments: Expression[];
    then?: BlockStatement;
}
interface MemberExpression extends Expression {
    type: 'MemberExpression';
    object: Expression;
    property: Expression;
    computed: boolean;
}
interface ArrayExpression extends Expression {
    type: 'ArrayExpression';
    elements: Expression[];
}
interface Statement extends ASTNode {
}
interface NoopStatement extends Statement {
    type: 'NoopStatement';
}
interface ExpressionStatement extends Statement {
    type: 'ExpressionStatement';
    expression: Expression;
}
interface VariableDeclaration extends Statement {
    type: 'VariableDeclaration';
    name: string;
    isGlobal: boolean;
    initializer: Expression;
}
interface AssignmentStatement extends Statement {
    type: 'AssignmentStatement';
    left: Expression;
    operator: string;
    right: Expression;
}
interface IncrementStatement extends Statement {
    type: 'IncrementStatement';
    operator: string;
    target: Expression;
}
interface IfStatement extends Statement {
    type: 'IfStatement';
    condition: Expression;
    then: Statement;
    else?: Statement;
}
interface WhileStatement extends Statement {
    type: 'WhileStatement';
    condition: Expression;
    body: Statement;
}
interface ForStatement extends Statement {
    type: 'ForStatement';
    init?: AssignmentStatement | IncrementStatement;
    condition?: Expression;
    increment?: AssignmentStatement | IncrementStatement;
    body: Statement;
}
interface LoopStatement extends Statement {
    type: 'LoopStatement';
    body: Statement;
}
interface ReturnStatement extends Statement {
    type: 'ReturnStatement';
    value?: Expression;
}
interface BlockStatement extends Statement {
    type: 'BlockStatement';
    body: Statement[];
}
interface DecoratorStatement extends Statement {
    type: 'DecoratorStatement';
    name: IdentifierExpression;
    arguments: LiteralExpression[];
    target: Statement;
}
interface Parameter {
    name: IdentifierExpression;
    type: IdentifierExpression;
}
interface FunctionDeclaration extends Statement {
    type: 'FunctionDeclaration';
    name: IdentifierExpression;
    parameters: Parameter[];
    returnType: IdentifierExpression;
    once: boolean;
    body: BlockStatement;
}
interface ExternDeclaration extends Statement {
    type: 'ExternDeclaration';
    name: IdentifierExpression;
    value: any;
}
interface ModuleDeclaration extends Statement {
    type: 'ModuleDeclaration';
    name: IdentifierExpression;
    body?: Statement[];
    alias?: Expression;
}
interface ImportStatement extends Statement {
    type: 'ImportStatement';
    path: LiteralExpression;
}
interface Program extends ASTNode {
    type: 'Program';
    body: Statement[];
}
declare class Parser {
    private lexer;
    private previousToken;
    private currentToken;
    private nextToken;
    constructor(lexer: Lexer);
    reset(lexer?: Lexer): void;
    private loadNextToken;
    private isEof;
    private peek;
    private previous;
    private advance;
    private skipComments;
    private _matchBase;
    private match;
    private matchOperator;
    private matchIdentifier;
    private matchKeyword;
    private matchPunctuation;
    private _checkBase;
    private check;
    private synchronize;
    private consume;
    private _consumeBase;
    private consumeKeyword;
    private consumeEol;
    private consumeOperator;
    private consumeIdentifier;
    private consumePunctuation;
    private parseExpression;
    private parseLogicalOr;
    private parseLogicalAnd;
    private parseEquality;
    private parseComparison;
    private parseTerm;
    private parseFactor;
    private parseUnary;
    private parseCall;
    private finishCall;
    private parsePrimary;
    private parseArrayLiteral;
    private parseStatement;
    private parseDecoratorStatement;
    private parseVariableDeclaration;
    private parseIfStatement;
    private parseWhileStatement;
    private parseForStatement;
    private parseLoopStatement;
    private parseReturnStatement;
    private parseBlockStatement;
    private parseExpressionStatement;
    private parseStatementOrBlock;
    private parseFunctionDeclaration;
    private parseExternDeclaration;
    private parseModuleDeclaration;
    private parseTypeExpression;
    private parseImportStatement;
    private parseJSONValue;
    private parseDeclaration;
    parse(): Program;
    parseExpressionOnly(): Expression;
}
declare function toSource(node: ASTNode, indent?: number, semi?: boolean): string;

export { type ASTNode, type ArrayExpression, type AssignmentStatement, type BinaryExpression, type BlockStatement, type CallExpression, type DecoratorStatement, type Expression, type ExpressionStatement, type ExternDeclaration, type ForStatement, type FunctionDeclaration, type IdentifierExpression, type IfStatement, type ImportStatement, type IncrementStatement, Lexer, LexerError, type LiteralExpression, type LoopStatement, type MemberExpression, type ModuleDeclaration, type NoopStatement, type Parameter, Parser, ParserError, type Program, type ReturnStatement, type Statement, type Token, TokenType, type UnaryExpression, type VariableDeclaration, type WhileStatement, toSource };
