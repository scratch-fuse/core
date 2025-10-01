import {
  Variable,
  Reporter,
  Block,
  Script,
  BooleanInput,
  AnyInput,
  SubstackInput
} from './base'
import {
  FunctionDeclaration,
  Program,
  Statement,
  Expression,
  CallExpression,
  MemberExpression,
  IdentifierExpression,
  LiteralExpression,
  BinaryExpression,
  UnaryExpression,
  ArrayExpression,
  ExpressionStatement,
  AssignmentStatement,
  IfStatement,
  WhileStatement,
  UntilStatement,
  LoopStatement,
  ReturnStatement,
  VariableDeclaration,
  BlockStatement,
  DecoratorStatement,
  NamespaceDeclaration,
  IncrementStatement,
  ForStatement
} from '../parser'

import { ErrorList } from '../util/error'

export interface ProcedureArgument {
  name: string
  type: 'any' | 'bool'
}

export type ListCommand = (v: Variable, args: (Reporter | string)[]) => Block[]
export type ListReporter = (
  v: Variable,
  args: (Reporter | string)[]
) => TypedValue
export type VarCommand = (v: Variable, args: (Reporter | string)[]) => Block[]
// Var methods have no side effects, so VarCommand is not needed
export type VarReporter = (
  v: Reporter,
  args: (Reporter | string)[]
) => TypedValue

export class CompilerError extends Error {
  constructor(
    message: string,
    public line: number,
    public column: number
  ) {
    super(`${message} at ${line}:${column}`)
    this.name = 'CompilerError'
  }
}

export class Scope {
  private static listCmds = new Map<string, ListCommand>([
    [
      'push',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 1)
          throw new Error('push expects exactly one argument')
        return [
          {
            opcode: 'data_addtolist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              ITEM: { type: 'any', value: rhs[0] }
            }
          }
        ]
      }
    ],
    [
      'pop',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 0) throw new Error('pop expects no arguments')
        return [
          {
            opcode: 'data_deleteoflist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: 'any', value: 'last' }
            }
          }
        ]
      }
    ],
    [
      'insert',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 2)
          throw new Error('insert expects exactly two arguments')
        return [
          {
            opcode: 'data_insertatlist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: 'any', value: rhs[0] },
              ITEM: { type: 'any', value: rhs[1] }
            }
          }
        ]
      }
    ],
    [
      'remove',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 1)
          throw new Error('remove expects exactly one argument')
        return [
          {
            opcode: 'data_deleteoflist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: 'any', value: rhs[0] }
            }
          }
        ]
      }
    ],
    [
      'replace',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 2)
          throw new Error('replace expects exactly two arguments')
        return [
          {
            opcode: 'data_replaceitemoflist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: 'any', value: rhs[0] },
              ITEM: { type: 'any', value: rhs[1] }
            }
          }
        ]
      }
    ],
    [
      'show',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 0) throw new Error('show expects no arguments')
        return [
          {
            opcode: 'data_showlist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {}
          }
        ]
      }
    ],
    [
      'hide',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 0) throw new Error('hide expects no arguments')
        return [
          {
            opcode: 'data_hidelist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {}
          }
        ]
      }
    ],
    [
      'clear',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 0) throw new Error('clear expects no arguments')
        return [
          {
            opcode: 'data_deletealloflist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {}
          }
        ]
      }
    ]
  ])
  private static listReps = new Map<string, ListReporter>([
    [
      'includes',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 1)
          throw new Error('includes expects exactly one argument')
        return {
          type: 'bool',
          value: {
            opcode: 'data_listcontainsitem',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              ITEM: { type: 'any', value: rhs[0] }
            }
          }
        }
      }
    ],
    [
      'at',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 1) throw new Error('at expects exactly one argument')
        return {
          type: 'any',
          value: {
            opcode: 'data_itemoflist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              INDEX: { type: 'any', value: rhs[0] }
            }
          }
        }
      }
    ],
    [
      'indexOf',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 1)
          throw new Error('indexOf expects exactly one argument')
        return {
          type: 'any',
          value: {
            opcode: 'data_indexoflist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {
              ITEM: { type: 'any', value: rhs[0] }
            }
          }
        }
      }
    ],
    [
      'length',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 0) throw new Error('length expects no arguments')
        return {
          type: 'any',
          value: {
            opcode: 'data_lengthoflist',
            fields: {
              LIST: v.exportName ?? v.name
            },
            inputs: {}
          }
        }
      }
    ]
  ])
  private static varCmds = new Map<string, VarCommand>([
    [
      'show',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 0) throw new Error('show expects no arguments')
        return [
          {
            opcode: 'data_showvariable',
            fields: {
              VARIABLE: v.exportName ?? v.name
            },
            inputs: {}
          }
        ]
      }
    ],
    [
      'hide',
      (v: Variable, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 0) throw new Error('hide expects no arguments')
        return [
          {
            opcode: 'data_hidevariable',
            fields: {
              VARIABLE: v.exportName ?? v.name
            },
            inputs: {}
          }
        ]
      }
    ]
  ])
  private static varReps = new Map<string, VarReporter>([
    [
      'at',
      (v: Reporter, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 1) throw new Error('at expects exactly one argument')
        return {
          type: 'any',
          value: {
            opcode: 'operator_letter_of',
            fields: {},
            inputs: {
              STRING: { type: 'any', value: v },
              LETTER: { type: 'any', value: rhs[0] }
            }
          }
        }
      }
    ],
    [
      'includes',
      (v: Reporter, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 1)
          throw new Error('includes expects exactly one argument')
        return {
          type: 'bool',
          value: {
            opcode: 'operator_contains',
            fields: {},
            inputs: {
              STRING1: { type: 'any', value: v },
              STRING2: { type: 'any', value: rhs[0] }
            }
          }
        }
      }
    ],
    [
      'length',
      (v: Reporter, rhs: (Reporter | string)[]) => {
        if (rhs.length !== 0) throw new Error('length expects no arguments')
        return {
          type: 'any',
          value: {
            opcode: 'operator_length',
            fields: {},
            inputs: {
              STRING: { type: 'any', value: v }
            }
          }
        }
      }
    ]
  ])
  constructor(
    public readonly variables: Map<string, Variable>,
    private args?: Map<string, ProcedureArgument>
  ) {}
  typeof(name: string): 'list' | 'scalar' | 'arg_any' | 'arg_bool' | null {
    const arg = this.args?.get(name)
    if (arg) return arg.type === 'bool' ? 'arg_bool' : 'arg_any'
    const variable = this.variables.get(name)
    if (variable) return variable.type
    return null
  }

  get(name: string): Reporter | null {
    const arg = this.args?.get(name)
    if (arg) {
      switch (arg.type) {
        case 'any':
          return {
            opcode: 'argument_reporter_string_number',
            fields: {
              VALUE: arg.name
            },
            inputs: {}
          }
        case 'bool':
          return {
            opcode: 'argument_reporter_boolean',
            fields: {
              VALUE: arg.name
            },
            inputs: {}
          }
      }
    }
    const variable = this.variables.get(name)
    if (variable) {
      switch (variable.type) {
        case 'scalar':
          return {
            opcode: 'data_variable',
            fields: {
              VARIABLE: variable.exportName ?? variable.name
            },
            inputs: {}
          }
        case 'list':
          return {
            opcode: 'data_listcontents',
            fields: {
              LIST: variable.exportName ?? variable.name
            },
            inputs: {}
          }
      }
    }
    return null
  }
  set(name: string, value: Reporter | string): Block[] {
    const arg = this.args?.get(name)
    if (arg) throw new Error(`Cannot assign to argument ${name}`)
    const variable = this.variables.get(name)
    if (variable) {
      if (variable.type === 'scalar') {
        return [
          {
            opcode: 'data_setvariableto',
            fields: {
              VARIABLE: variable.exportName ?? variable.name
            },
            inputs: {
              VALUE: { type: 'any', value: value }
            }
          }
        ]
      } else throw new Error(`Cannot assign to list variable ${name}`)
    }
    throw new Error(`Variable ${name} not found`)
  }
  add(name: string, value: Reporter | string): Block[] {
    const arg = this.args?.get(name)
    if (arg) throw new Error(`Cannot add to argument ${name}`)
    const variable = this.variables.get(name)
    if (variable) {
      if (variable.type === 'scalar') {
        return [
          {
            opcode: 'data_changevariableby',
            fields: {
              VARIABLE: variable.exportName ?? variable.name
            },
            inputs: {
              VALUE: { type: 'any', value: value }
            }
          }
        ]
      } else throw new Error(`Cannot add to list variable ${name}`)
    }
    throw new Error(`Variable ${name} not found`)
  }
  stmtMethod(
    name: string,
    func: string,
    args: (Reporter | string)[]
  ): Block[] | null {
    const arg = this.args?.get(name)
    if (arg) return null
    const variable = this.variables.get(name)
    if (variable) {
      if (variable.type === 'list') {
        const listCmd = Scope.listCmds.get(func)
        if (listCmd) return listCmd(variable, args)
        else return null
      } else {
        const varCmd = Scope.varCmds.get(func)
        if (varCmd) return varCmd(variable, args)
        else return null
      }
    }
    throw new Error(`Variable ${name} not found`)
  }
  exprMethod(
    name: string,
    func: string,
    args: (Reporter | string)[]
  ): TypedValue | null {
    const arg = this.args?.get(name)
    if (arg) {
      const varRep = Scope.varReps.get(func)
      if (varRep)
        return varRep(
          arg.type === 'bool'
            ? {
                opcode: 'argument_reporter_boolean',
                fields: { VALUE: arg.name },
                inputs: {}
              }
            : {
                opcode: 'argument_reporter_string_number',
                fields: { VALUE: arg.name },
                inputs: {}
              },
          args
        )
    }
    const variable = this.variables.get(name)
    if (variable) {
      if (variable.type === 'list') {
        const listRep = Scope.listReps.get(func)
        if (listRep) return listRep(variable, args)
        else return null
      } else {
        const varRep = Scope.varReps.get(func)
        if (varRep)
          return varRep(
            {
              opcode: 'data_variable',
              fields: {
                VARIABLE: variable.exportName ?? variable.name
              },
              inputs: {}
            },
            args
          )
        else return null
      }
    }
    throw new Error(`Variable ${name} not found`)
  }
}

export interface CompiledFunction {
  decl: FunctionDeclaration
  proccode: string
  impl: Block[]
}

export class ScratchFunction {
  public scope: Scope
  constructor(
    globalVars: Map<string, Variable>,
    public decl: FunctionDeclaration,
    private exportName: string | null
  ) {
    const args = new Map<string, ProcedureArgument>()
    for (const arg of decl.parameters) {
      if (['bool', 'any'].indexOf(arg.type.name) === -1) {
        throw new CompilerError(
          `Invalid argument type ${arg.type}, at function ${decl.name.name}`,
          decl.name.line,
          decl.name.column
        )
      }
      args.set(arg.name.name, {
        type: arg.type.name === 'bool' ? 'bool' : 'any',
        name: arg.name.name
      })
    }
    this.scope = new Scope(globalVars, args)
    if (this.exportName) {
      // Validate exportName
      ScratchFunction.getProccode(decl, this.exportName)
    }
  }
  static escape(str: string) {
    // replace % with %%
    return str.replace(/%/g, '%%')
  }
  // TODO: 移动到其它位置，解耦合
  static getProccode(decl: FunctionDeclaration, exportName: string | null) {
    return exportName
      ? ScratchFunction.parseTemplateName(exportName, decl)
      : `${decl.name.name}(${decl.parameters
          .map(
            p =>
              `${ScratchFunction.escape(p.name.name)} = %${
                p.type.name === 'bool' ? 'b' : 's'
              }`
          )
          .join(', ')})`
  }
  get proccode() {
    return ScratchFunction.getProccode(this.decl, this.exportName)
  }
  private static parseTemplateName(
    template: string,
    decl: FunctionDeclaration
  ): string {
    const tokens = ScratchFunction.lexTemplate(
      template,
      decl.name.line,
      decl.name.column
    )
    let result = ''
    let paramIndex = 0

    for (const token of tokens) {
      if (token.type === 'text') {
        result += ScratchFunction.escape(token.value)
      } else if (token.type === 'param') {
        if (paramIndex >= decl.parameters.length) {
          throw new CompilerError(
            `Too many parameter placeholders in template`,
            decl.name.line,
            decl.name.column
          )
        }
        const param = decl.parameters[paramIndex]
        if (token.value !== param.name.name) {
          throw new CompilerError(
            `Parameter placeholder [${token.value}] does not match parameter ${param.name.name}`,
            decl.name.line,
            decl.name.column
          )
        }
        result += ` %${param.type.name === 'boolean' ? 'b' : 's'} `
        paramIndex++
      } else if (token.type === 'escaped') {
        result += token.value
      }
    }

    if (paramIndex < decl.parameters.length) {
      throw new CompilerError(
        `Missing parameter placeholders for: ${decl.parameters
          .slice(paramIndex)
          .map(p => p.name.name)
          .join(', ')}`,
        decl.name.line,
        decl.name.column
      )
    }

    return result.trim()
  }

  private static lexTemplate(
    template: string,
    line: number,
    column: number
  ): Array<{ type: 'text' | 'param' | 'escaped'; value: string }> {
    const tokens: Array<{ type: 'text' | 'param' | 'escaped'; value: string }> =
      []
    let i = 0
    let current = ''

    while (i < template.length) {
      if (
        i < template.length - 1 &&
        template[i] === '[' &&
        template[i + 1] === '['
      ) {
        // Escaped opening bracket [[
        if (current) {
          tokens.push({ type: 'text', value: current })
          current = ''
        }
        tokens.push({ type: 'escaped', value: '[' })
        i += 2
      } else if (
        i < template.length - 1 &&
        template[i] === ']' &&
        template[i + 1] === ']'
      ) {
        // Escaped closing bracket ]]
        if (current) {
          tokens.push({ type: 'text', value: current })
          current = ''
        }
        tokens.push({ type: 'escaped', value: ']' })
        i += 2
      } else if (template[i] === '[') {
        // Start of parameter placeholder
        if (current) {
          tokens.push({ type: 'text', value: current })
          current = ''
        }
        i++ // skip opening [
        let paramName = ''
        while (i < template.length && template[i] !== ']') {
          paramName += template[i]
          i++
        }
        if (i >= template.length) {
          throw new CompilerError(
            'Unclosed parameter placeholder',
            line,
            column
          )
        }
        tokens.push({ type: 'param', value: paramName })
        i++ // skip closing ]
      } else {
        current += template[i]
        i++
      }
    }

    if (current) {
      tokens.push({ type: 'text', value: current })
    }

    return tokens
  }
}

export type TypedValue =
  | {
      type: 'any'
      value: Reporter | string
    }
  | { type: 'bool'; value: Reporter }

export type Namespace = Map<string /** name */, NamespaceEntry>
export interface NamespaceEntry {
  opcode: string
  type: 'command' | 'reporter' | 'boolean' | 'conditional' | 'hat'
  inputs?: Record<string, BooleanInput | AnyInput | SubstackInput> // preset inputs
  fields?: Record<string, string> // preset fields
  args: (
    | NamespaceEntryArgumentAny
    | NamespaceEntryArgumentBoolean
    | NamespaceEntryArgumentSubstack
    | NamespaceEntryArgumentField
  )[]
}
export interface NamespaceEntryArgumentBase {
  name: string
}

export interface NamespaceEntryArgumentAny extends NamespaceEntryArgumentBase {
  type: 'any'
}

export interface NamespaceEntryArgumentBoolean
  extends NamespaceEntryArgumentBase {
  type: 'boolean'
}
export interface NamespaceEntryArgumentSubstack
  extends NamespaceEntryArgumentBase {
  type: 'substack'
}
export interface NamespaceEntryArgumentField
  extends NamespaceEntryArgumentBase {
  type: 'field'
  menu: Record<string, string> | null // null means as-is
}

export class Compiler {
  constructor(
    public globalScope: Scope,
    public funcs: Map<string, ScratchFunction>,
    public namespaces: Map<string, Namespace>
  ) {}
  parse(stmt: ScratchFunction): CompiledFunction
  parse(stmt: Program): Script[]
  parse(stmt: Statement, functionReturnType?: 'bool' | 'any' | 'void'): Block[]
  parse(
    stmt: Statement | Program | ScratchFunction,
    functionReturnType?: 'bool' | 'any' | 'void'
  ): Block[] | Script[] | CompiledFunction {
    if (stmt instanceof ScratchFunction) {
      return this.parseScratchFunction(stmt)
    } else if (stmt.type === 'Program') {
      return this.parseProgram(stmt as Program)
    } else {
      return this.parseStatement(stmt as Statement, functionReturnType ?? null)
    }
  }
  parseExpr(stmt: Expression): TypedValue {
    switch (stmt.type) {
      case 'Literal':
        return this.parseLiteralExpression(stmt as LiteralExpression)
      case 'Identifier':
        return this.parseIdentifierExpression(stmt as IdentifierExpression)
      case 'BinaryExpression':
        return this.parseBinaryExpression(stmt as BinaryExpression)
      case 'UnaryExpression':
        return this.parseUnaryExpression(stmt as UnaryExpression)
      case 'CallExpression':
        return this.parseCallExpressionAsReporter(stmt as CallExpression)
      case 'MemberExpression':
        return this.parseMemberExpression(stmt as MemberExpression)
      case 'ArrayExpression':
        throw new CompilerError(
          'ArrayExpression is only allowed in variable declarations',
          stmt.line,
          stmt.column
        )
      default:
        throw new CompilerError(
          `Unsupported expression type: ${stmt.type}`,
          stmt.line,
          stmt.column
        )
    }
  }

  // Helper methods for parseExpr
  private parseLiteralExpression(expr: LiteralExpression): TypedValue {
    const value = expr.value
    if (typeof value === 'boolean') {
      return this.getBooleanLiteral(value)
    } else {
      return {
        type: 'any',
        value: String(value)
      }
    }
  }

  private parseIdentifierExpression(expr: IdentifierExpression): TypedValue {
    const name = expr.name
    let reporter: Reporter | null
    try {
      reporter = this.globalScope.get(name)
    } catch (error) {
      throw new CompilerError((error as Error).message, expr.line, expr.column)
    }
    if (reporter) {
      const varType = this.globalScope.typeof(name)
      // Convert variable types to TypedValue types
      if (varType === 'arg_bool') {
        return { type: 'bool', value: reporter }
      } else {
        return { type: 'any', value: reporter }
      }
    }
    throw new CompilerError(
      `Variable ${name} not found`,
      expr.line,
      expr.column
    )
  }

  private parseBinaryExpression(expr: BinaryExpression): TypedValue {
    const left = this.parseExpr(expr.left)
    const right = this.parseExpr(expr.right)

    switch (expr.operator) {
      case '+':
        return {
          type: 'any',
          value: {
            opcode: 'operator_add',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '-':
        return {
          type: 'any',
          value: {
            opcode: 'operator_subtract',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '*':
        return {
          type: 'any',
          value: {
            opcode: 'operator_multiply',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '/':
        return {
          type: 'any',
          value: {
            opcode: 'operator_divide',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '%':
        return {
          type: 'any',
          value: {
            opcode: 'operator_mod',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: left.value },
              NUM2: { type: 'any', value: right.value }
            }
          }
        }
      case '..':
        return {
          type: 'any',
          value: {
            opcode: 'operator_join',
            fields: {},
            inputs: {
              STRING1: { type: 'any', value: left.value },
              STRING2: { type: 'any', value: right.value }
            }
          }
        }
      case '==':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_equals',
            fields: {},
            inputs: {
              OPERAND1: { type: 'any', value: left.value },
              OPERAND2: { type: 'any', value: right.value }
            }
          }
        }
      case '!=':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                type: 'bool',
                value: {
                  opcode: 'operator_equals',
                  fields: {},
                  inputs: {
                    OPERAND1: { type: 'any', value: left.value },
                    OPERAND2: { type: 'any', value: right.value }
                  }
                }
              }
            }
          }
        }
      case '<':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_lt',
            fields: {},
            inputs: {
              OPERAND1: { type: 'any', value: left.value },
              OPERAND2: { type: 'any', value: right.value }
            }
          }
        }
      case '>':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_gt',
            fields: {},
            inputs: {
              OPERAND1: { type: 'any', value: left.value },
              OPERAND2: { type: 'any', value: right.value }
            }
          }
        }
      case '<=':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                type: 'bool',
                value: {
                  opcode: 'operator_gt',
                  fields: {},
                  inputs: {
                    OPERAND1: { type: 'any', value: left.value },
                    OPERAND2: { type: 'any', value: right.value }
                  }
                }
              }
            }
          }
        }
      case '>=':
        return {
          type: 'bool',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                type: 'bool',
                value: {
                  opcode: 'operator_lt',
                  fields: {},
                  inputs: {
                    OPERAND1: { type: 'any', value: left.value },
                    OPERAND2: { type: 'any', value: right.value }
                  }
                }
              }
            }
          }
        }
      case '&&':
        this.ensureBooleanType(
          left,
          'Left operand of && must be boolean',
          expr.line,
          expr.column
        )
        this.ensureBooleanType(
          right,
          'Right operand of && must be boolean',
          expr.line,
          expr.column
        )
        return {
          type: 'bool',
          value: {
            opcode: 'operator_and',
            fields: {},
            inputs: {
              OPERAND1: { type: 'bool', value: left.value as Reporter },
              OPERAND2: { type: 'bool', value: right.value as Reporter }
            }
          }
        }
      case '||':
        this.ensureBooleanType(
          left,
          'Left operand of || must be boolean',
          expr.line,
          expr.column
        )
        this.ensureBooleanType(
          right,
          'Right operand of || must be boolean',
          expr.line,
          expr.column
        )
        return {
          type: 'bool',
          value: {
            opcode: 'operator_or',
            fields: {},
            inputs: {
              OPERAND1: { type: 'bool', value: left.value as Reporter },
              OPERAND2: { type: 'bool', value: right.value as Reporter }
            }
          }
        }
      default:
        throw new CompilerError(
          `Unsupported binary operator: ${expr.operator}`,
          expr.line,
          expr.column
        )
    }
  }

  private parseUnaryExpression(expr: UnaryExpression): TypedValue {
    const operand = this.parseExpr(expr.operand)

    switch (expr.operator) {
      case '!':
        this.ensureBooleanType(
          operand,
          'Operand of ! must be boolean',
          expr.line,
          expr.column
        )
        return {
          type: 'bool',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: { type: 'bool', value: operand.value as Reporter }
            }
          }
        }
      case '-':
        return {
          type: 'any',
          value: {
            opcode: 'operator_subtract',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: '0' },
              NUM2: { type: 'any', value: operand.value }
            }
          }
        }
      case '+':
        return {
          type: 'any',
          value: {
            opcode: 'operator_add',
            fields: {},
            inputs: {
              NUM1: { type: 'any', value: '0' },
              NUM2: { type: 'any', value: operand.value }
            }
          }
        }
      default:
        throw new CompilerError(
          `Unsupported unary operator: ${expr.operator}`,
          expr.line,
          expr.column
        )
    }
  }

  private parseCallExpressionAsReporter(expr: CallExpression): TypedValue {
    if (expr.then) {
      throw new CompilerError(
        'Call expressions with then blocks cannot be used as reporters',
        expr.line,
        expr.column
      )
    }

    if (expr.callee.type === 'MemberExpression') {
      const memberExpr = expr.callee as MemberExpression
      return this.parseNamespaceCallAsReporter(
        memberExpr,
        expr.arguments,
        expr.line,
        expr.column
      )
    } else if (expr.callee.type === 'Identifier') {
      const funcName = (expr.callee as IdentifierExpression).name
      return this.parseFunctionCallAsReporter(
        funcName,
        expr.arguments,
        expr.line,
        expr.column
      )
    } else {
      throw new CompilerError(
        'Unsupported call expression callee type',
        expr.line,
        expr.column
      )
    }
  }

  private parseMemberExpression(expr: MemberExpression): TypedValue {
    if (expr.object.type !== 'Identifier') {
      throw new CompilerError(
        'Only simple member expressions are supported',
        expr.line,
        expr.column
      )
    }

    const objectName = (expr.object as IdentifierExpression).name

    if (expr.computed) {
      // Handle computed access like test[1]
      const index = this.parseExpr(expr.property)
      return this.parseComputedAccess(objectName, index, expr.line, expr.column)
    } else {
      // Handle dot notation like test.method
      const propertyName = (expr.property as IdentifierExpression).name

      // Check if this is a variable method call
      const varType = this.globalScope.typeof(objectName)
      if (varType) {
        try {
          const methodResult = this.globalScope.exprMethod(
            objectName,
            propertyName,
            []
          )
          if (methodResult) {
            return methodResult
          }
        } catch (error) {
          throw new CompilerError(
            (error as Error).message,
            expr.line,
            expr.column
          )
        }
      }

      throw new CompilerError(
        `Member ${propertyName} not found on ${objectName} or member functions cannot be used as values`,
        expr.line,
        expr.column
      )
    }
  }

  private parseNamespaceCallAsReporter(
    memberExpr: MemberExpression,
    args: Expression[],
    line: number,
    column: number
  ): TypedValue {
    if (
      memberExpr.object.type !== 'Identifier' ||
      memberExpr.property.type !== 'Identifier'
    ) {
      throw new CompilerError(
        'Namespace calls must use simple identifiers',
        line,
        column
      )
    }

    const namespaceName = (memberExpr.object as IdentifierExpression).name
    const functionName = (memberExpr.property as IdentifierExpression).name

    const namespace = this.namespaces.get(namespaceName)
    if (!namespace) {
      throw new CompilerError(
        `Namespace ${namespaceName} not found`,
        line,
        column
      )
    }

    const entry = namespace.get(functionName)
    if (!entry) {
      throw new CompilerError(
        `Function ${functionName} not found in namespace ${namespaceName}`,
        line,
        column
      )
    }

    if (entry.type !== 'reporter' && entry.type !== 'boolean') {
      throw new CompilerError(
        `${namespaceName}.${functionName} cannot be used as a reporter (type: ${entry.type})`,
        line,
        column
      )
    }

    const parsedArgs = args.map(arg => this.parseExpr(arg))
    const inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput } =
      Object.assign({}, entry.inputs)
    const fields: { [key: string]: string } = Object.assign({}, entry.fields)

    // Match arguments with entry arguments
    if (parsedArgs.length !== entry.args.length) {
      throw new CompilerError(
        `${namespaceName}.${functionName} expects ${entry.args.length} arguments, got ${parsedArgs.length}`,
        line,
        column
      )
    }

    for (let i = 0; i < entry.args.length; i++) {
      const argDef = entry.args[i]
      const argValue = parsedArgs[i]

      if (argDef.type === 'boolean' && argValue.type !== 'bool') {
        throw new CompilerError(
          `Argument ${argDef.name} must be boolean`,
          line,
          column
        )
      }

      if (argDef.type === 'substack') {
        throw new CompilerError(
          `Argument ${argDef.name} cannot be a substack`,
          line,
          column
        )
      }

      if (argDef.type === 'field') {
        // Must be literal
        const rawArgValue = args[i]
        if (rawArgValue.type !== 'Literal') {
          throw new CompilerError(
            `Argument ${argDef.name} must be a literal for field`,
            line,
            column
          )
        }
        const literal = rawArgValue as LiteralExpression
        if (typeof literal.value !== 'string') {
          throw new CompilerError(
            `Argument ${argDef.name} must be a string literal`,
            line,
            column
          )
        }
        const fieldValue = String(literal.value)
        if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
          throw new CompilerError(
            `Argument ${argDef.name} has invalid value ${fieldValue}`,
            line,
            column
          )
        }
        fields[argDef.name] = argDef.menu ? argDef.menu[fieldValue] : fieldValue
      } else {
        inputs[argDef.name] =
          argValue.type === 'bool'
            ? { type: 'bool', value: argValue.value as Reporter }
            : { type: 'any', value: argValue.value }
      }
    }

    return {
      type: entry.type === 'boolean' ? 'bool' : 'any',
      value: {
        opcode: entry.opcode,
        fields,
        inputs
      }
    }
  }

  private parseFunctionCallAsReporter(
    funcName: string,
    args: Expression[],
    line: number,
    column: number
  ): TypedValue {
    const func = this.funcs.get(funcName)

    if (!func) {
      throw new CompilerError(`Function ${funcName} not found`, line, column)
    }

    if (func.decl.returnType.name === 'void') {
      throw new CompilerError(
        `Function ${funcName} returns void and cannot be used as a reporter`,
        line,
        column
      )
    }

    const parsedArgs = args.map(arg => this.parseExpr(arg))

    // Check argument count
    if (parsedArgs.length !== func.decl.parameters.length) {
      throw new CompilerError(
        `Function ${funcName} expects ${func.decl.parameters.length} arguments, got ${parsedArgs.length}`,
        line,
        column
      )
    }

    // Check argument types
    for (let i = 0; i < func.decl.parameters.length; i++) {
      const paramType =
        func.decl.parameters[i].type.name === 'bool' ? 'bool' : 'any'
      if (paramType === 'bool' && parsedArgs[i].type !== 'bool') {
        throw new CompilerError(
          `Parameter ${func.decl.parameters[i].name} must be boolean`,
          line,
          column
        )
      }
    }

    const inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput } =
      {}
    for (let i = 0; i < parsedArgs.length; i++) {
      const paramName = func.decl.parameters[i].name.name
      const paramType =
        func.decl.parameters[i].type.name === 'bool' ? 'bool' : 'any'
      inputs[paramName] =
        paramType === 'bool'
          ? { type: 'bool', value: parsedArgs[i].value as Reporter }
          : { type: 'any', value: parsedArgs[i].value }
    }

    const names = func.decl.parameters.map(p => p.name.name)

    return {
      type: func.decl.returnType.name === 'bool' ? 'bool' : 'any',
      value: {
        opcode: 'procedures_call',
        fields: {},
        inputs,
        mutation: {
          tagName: 'mutation',
          proccode: func.proccode,
          children: [],
          return: '1',
          warp: func.decl.once ? 'true' : 'false',
          argumentids: JSON.stringify(names), // FIXME: use stable IDs
          argumentnames: JSON.stringify(names),
          argumentdefaults: JSON.stringify(
            func.decl.parameters.map(p =>
              p.type.name === 'any' ? '' : 'false'
            )
          )
        }
      }
    }
  }

  // Type checking utility
  private ensureBooleanType(
    value: TypedValue,
    message: string,
    line: number,
    column: number
  ): void {
    if (value.type !== 'bool') {
      throw new CompilerError(message, line, column)
    }
  }

  // Handle computed access (test[index])
  private parseComputedAccess(
    objectName: string,
    index: TypedValue,
    line: number,
    column: number
  ): TypedValue {
    const varType = this.globalScope.typeof(objectName)
    if (!varType) {
      throw new CompilerError(`Variable ${objectName} not found`, line, column)
    }

    if (varType === 'list') {
      // For lists, use list.at(index)
      try {
        const methodResult = this.globalScope.exprMethod(objectName, 'at', [
          index.value
        ])
        if (methodResult) {
          return methodResult
        }
        throw new CompilerError(
          `Method 'at' not found on list ${objectName}`,
          line,
          column
        )
      } catch (error) {
        if (error instanceof CompilerError) {
          throw error
        }
        throw new CompilerError((error as Error).message, line, column)
      }
    } else if (varType === 'scalar' || varType === 'arg_any') {
      // For any/scalar, use string.at(index)
      try {
        const methodResult = this.globalScope.exprMethod(objectName, 'at', [
          index.value
        ])
        if (methodResult) {
          return methodResult
        }
        throw new CompilerError(
          `Method 'at' not found on variable ${objectName}`,
          line,
          column
        )
      } catch (error) {
        if (error instanceof CompilerError) {
          throw error
        }
        throw new CompilerError((error as Error).message, line, column)
      }
    } else {
      throw new CompilerError(
        `Cannot use computed access on ${varType}`,
        line,
        column
      )
    }
  }

  // Parse ScratchFunction
  private parseScratchFunction(func: ScratchFunction): CompiledFunction {
    // 使用函数的作用域来编译函数体
    const oldScope = this.globalScope
    this.globalScope = func.scope

    try {
      // 确定函数返回类型
      const returnType: 'bool' | 'any' | 'void' =
        func.decl.returnType.name === 'bool'
          ? 'bool'
          : func.decl.returnType.name === 'void'
            ? 'void'
            : 'any'

      // 编译函数体
      const impl = this.parseBlockStatement(func.decl.body, returnType)

      if (func.decl.returnType.name !== 'void') {
        // 确保所有代码路径都返回值
        const allPathsReturn = (blocks: Block[]): boolean => {
          for (const block of blocks) {
            if (block.opcode === 'control_if_else') {
              const substack1: Block[] =
                (block.inputs.SUBSTACK as SubstackInput)?.value || []
              const substack2: Block[] =
                (block.inputs.SUBSTACK2 as SubstackInput)?.value || []
              if (!allPathsReturn(substack1) || !allPathsReturn(substack2)) {
                return false
              }
            } else if (block.opcode === 'control_if') {
              const substack: Block[] =
                (block.inputs.SUBSTACK as SubstackInput)?.value || []
              if (!allPathsReturn(substack)) {
                return false
              }
            } else if (block.opcode === 'procedures_return') {
              return true
            }
          }
          return false
        }

        if (!allPathsReturn(impl)) {
          throw new CompilerError(
            `Not all code paths return a value in function ${func.decl.name.name}`,
            func.decl.name.line,
            func.decl.name.column
          )
        }
      }

      return {
        decl: func.decl,
        proccode: func.proccode,
        impl: impl
      }
    } finally {
      // 恢复原始作用域
      this.globalScope = oldScope
    }
  }

  // Parse Program (top-level)
  private parseProgram(program: Program): Script[] {
    const scripts: Script[] = []
    const errors: Error[] = []

    for (const stmt of program.body) {
      try {
        switch (stmt.type) {
          case 'ExpressionStatement':
            const exprStmt = stmt as ExpressionStatement
            if (exprStmt.expression.type === 'CallExpression') {
              const callExpr = exprStmt.expression as CallExpression
              if (callExpr.then) {
                // Check if this is a hat block call
                const hatScript = this.parseHatCall(callExpr)
                if (hatScript) {
                  scripts.push(hatScript)
                  continue
                }
              }
            }
            // Fall through to handle as regular expression if not a hat call
            throw new CompilerError(
              'Top-level expressions must be hat block calls',
              stmt.line,
              stmt.column
            )

          case 'NamespaceDeclaration':
          case 'FunctionDeclaration':
          case 'VariableDeclaration':
            // Skip these as they are handled during initialization
            break

          case 'DecoratorStatement': {
            // Perform simple check
            const decorator = stmt as DecoratorStatement
            if (decorator.name.name !== 'extern') {
              throw new CompilerError(
                `Unknown decorator @${decorator.name.name}`,
                decorator.line,
                decorator.column
              )
            }
            if (
              !['FunctionDeclaration', 'VariableDeclaration'].includes(
                decorator.target.type
              )
            ) {
              throw new CompilerError(
                '@export can only be applied to functions or variables',
                decorator.line,
                decorator.column
              )
            }
            break
          }

          default:
            throw new CompilerError(
              `Statement type ${stmt.type} is not allowed at top level`,
              stmt.line,
              stmt.column
            )
        }
      } catch (error) {
        errors.push(error as Error)
        // Continue processing other statements
      }
    }

    // If there are errors, throw them all at once using ErrorList
    if (errors.length > 0) {
      throw new ErrorList(errors)
    }

    return scripts
  }

  // Parse Statement (in function/block context)
  private parseStatement(
    stmt: Statement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    switch (stmt.type) {
      case 'NoopStatement':
        return []
      case 'ExpressionStatement':
        return this.parseExpressionStatement(
          stmt as ExpressionStatement,
          functionReturnType
        )
      case 'AssignmentStatement':
        return this.parseAssignmentStatement(stmt as AssignmentStatement)
      case 'IncrementStatement':
        return this.parseIncrementStatement(stmt as IncrementStatement)
      case 'IfStatement':
        return this.parseIfStatement(stmt as IfStatement, functionReturnType)
      case 'WhileStatement':
        return this.parseWhileStatement(
          stmt as WhileStatement,
          functionReturnType
        )
      case 'ForStatement':
        return this.parseForStatement(stmt as ForStatement, functionReturnType)
      case 'UntilStatement':
        return this.parseUntilStatement(
          stmt as UntilStatement,
          functionReturnType
        )
      case 'LoopStatement':
        return this.parseLoopStatement(
          stmt as LoopStatement,
          functionReturnType
        )
      case 'ReturnStatement':
        return this.parseReturnStatement(
          stmt as ReturnStatement,
          functionReturnType
        )
      case 'BlockStatement':
        return this.parseBlockStatement(
          stmt as BlockStatement,
          functionReturnType
        )
      case 'NamespaceDeclaration':
        throw new CompilerError(
          'Namespace declarations are not allowed in function bodies',
          stmt.line,
          stmt.column
        )
      case 'VariableDeclaration':
        throw new CompilerError(
          'Variable declarations are not allowed in function bodies (use assignment instead)',
          stmt.line,
          stmt.column
        )
      case 'FunctionDeclaration':
        throw new CompilerError(
          'Function declarations are not allowed in function bodies',
          stmt.line,
          stmt.column
        )
      case 'DecoratorStatement':
        throw new CompilerError(
          'Decorators are not allowed in function bodies',
          stmt.line,
          stmt.column
        )
      default:
        throw new CompilerError(
          `Unsupported statement type: ${stmt.type}`,
          stmt.line,
          stmt.column
        )
    }
  }

  // Parse hat block calls
  private parseHatCall(callExpr: CallExpression): Script | null {
    if (!callExpr.then) return null

    if (callExpr.callee.type === 'MemberExpression') {
      const memberExpr = callExpr.callee as MemberExpression
      if (
        memberExpr.object.type === 'Identifier' &&
        memberExpr.property.type === 'Identifier'
      ) {
        const namespaceName = (memberExpr.object as IdentifierExpression).name
        const functionName = (memberExpr.property as IdentifierExpression).name

        const namespace = this.namespaces.get(namespaceName)
        if (!namespace) return null

        const entry = namespace.get(functionName)
        if (!entry || entry.type !== 'hat') return null

        const parsedArgs = callExpr.arguments.map(arg => this.parseExpr(arg))
        const inputs: {
          [key: string]: BooleanInput | AnyInput | SubstackInput
        } = {}

        // Match arguments with entry arguments
        if (parsedArgs.length !== entry.args.length) {
          throw new CompilerError(
            `${namespaceName}.${functionName} expects ${entry.args.length} arguments, got ${parsedArgs.length}`,
            callExpr.line,
            callExpr.column
          )
        }

        for (let i = 0; i < entry.args.length; i++) {
          const argDef = entry.args[i]
          const argValue = parsedArgs[i]

          if (argDef.type === 'boolean' && argValue.type !== 'bool') {
            throw new CompilerError(
              `Argument ${argDef.name} must be boolean`,
              callExpr.line,
              callExpr.column
            )
          }

          inputs[argDef.name] =
            argValue.type === 'bool'
              ? { type: 'bool', value: argValue.value as Reporter }
              : { type: 'any', value: argValue.value }
        }

        const hat = {
          opcode: entry.opcode,
          fields: {},
          inputs
        }

        const blocks = this.parseBlockStatement(callExpr.then, null)

        return { hat, blocks }
      }
    } else if (callExpr.callee.type === 'Identifier') {
      // Function call with no arguments but with then block, treat as hat if it's a hat
      const funcName = (callExpr.callee as IdentifierExpression).name

      // Check if this is a namespace member without arguments (like event.greenflag)
      // This case should not happen as parser wraps it in CallExpression
      return null
    }

    return null
  }

  // Parse hat member calls (like event.greenflag {})
  // This method is no longer needed as parser wraps member expressions with then blocks into CallExpressions

  // Statement parsing methods
  private parseExpressionStatement(
    stmt: ExpressionStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const expr = stmt.expression

    if (expr.type === 'CallExpression') {
      const callExpr = expr as CallExpression

      // Parse as command call
      if (callExpr.callee.type === 'MemberExpression') {
        const memberExpr = callExpr.callee as MemberExpression
        if (
          memberExpr.object.type === 'Identifier' &&
          memberExpr.property.type === 'Identifier'
        ) {
          const objectName = (memberExpr.object as IdentifierExpression).name
          const propertyName = (memberExpr.property as IdentifierExpression)
            .name

          // First check if this is a namespace call
          const namespace = this.namespaces.get(objectName)
          if (namespace) {
            return this.parseNamespaceCallAsCommand(
              memberExpr,
              callExpr.arguments,
              functionReturnType,
              callExpr.then,
              callExpr.line,
              callExpr.column
            )
          }

          // Then check if this is a method call on a variable
          const varType = this.globalScope.typeof(objectName)
          if (varType) {
            const args = callExpr.arguments.map(
              arg => this.parseExpr(arg).value
            )
            try {
              const methodResult = this.globalScope.stmtMethod(
                objectName,
                propertyName,
                args
              )
              if (methodResult) {
                return methodResult
              }
              // If stmtMethod returns null, the method doesn't exist
              throw new CompilerError(
                `Method ${propertyName} not found on ${objectName} or cannot be used as a statement`,
                callExpr.line,
                callExpr.column
              )
            } catch (error) {
              if (error instanceof CompilerError) {
                throw error
              }
              throw new CompilerError(
                (error as Error).message,
                callExpr.line,
                callExpr.column
              )
            }
          }

          throw new CompilerError(
            `Object ${objectName} not found (neither namespace nor variable)`,
            callExpr.line,
            callExpr.column
          )
        }
      } else if (callExpr.callee.type === 'Identifier') {
        const funcName = (callExpr.callee as IdentifierExpression).name
        return this.parseFunctionCallAsCommand(
          funcName,
          callExpr.arguments,
          callExpr.line,
          callExpr.column
        )
      }
    }

    throw new CompilerError(
      'Expression statements must be function calls or method calls',
      stmt.line,
      stmt.column
    )
  }

  private operateVar(
    varName: string,
    operator: string,
    value: TypedValue,
    line: number,
    column: number
  ): Block[] {
    switch (operator) {
      case '=':
        return this.globalScope.set(varName, value.value)
      case '+=':
        return this.globalScope.add(varName, value.value)
      case '-=':
        return this.globalScope.add(varName, {
          opcode: 'operator_subtract',
          fields: {},
          inputs: {
            NUM1: { type: 'any', value: '0' },
            NUM2: { type: 'any', value: value.value }
          }
        })
      // *= -> = var * value, /= -> = var / value, etc.
      case '*=':
      case '/=':
      case '%=':
      case '..=': {
        const operatorMap: Record<string, string> = {
          '*': 'operator_multiply',
          '/': 'operator_divide',
          '%': 'operator_mod',
          '.': 'operator_join'
        }
        const op = operator[0] // Get the operator character
        let varValue: Reporter
        try {
          const val = this.globalScope.get(varName)
          if (!val) {
            throw new CompilerError(
              `Variable ${varName} not found for operation`,
              0,
              0
            )
          }
          varValue = val
        } catch (error) {
          throw new CompilerError((error as Error).message, line, column)
        }
        const operationBlock = {
          opcode: operatorMap[op],
          fields: {},
          inputs: {
            NUM1: { type: 'any', value: varValue },
            NUM2: { type: 'any', value: value.value }
          }
        } satisfies Reporter
        return this.globalScope.set(varName, operationBlock)
      }
      default:
        throw new CompilerError(
          `Unsupported operator: ${operator}`,
          line,
          column
        )
    }
  }

  private operateComputed(
    memberExpr: MemberExpression,
    operator: string,
    value: TypedValue,
    line: number,
    column: number
  ): Block[] {
    if (!memberExpr.computed) {
      throw new CompilerError(
        'Dot notation assignments are not supported',
        line,
        column
      )
    }

    if (memberExpr.object.type !== 'Identifier') {
      throw new CompilerError(
        'Only simple computed assignments are supported',
        line,
        column
      )
    }

    const objectName = (memberExpr.object as IdentifierExpression).name
    const varType = this.globalScope.typeof(objectName)

    if (varType !== 'list') {
      throw new CompilerError(
        'Computed assignment is only supported for lists',
        line,
        column
      )
    }

    switch (operator) {
      case '=': {
        const index = this.parseExpr(memberExpr.property)

        // Use list.replace(index, value)
        try {
          const methodResult = this.globalScope.stmtMethod(
            objectName,
            'replace',
            [index.value, value.value]
          )
          if (methodResult) {
            return methodResult
          }
          throw new CompilerError(
            `Method 'replace' not found on list ${objectName}`,
            line,
            column
          )
        } catch (error) {
          if (error instanceof CompilerError) {
            throw error
          }
          throw new CompilerError((error as Error).message, line, column)
        }
      }
      case '+=':
      case '-=':
      case '*=':
      case '/=':
      case '%=':
      case '..=': {
        // Use list.replace(list.get(index) op value)
        const operatorMap: Record<string, string> = {
          '*': 'operator_multiply',
          '/': 'operator_divide',
          '%': 'operator_mod',
          '.': 'operator_join'
        }
        const op = operator[0] // Get the operator character
        const index = this.parseExpr(memberExpr.property)
        let currentValue: TypedValue
        try {
          currentValue = this.parseComputedAccess(
            objectName,
            index,
            line,
            column
          )
        } catch (error) {
          throw new CompilerError((error as Error).message, line, column)
        }
        const additionBlock = {
          opcode: operatorMap[op],
          fields: {},
          inputs: {
            NUM1: { type: 'any', value: currentValue.value },
            NUM2: { type: 'any', value: value.value }
          }
        } satisfies Reporter
        try {
          const methodResult = this.globalScope.stmtMethod(
            objectName,
            'replace',
            [index.value, additionBlock]
          )
          if (methodResult) {
            return methodResult
          }
          throw new CompilerError(
            `Method 'replace' not found on list ${objectName}`,
            line,
            column
          )
        } catch (error) {
          if (error instanceof CompilerError) {
            throw error
          }
          throw new CompilerError((error as Error).message, line, column)
        }
      }
      default:
        throw new CompilerError(
          `Unsupported operator: ${operator}`,
          line,
          column
        )
    }
  }

  private parseIncrementStatement(stmt: IncrementStatement): Block[] {
    if (stmt.target.type === 'Identifier') {
      // Simple variable assignment
      const varName = (stmt.target as IdentifierExpression).name

      switch (stmt.operator) {
        case '++':
          return this.operateVar(
            varName,
            '+=',
            { type: 'any', value: '1' },
            stmt.line,
            stmt.column
          )
        case '--':
          return this.operateVar(
            varName,
            '-=',
            { type: 'any', value: '1' },
            stmt.line,
            stmt.column
          )
        default:
          throw new CompilerError(
            `Unsupported increment operator: ${stmt.operator}`,
            stmt.line,
            stmt.column
          )
      }
    } else if (stmt.target.type === 'MemberExpression') {
      // Computed member assignment like test[1]++
      const memberExpr = stmt.target as MemberExpression

      switch (stmt.operator) {
        case '++':
          return this.operateComputed(
            memberExpr,
            '+=',
            { type: 'any', value: '1' },
            stmt.line,
            stmt.column
          )
        case '--':
          return this.operateComputed(
            memberExpr,
            '-=',
            { type: 'any', value: '1' },
            stmt.line,
            stmt.column
          )
        default:
          throw new CompilerError(
            `Unsupported increment operator: ${stmt.operator}`,
            stmt.line,
            stmt.column
          )
      }
    } else {
      throw new CompilerError(
        'Unsupported increment target',
        stmt.line,
        stmt.column
      )
    }
  }
  private parseAssignmentStatement(stmt: AssignmentStatement): Block[] {
    const value = this.parseExpr(stmt.right)

    if (stmt.left.type === 'Identifier') {
      // Simple variable assignment
      const varName = (stmt.left as IdentifierExpression).name
      return this.operateVar(
        varName,
        stmt.operator,
        value,
        stmt.line,
        stmt.column
      )
    } else if (stmt.left.type === 'MemberExpression') {
      // Computed member assignment like test[1] = value
      const memberExpr = stmt.left as MemberExpression
      return this.operateComputed(
        memberExpr,
        stmt.operator,
        value,
        stmt.line,
        stmt.column
      )
    } else {
      throw new CompilerError(
        'Unsupported assignment target',
        stmt.line,
        stmt.column
      )
    }
  }

  private parseIfStatement(
    stmt: IfStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const condition = this.parseExpr(stmt.condition)
    this.ensureBooleanType(
      condition,
      'If condition must be boolean',
      stmt.line,
      stmt.column
    )

    const thenBlocks: Block[] =
      stmt.then.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.then as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.then, functionReturnType)
    const elseBlocks = stmt.else
      ? stmt.else.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.else as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.else as Statement, functionReturnType)
      : []

    return [
      {
        opcode: elseBlocks.length > 0 ? 'control_if_else' : 'control_if',
        fields: {},
        inputs: {
          CONDITION: { type: 'bool', value: condition.value as Reporter },
          SUBSTACK: { type: 'substack', value: thenBlocks },
          ...(elseBlocks.length > 0 && {
            SUBSTACK2: { type: 'substack', value: elseBlocks }
          })
        }
      }
    ]
  }

  private parseWhileStatement(
    stmt: WhileStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const condition = this.parseExpr(stmt.condition)
    this.ensureBooleanType(
      condition,
      'While condition must be boolean',
      stmt.line,
      stmt.column
    )

    const bodyBlocks =
      stmt.body.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.body as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.body, functionReturnType)

    return [
      {
        opcode: 'control_while',
        fields: {},
        inputs: {
          CONDITION: { type: 'bool', value: condition.value as Reporter },
          SUBSTACK: { type: 'substack', value: bodyBlocks }
        }
      }
    ]
  }

  private getBooleanLiteral(value: boolean): TypedValue {
    if (value)
      return {
        type: 'bool',
        value: {
          opcode: 'operator_not',
          fields: {},
          inputs: {}
        }
      }
    return {
      type: 'bool',
      value: {
        opcode: 'operator_not',
        fields: {},
        inputs: {
          OPERAND: {
            type: 'bool',
            value: {
              opcode: 'operator_not',
              fields: {},
              inputs: {}
            }
          }
        }
      }
    }
  }

  private parseForStatement(
    stmt: ForStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const condition = stmt.condition
      ? this.parseExpr(stmt.condition)
      : this.getBooleanLiteral(true)
    this.ensureBooleanType(
      condition,
      'While condition must be boolean',
      stmt.line,
      stmt.column
    )

    const init = stmt.init
      ? this.parseStatement(stmt.init, functionReturnType)
      : []
    const increment = stmt.increment
      ? this.parseStatement(stmt.increment, functionReturnType)
      : []

    const bodyBlocks =
      stmt.body.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.body as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.body, functionReturnType)

    // Convert into while loop
    return [
      ...init,
      {
        opcode: 'control_while',
        fields: {},
        inputs: {
          CONDITION: { type: 'bool', value: condition.value as Reporter },
          SUBSTACK: {
            type: 'substack',
            value: [...bodyBlocks, ...increment]
          }
        }
      }
    ]
  }

  private parseUntilStatement(
    stmt: UntilStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const condition = this.parseExpr(stmt.condition)
    this.ensureBooleanType(
      condition,
      'Until condition must be boolean',
      stmt.line,
      stmt.column
    )

    const bodyBlocks =
      stmt.body.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.body as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.body, functionReturnType)

    return [
      {
        opcode: 'control_repeat_until',
        fields: {},
        inputs: {
          CONDITION: { type: 'bool', value: condition.value as Reporter },
          SUBSTACK: { type: 'substack', value: bodyBlocks }
        }
      }
    ]
  }

  private parseLoopStatement(
    stmt: LoopStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const bodyBlocks =
      stmt.body.type === 'BlockStatement'
        ? this.parseBlockStatement(
            stmt.body as BlockStatement,
            functionReturnType
          )
        : this.parseStatement(stmt.body, functionReturnType)

    return [
      {
        opcode: 'control_forever',
        fields: {},
        inputs: {
          SUBSTACK: { type: 'substack', value: bodyBlocks }
        }
      }
    ]
  }

  private parseReturnStatement(
    stmt: ReturnStatement,
    functionReturnType: 'bool' | 'void' | 'any' | null
  ): Block[] {
    if (stmt.value) {
      if (functionReturnType === 'void' || !functionReturnType) {
        throw new CompilerError(
          `Cannot return a value from a void function or a non-function context`,
          stmt.line,
          stmt.column
        )
      }
      const returnValue = this.parseExpr(stmt.value)
      if (functionReturnType === 'bool') {
        this.ensureBooleanType(
          returnValue,
          'Return value must be boolean',
          stmt.line,
          stmt.column
        )
      }
      return [
        {
          opcode: 'procedures_return',
          fields: {},
          inputs: {
            VALUE: { type: 'any', value: returnValue.value }
          }
        }
      ]
    } else {
      if (functionReturnType !== 'void' && functionReturnType !== null) {
        throw new CompilerError(
          `Must return a value from a non-void function`,
          stmt.line,
          stmt.column
        )
      }
      return [
        {
          opcode: 'control_stop',
          fields: {
            STOP_OPTION: 'this script'
          },
          inputs: {}
        }
      ]
    }
  }

  private parseBlockStatement(
    stmt: BlockStatement,
    functionReturnType: 'bool' | 'any' | 'void' | null
  ): Block[] {
    const blocks: Block[] = []
    for (const statement of stmt.body) {
      blocks.push(...this.parseStatement(statement, functionReturnType))
    }
    return blocks
  }

  // Namespace and function call helpers
  private parseNamespaceCallAsCommand(
    memberExpr: MemberExpression,
    args: Expression[],
    functionReturnType: 'bool' | 'any' | 'void' | null,
    thenBlock: BlockStatement | undefined,
    line: number,
    column: number
  ): Block[] {
    if (
      memberExpr.object.type !== 'Identifier' ||
      memberExpr.property.type !== 'Identifier'
    ) {
      throw new CompilerError(
        'Namespace calls must use simple identifiers',
        line,
        column
      )
    }

    const namespaceName = (memberExpr.object as IdentifierExpression).name
    const functionName = (memberExpr.property as IdentifierExpression).name

    const namespace = this.namespaces.get(namespaceName)
    if (!namespace) {
      throw new CompilerError(
        `Namespace ${namespaceName} not found`,
        line,
        column
      )
    }

    const entry = namespace.get(functionName)
    if (!entry) {
      throw new CompilerError(
        `Function ${functionName} not found in namespace ${namespaceName}`,
        line,
        column
      )
    }

    if (entry.type !== 'command' && entry.type !== 'conditional') {
      throw new CompilerError(
        `${namespaceName}.${functionName} cannot be used as a command (type: ${entry.type})`,
        line,
        column
      )
    }

    const parsedArgs = args.map(arg => this.parseExpr(arg))
    const inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput } =
      Object.assign({}, entry.inputs)
    const fields: { [key: string]: string } = Object.assign({}, entry.fields)

    // Find substack argument if any
    let substackArgDef = entry.args.find(arg => arg.type === 'substack')

    // Match arguments with entry arguments (non-substack arguments)
    const nonSubstackArgs = entry.args.filter(arg => arg.type !== 'substack')
    if (parsedArgs.length !== nonSubstackArgs.length) {
      throw new CompilerError(
        `${namespaceName}.${functionName} expects ${nonSubstackArgs.length} non-substack arguments, got ${parsedArgs.length}`,
        line,
        column
      )
    }

    for (let i = 0; i < nonSubstackArgs.length; i++) {
      const argDef = nonSubstackArgs[i]
      const argValue = parsedArgs[i]

      if (argDef.type === 'boolean' && argValue.type !== 'bool') {
        throw new CompilerError(
          `Argument ${argDef.name} must be boolean`,
          line,
          column
        )
      }

      if (argDef.type === 'field') {
        // Must be literal
        const rawArgValue = args[i]
        if (rawArgValue.type !== 'Literal') {
          throw new CompilerError(
            `Argument ${argDef.name} must be a literal for field`,
            line,
            column
          )
        }
        const literal = rawArgValue as LiteralExpression
        if (typeof literal.value !== 'string') {
          throw new CompilerError(
            `Argument ${argDef.name} must be a string literal`,
            line,
            column
          )
        }
        const fieldValue = String(literal.value)
        if (argDef.menu && !Object.keys(argDef.menu).includes(fieldValue)) {
          throw new CompilerError(
            `Argument ${argDef.name} has invalid value ${fieldValue}`,
            line,
            column
          )
        }
        fields[argDef.name] = argDef.menu ? argDef.menu[fieldValue] : fieldValue
      } else {
        inputs[argDef.name] =
          argValue.type === 'bool'
            ? { type: 'bool', value: argValue.value as Reporter }
            : { type: 'any', value: argValue.value }
      }
    }

    // Handle substack
    if (substackArgDef) {
      if (thenBlock) {
        inputs[substackArgDef.name] = {
          type: 'substack',
          value: this.parseBlockStatement(thenBlock, functionReturnType)
        }
      } else {
        inputs[substackArgDef.name] = { type: 'substack', value: [] }
      }
    } else if (thenBlock) {
      throw new CompilerError(
        `${namespaceName}.${functionName} does not accept a then block`,
        line,
        column
      )
    }

    return [
      {
        opcode: entry.opcode,
        fields,
        inputs
      }
    ]
  }

  private parseFunctionCallAsCommand(
    funcName: string,
    args: Expression[],
    line: number,
    column: number
  ): Block[] {
    const func = this.funcs.get(funcName)

    if (!func) {
      throw new CompilerError(`Function ${funcName} not found`, line, column)
    }

    if (func.decl.returnType.name !== 'void') {
      throw new CompilerError(
        `Function ${funcName} returns a value and cannot be used as a command`,
        line,
        column
      )
    }

    const parsedArgs = args.map(arg => this.parseExpr(arg))

    // Check argument count
    if (parsedArgs.length !== func.decl.parameters.length) {
      throw new CompilerError(
        `Function ${funcName} expects ${func.decl.parameters.length} arguments, got ${parsedArgs.length}`,
        line,
        column
      )
    }

    // Check argument types
    for (let i = 0; i < func.decl.parameters.length; i++) {
      const paramType =
        func.decl.parameters[i].type.name === 'bool' ? 'bool' : 'any'
      if (paramType === 'bool' && parsedArgs[i].type !== 'bool') {
        throw new CompilerError(
          `Parameter ${func.decl.parameters[i].name} must be boolean`,
          line,
          column
        )
      }
    }

    const inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput } =
      {}
    for (let i = 0; i < parsedArgs.length; i++) {
      const paramName = func.decl.parameters[i].name.name
      const paramType =
        func.decl.parameters[i].type.name === 'bool' ? 'bool' : 'any'
      inputs[paramName] =
        paramType === 'bool'
          ? { type: 'bool', value: parsedArgs[i].value as Reporter }
          : { type: 'any', value: parsedArgs[i].value }
    }

    const names = func.decl.parameters.map(p => p.name.name)

    return [
      {
        opcode: 'procedures_call',
        fields: {},
        inputs,
        mutation: {
          tagName: 'mutation',
          proccode: func.proccode,
          children: [],
          warp: func.decl.once ? 'true' : 'false',
          argumentids: JSON.stringify(names), // FIXME: use stable IDs
          argumentnames: JSON.stringify(names),
          argumentdefaults: JSON.stringify(
            func.decl.parameters.map(p =>
              p.type.name === 'any' ? '' : 'false'
            )
          )
        }
      }
    ]
  }
  static getFunctions(
    globalScope: Scope,
    program: Program
  ): Map<string, ScratchFunction> {
    const functions: Map<string, ScratchFunction> = new Map()
    for (const decl of program.body) {
      if (
        decl.type === 'FunctionDeclaration' ||
        decl.type === 'DecoratorStatement'
      ) {
        let funcDecl: FunctionDeclaration
        let exportName: string | null = null
        if (decl.type === 'DecoratorStatement') {
          const decorator = decl as DecoratorStatement
          if (decorator.name.name !== 'extern') {
            throw new CompilerError(
              `Unknown decorator @${decorator.name.name}`,
              decorator.line,
              decorator.column
            )
          }
          // @export("external name") fn ...
          if (decorator.target.type !== 'FunctionDeclaration') {
            continue
          }
          if (decorator.arguments.length !== 1) {
            throw new CompilerError(
              '@export takes at most one argument',
              decorator.line,
              decorator.column
            )
          }
          const arg = decorator.arguments[0]
          if (typeof arg.value !== 'string') {
            throw new CompilerError(
              '@export argument must be a string literal',
              decorator.line,
              decorator.column
            )
          }
          exportName = arg.value
          funcDecl = decorator.target as FunctionDeclaration
        } else {
          funcDecl = decl as FunctionDeclaration
        }
        const func = new ScratchFunction(
          globalScope.variables,
          funcDecl,
          exportName
        )
        if (functions.has(funcDecl.name.name)) {
          throw new CompilerError(
            `Function ${funcDecl.name.name} is already declared`,
            funcDecl.line,
            funcDecl.column
          )
        }
        functions.set(funcDecl.name.name, func)
      }
    }
    return functions
  }
}

export interface ProgramInfo {
  namespaces: Map<string, Namespace>
  variables: Map<
    string,
    [Variable, string | boolean | number | (string | boolean | number)[]]
  >
}

export function getProgramInfo(program: Program): ProgramInfo {
  const variables = new Map<
    string,
    [Variable, string | boolean | number | (string | boolean | number)[]]
  >()
  const namespaces = new Map<string, Namespace>()

  for (const decl of program.body) {
    if (
      decl.type === 'VariableDeclaration' ||
      decl.type === 'DecoratorStatement'
    ) {
      let varDecl: VariableDeclaration
      let exportName: string | null = null
      if (decl.type === 'DecoratorStatement') {
        const decorator = decl as DecoratorStatement
        if (decorator.name.name !== 'extern') {
          throw new CompilerError(
            `Unknown decorator @${decorator.name.name}`,
            decorator.line,
            decorator.column
          )
        }
        // @export("external name") global id = 1
        if (decorator.target.type !== 'VariableDeclaration') {
          continue
        }
        if (decorator.arguments.length !== 1) {
          throw new CompilerError(
            '@export takes at most one argument',
            decorator.line,
            decorator.column
          )
        }
        const arg = decorator.arguments[0]
        if (typeof arg.value !== 'string') {
          throw new CompilerError(
            '@export argument must be a string literal',
            decorator.line,
            decorator.column
          )
        }
        exportName = arg.value
        varDecl = decorator.target as VariableDeclaration
      } else {
        varDecl = decl as VariableDeclaration
      }
      let value: string | number | boolean | (string | number | boolean)[]
      if (varDecl.initializer.type === 'ArrayExpression') {
        value = (varDecl.initializer as ArrayExpression).elements.map(elem => {
          if (elem.type === 'Literal') {
            return (elem as LiteralExpression).value
          } else {
            throw new CompilerError(
              'Array elements must be literals',
              elem.line,
              elem.column
            )
          }
        })
      } else if (varDecl.initializer.type === 'Literal') {
        value = (varDecl.initializer as LiteralExpression).value
      } else {
        throw new CompilerError(
          `Variable initializer must be a literal or array, got ${varDecl.initializer.type}`,
          varDecl.initializer.line,
          varDecl.initializer.column
        )
      }
      const variable: Variable = {
        name: varDecl.name,
        exportName,
        type: Array.isArray(value) ? 'list' : 'scalar',
        isGlobal: varDecl.isGlobal
      }

      if (variables.has(varDecl.name)) {
        throw new CompilerError(
          `Variable ${varDecl.name} is already declared`,
          varDecl.line,
          varDecl.column
        )
      }

      variables.set(varDecl.name, [variable, value])
    } else if (decl.type === 'NamespaceDeclaration') {
      const nsDecl = decl as NamespaceDeclaration
      const namespaceMap = new Map()

      if (nsDecl.body && nsDecl.body.properties) {
        for (const prop of nsDecl.body.properties) {
          if (prop.value && typeof prop.value === 'object') {
            const entry = {
              opcode: prop.value.opcode || '',
              type: prop.value.type || 'command',
              args: prop.value.args || []
            }
            namespaceMap.set(prop.key, entry)
          }
        }
      }

      const existing = namespaces.get(nsDecl.name)

      if (existing) {
        // merge if already exists

        namespaces.set(nsDecl.name, mergeNamespace(existing, namespaceMap))
      } else {
        namespaces.set(nsDecl.name, namespaceMap)
      }
    }
  }

  return { namespaces, variables }
}

export function mergeNamespace(
  base: Namespace,
  additional: Namespace
): Namespace {
  const merged = new Map(base)
  for (const [key, value] of additional) {
    if (merged.has(key)) {
      throw new CompilerError(`Duplicate namespace entry: ${key}`, 0, 0)
    }
    merged.set(key, value)
  }
  return merged
}
