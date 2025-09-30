import { Variable, Reporter, Block, Script } from './base';
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
  BlockStatement
} from '../parser';

export interface ProcedureArgument {
  name: string;
  type: 'any' | 'boolean';
}

export type ListCommand = (v: Variable, rhs: Reporter[]) => Block[];
export type ListReporter = (v: Variable, rhs: Reporter[]) => TypedValue;
// export type VarCommand = (v: Variable, rhs: Reporter[]) => Block[];
// Var methods have no side effects, so VarCommand is not needed
export type VarReporter = (v: Reporter, rhs: Reporter[]) => TypedValue;

export class Scope {
  private static listCmds = new Map<string, ListCommand>([
    [
      'push',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 1)
          throw new Error('push expects exactly one argument');
        return [
          {
            opcode: 'data_addtolist',
            fields: {
              LIST: v.name
            },
            inputs: {
              ITEM: rhs
            }
          }
        ];
      }
    ],
    [
      'pop',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 0) throw new Error('pop expects no arguments');
        return [
          {
            opcode: 'data_deleteoflist',
            fields: {
              LIST: v.name
            },
            inputs: {
              INDEX: 'last'
            }
          }
        ];
      }
    ],
    [
      'insert',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 2)
          throw new Error('insert expects exactly two arguments');
        return [
          {
            opcode: 'data_insertatlist',
            fields: {
              LIST: v.name
            },
            inputs: {
              INDEX: rhs[0],
              ITEM: rhs[1]
            }
          }
        ];
      }
    ],
    [
      'remove',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 1)
          throw new Error('remove expects exactly one argument');
        return [
          {
            opcode: 'data_deleteoflist',
            fields: {
              LIST: v.name
            },
            inputs: {
              INDEX: rhs[0]
            }
          }
        ];
      }
    ],
    [
      'replace',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 2)
          throw new Error('replace expects exactly two arguments');
        return [
          {
            opcode: 'data_replaceitemoflist',
            fields: {
              LIST: v.name
            },
            inputs: {
              INDEX: rhs[0],
              ITEM: rhs[1]
            }
          }
        ];
      }
    ],
    [
      'show',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 0) throw new Error('show expects no arguments');
        return [
          {
            opcode: 'data_showlist',
            fields: {
              LIST: v.name
            },
            inputs: {}
          }
        ];
      }
    ],
    [
      'hide',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 0) throw new Error('hide expects no arguments');
        return [
          {
            opcode: 'data_hidelist',
            fields: {
              LIST: v.name
            },
            inputs: {}
          }
        ];
      }
    ]
  ]);
  private static listReps = new Map<string, ListReporter>([
    [
      'length',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 0) throw new Error('length expects no arguments');
        return {
          type: 'any',
          value: {
            opcode: 'data_lengthoflist',
            fields: {
              LIST: v.name
            },
            inputs: {}
          }
        };
      }
    ],
    [
      'includes',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 1)
          throw new Error('includes expects exactly one argument');
        return {
          type: 'boolean',
          value: {
            opcode: 'data_listcontainsitem',
            fields: {
              LIST: v.name
            },
            inputs: {
              ITEM: rhs[0]
            }
          }
        };
      }
    ],
    [
      'at',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 1)
          throw new Error('at expects exactly one argument');
        return {
          type: 'any',
          value: {
            opcode: 'data_itemoflist',
            fields: {
              LIST: v.name
            },
            inputs: {
              INDEX: rhs[0]
            }
          }
        };
      }
    ],
    [
      'indexOf',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 1)
          throw new Error('indexOf expects exactly one argument');
        return {
          type: 'any',
          value: {
            opcode: 'data_indexoflist',
            fields: {
              LIST: v.name
            },
            inputs: {
              ITEM: rhs[0]
            }
          }
        };
      }
    ],
    [
      'length',
      (v: Variable, rhs: Reporter[]) => {
        if (rhs.length !== 0) throw new Error('length expects no arguments');
        return {
          type: 'any',
          value: {
            opcode: 'data_lengthoflist',
            fields: {
              LIST: v.name
            },
            inputs: {}
          }
        };
      }
    ]
  ]);
  private static varReps = new Map<string, VarReporter>([
    [
      'at',
      (v: Reporter, rhs: Reporter[]) => {
        if (rhs.length !== 1)
          throw new Error('at expects exactly one argument');
        return {
          type: 'any',
          value: {
            opcode: 'operator_letter_of',
            fields: {},
            inputs: {
              STRING: v,
              LETTER: rhs[0]
            }
          }
        };
      }
    ],
    [
      'includes',
      (v: Reporter, rhs: Reporter[]) => {
        if (rhs.length !== 1)
          throw new Error('contains expects exactly one argument');
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_contains',
            fields: {},
            inputs: {
              STRING1: v,
              STRING2: rhs[0]
            }
          }
        };
      }
    ],
    [
      'length',
      (v: Reporter, rhs: Reporter[]) => {
        if (rhs.length !== 0) throw new Error('length expects no arguments');
        return {
          type: 'any',
          value: {
            opcode: 'operator_length',
            fields: {},
            inputs: {
              STRING: v
            }
          }
        };
      }
    ]
  ]);
  constructor(
    private variables: Map<string, Variable>,
    private args?: Map<string, ProcedureArgument>
  ) {}
  typeof(name: string): 'list' | 'scalar' | 'arg_any' | 'arg_boolean' | null {
    const arg = this.args?.get(name);
    if (arg) return arg.type === 'boolean' ? 'arg_boolean' : 'arg_any';
    const variable = this.variables.get(name);
    if (variable) return variable.type;
    return null;
  }

  get(name: string): Reporter | null {
    const arg = this.args?.get(name);
    if (arg) {
      switch (arg.type) {
        case 'any':
          return {
            opcode: 'argument_reporter_string_number',
            fields: {
              VALUE: arg.name
            },
            inputs: {}
          };
        case 'boolean':
          return {
            opcode: 'argument_reporter_boolean',
            fields: {
              VALUE: arg.name
            },
            inputs: {}
          };
      }
    }
    const variable = this.variables.get(name);
    if (variable) {
      switch (variable.type) {
        case 'scalar':
          return {
            opcode: 'data_variable',
            fields: {
              VARIABLE: variable.name
            },
            inputs: {}
          };
        case 'list':
          return {
            opcode: 'data_listcontents',
            fields: {
              LIST: variable.name
            },
            inputs: {}
          };
      }
    }
    return null;
  }
  set(name: string, value: Reporter): Block[] {
    const arg = this.args?.get(name);
    if (arg) throw new Error(`Cannot assign to argument ${name}`);
    const variable = this.variables.get(name);
    if (variable) {
      if (variable.type === 'scalar') {
        return [
          {
            opcode: 'data_setvariableto',
            fields: {
              VARIABLE: variable.name
            },
            inputs: {
              VALUE: value
            }
          }
        ];
      } else throw new Error(`Cannot assign to list variable ${name}`);
    }
    throw new Error(`Variable ${name} not found`);
  }
  add(name: string, value: Reporter): Block[] {
    const arg = this.args?.get(name);
    if (arg) throw new Error(`Cannot add to argument ${name}`);
    const variable = this.variables.get(name);
    if (variable) {
      if (variable.type === 'scalar') {
        return [
          {
            opcode: 'data_changevariableby',
            fields: {
              VARIABLE: variable.name
            },
            inputs: {
              VALUE: value
            }
          }
        ];
      } else throw new Error(`Cannot add to list variable ${name}`);
    }
    throw new Error(`Variable ${name} not found`);
  }
  stmtMethod(name: string, func: string, args: Reporter[]): Block[] | null {
    const arg = this.args?.get(name);
    if (arg) return null;
    const variable = this.variables.get(name);
    if (variable) {
      if (variable.type === 'list') {
        const listCmd = Scope.listCmds.get(func);
        if (listCmd) return listCmd(variable, args);
        else return null;
      } else return null;
    }
    throw new Error(`Variable ${name} not found`);
  }
  exprMethod(name: string, func: string, args: Reporter[]): TypedValue | null {
    const arg = this.args?.get(name);
    if (arg) {
      const varRep = Scope.varReps.get(func);
      if (varRep)
        return varRep(
          arg.type === 'boolean'
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
        );
    }
    const variable = this.variables.get(name);
    if (variable) {
      if (variable.type === 'list') {
        const listRep = Scope.listReps.get(func);
        if (listRep) return listRep(variable, args);
        else return null;
      } else {
        const varRep = Scope.varReps.get(func);
        if (varRep)
          return varRep(
            {
              opcode: 'data_variable',
              fields: {
                VARIABLE: variable.name
              },
              inputs: {}
            },
            args
          );
        else return null;
      }
    }
    throw new Error(`Variable ${name} not found`);
  }
}

export class ScratchFunction {
  public scope: Scope;
  constructor(
    globalVars: Map<string, Variable>,
    public decl: FunctionDeclaration
  ) {
    const args = new Map<string, ProcedureArgument>();
    for (const arg of decl.parameters) {
      if (['bool', 'any'].indexOf(arg.type) === -1) {
        throw new Error(`Invalid argument type ${arg.type}`);
      }
      args.set(arg.name, {
        type: arg.type === 'bool' ? 'boolean' : 'any',
        name: arg.name
      });
    }
    this.scope = new Scope(globalVars, args);
  }
  static escape(str: string) {
    // replace % with %%
    return str.replace(/%/g, '%%');
  }
  get proccode() {
    if (this.decl.name.type === 'Identifier') {
      return `${this.decl.name.name}(${this.decl.parameters
        .map(
          p =>
            `${ScratchFunction.escape(p.name)} = %${
              p.type === 'boolean' ? 'b' : 's'
            }`
        )
        .join(', ')})`;
    } else {
      // aaa [arg1] bbb [arg2]
      if (typeof this.decl.name.value !== 'string') {
        throw new Error('Invalid function name');
      }
      return this.parseTemplateName(this.decl.name.value);
    }
  }

  private parseTemplateName(template: string): string {
    const tokens = this.lexTemplate(template);
    let result = '';
    let paramIndex = 0;

    for (const token of tokens) {
      if (token.type === 'text') {
        result += ScratchFunction.escape(token.value);
      } else if (token.type === 'param') {
        if (paramIndex >= this.decl.parameters.length) {
          throw new Error(`Too many parameter placeholders in template`);
        }
        const param = this.decl.parameters[paramIndex];
        if (token.value !== param.name) {
          throw new Error(
            `Parameter placeholder [${token.value}] does not match parameter ${param.name}`
          );
        }
        result += ` %${param.type === 'boolean' ? 'b' : 's'} `;
        paramIndex++;
      } else if (token.type === 'escaped') {
        result += token.value;
      }
    }

    if (paramIndex < this.decl.parameters.length) {
      throw new Error(
        `Missing parameter placeholders for: ${this.decl.parameters
          .slice(paramIndex)
          .map(p => p.name)
          .join(', ')}`
      );
    }

    return result.trim();
  }

  private lexTemplate(
    template: string
  ): Array<{ type: 'text' | 'param' | 'escaped'; value: string }> {
    const tokens: Array<{ type: 'text' | 'param' | 'escaped'; value: string }> =
      [];
    let i = 0;
    let current = '';

    while (i < template.length) {
      if (
        i < template.length - 1 &&
        template[i] === '[' &&
        template[i + 1] === '['
      ) {
        // Escaped opening bracket [[
        if (current) {
          tokens.push({ type: 'text', value: current });
          current = '';
        }
        tokens.push({ type: 'escaped', value: '[' });
        i += 2;
      } else if (
        i < template.length - 1 &&
        template[i] === ']' &&
        template[i + 1] === ']'
      ) {
        // Escaped closing bracket ]]
        if (current) {
          tokens.push({ type: 'text', value: current });
          current = '';
        }
        tokens.push({ type: 'escaped', value: ']' });
        i += 2;
      } else if (template[i] === '[') {
        // Start of parameter placeholder
        if (current) {
          tokens.push({ type: 'text', value: current });
          current = '';
        }
        i++; // skip opening [
        let paramName = '';
        while (i < template.length && template[i] !== ']') {
          paramName += template[i];
          i++;
        }
        if (i >= template.length) {
          throw new Error('Unclosed parameter placeholder');
        }
        tokens.push({ type: 'param', value: paramName });
        i++; // skip closing ]
      } else {
        current += template[i];
        i++;
      }
    }

    if (current) {
      tokens.push({ type: 'text', value: current });
    }

    return tokens;
  }
}

export type TypedValue = { type: 'any' | 'boolean'; value: Reporter };

export type Namespace = Map<string /** name */, NamespaceEntry>;
export interface NamespaceEntry {
  opcode: string;
  type: 'command' | 'reporter' | 'boolean' | 'conditional' | 'hat';
  args: (
    | NamespaceEntryArgumentAny
    | NamespaceEntryArgumentBoolean
    | NamespaceEntryArgumentSubstack
    | NamespaceEntryArgumentField
  )[];
}
export interface NamespaceEntryArgumentBase {
  name: string;
}

export interface NamespaceEntryArgumentAny extends NamespaceEntryArgumentBase {
  type: 'any';
  defaultValue?: string;
}

export interface NamespaceEntryArgumentBoolean
  extends NamespaceEntryArgumentBase {
  type: 'boolean';
  defaultValue?: boolean;
}
export interface NamespaceEntryArgumentSubstack
  extends NamespaceEntryArgumentBase {
  type: 'substack';
}
export interface NamespaceEntryArgumentField
  extends NamespaceEntryArgumentBase {
  type: 'field';
  menu: [string, string][];
  allowReporters?: boolean; // default false
  defaultValue?: string;
}

export class Deserializer {
  private globalScope: Scope;

  constructor(
    public variables: Map<string, Variable>,
    public funcs: ScratchFunction[],
    public namespaces: Map<string, Namespace>
  ) {
    this.globalScope = new Scope(variables);
  }
  parse(stmt: Program): Script[];
  parse(
    stmt: Statement,
    functionReturnType?: 'boolean' | 'any' | 'void'
  ): Block[];
  parse(
    stmt: Statement | Program,
    functionReturnType?: 'boolean' | 'any' | 'void'
  ): Block[] | Script[] {
    if (stmt.type === 'Program') {
      return this.parseProgram(stmt as Program);
    } else {
      return this.parseStatement(stmt as Statement, functionReturnType);
    }
  }
  parseExpr(stmt: Expression): TypedValue {
    switch (stmt.type) {
      case 'Literal':
        return this.parseLiteralExpression(stmt as LiteralExpression);
      case 'Identifier':
        return this.parseIdentifierExpression(stmt as IdentifierExpression);
      case 'BinaryExpression':
        return this.parseBinaryExpression(stmt as BinaryExpression);
      case 'UnaryExpression':
        return this.parseUnaryExpression(stmt as UnaryExpression);
      case 'CallExpression':
        return this.parseCallExpressionAsReporter(stmt as CallExpression);
      case 'MemberExpression':
        return this.parseMemberExpression(stmt as MemberExpression);
      case 'ArrayExpression':
        throw new Error(
          'ArrayExpression is only allowed in variable declarations'
        );
      default:
        throw new Error(`Unsupported expression type: ${stmt.type}`);
    }
  }

  // Helper methods for parseExpr
  private parseLiteralExpression(expr: LiteralExpression): TypedValue {
    const value = expr.value;
    if (typeof value === 'boolean') {
      if (value)
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {}
          }
        };
      return {
        type: 'boolean',
        value: {
          opcode: 'operator_not',
          fields: {},
          inputs: {
            OPERAND: {
              opcode: 'operator_not',
              fields: {},
              inputs: {}
            }
          }
        }
      };
    } else {
      return {
        type: 'any',
        value: {
          opcode: 'text',
          fields: {
            TEXT: String(value)
          },
          inputs: {}
        }
      };
    }
  }

  private parseIdentifierExpression(expr: IdentifierExpression): TypedValue {
    const name = expr.name;
    const reporter = this.globalScope.get(name);
    if (reporter) {
      const varType = this.globalScope.typeof(name);
      // Convert variable types to TypedValue types
      if (varType === 'arg_boolean') {
        return { type: 'boolean', value: reporter };
      } else {
        return { type: 'any', value: reporter };
      }
    }
    throw new Error(`Variable ${name} not found`);
  }

  private parseBinaryExpression(expr: BinaryExpression): TypedValue {
    const left = this.parseExpr(expr.left);
    const right = this.parseExpr(expr.right);

    switch (expr.operator) {
      case '+':
        return {
          type: 'any',
          value: {
            opcode: 'operator_add',
            fields: {},
            inputs: {
              NUM1: left.value,
              NUM2: right.value
            }
          }
        };
      case '-':
        return {
          type: 'any',
          value: {
            opcode: 'operator_subtract',
            fields: {},
            inputs: {
              NUM1: left.value,
              NUM2: right.value
            }
          }
        };
      case '*':
        return {
          type: 'any',
          value: {
            opcode: 'operator_multiply',
            fields: {},
            inputs: {
              NUM1: left.value,
              NUM2: right.value
            }
          }
        };
      case '/':
        return {
          type: 'any',
          value: {
            opcode: 'operator_divide',
            fields: {},
            inputs: {
              NUM1: left.value,
              NUM2: right.value
            }
          }
        };
      case '%':
        return {
          type: 'any',
          value: {
            opcode: 'operator_mod',
            fields: {},
            inputs: {
              NUM1: left.value,
              NUM2: right.value
            }
          }
        };
      case '..':
        return {
          type: 'any',
          value: {
            opcode: 'operator_join',
            fields: {},
            inputs: {
              STRING1: left.value,
              STRING2: right.value
            }
          }
        };
      case '==':
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_equals',
            fields: {},
            inputs: {
              OPERAND1: left.value,
              OPERAND2: right.value
            }
          }
        };
      case '!=':
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                opcode: 'operator_equals',
                fields: {},
                inputs: {
                  OPERAND1: left.value,
                  OPERAND2: right.value
                }
              }
            }
          }
        };
      case '<':
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_lt',
            fields: {},
            inputs: {
              OPERAND1: left.value,
              OPERAND2: right.value
            }
          }
        };
      case '>':
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_gt',
            fields: {},
            inputs: {
              OPERAND1: left.value,
              OPERAND2: right.value
            }
          }
        };
      case '<=':
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                opcode: 'operator_gt',
                fields: {},
                inputs: {
                  OPERAND1: left.value,
                  OPERAND2: right.value
                }
              }
            }
          }
        };
      case '>=':
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: {
                opcode: 'operator_lt',
                fields: {},
                inputs: {
                  OPERAND1: left.value,
                  OPERAND2: right.value
                }
              }
            }
          }
        };
      case '&&':
        this.ensureBooleanType(left, 'Left operand of && must be boolean');
        this.ensureBooleanType(right, 'Right operand of && must be boolean');
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_and',
            fields: {},
            inputs: {
              OPERAND1: left.value,
              OPERAND2: right.value
            }
          }
        };
      case '||':
        this.ensureBooleanType(left, 'Left operand of || must be boolean');
        this.ensureBooleanType(right, 'Right operand of || must be boolean');
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_or',
            fields: {},
            inputs: {
              OPERAND1: left.value,
              OPERAND2: right.value
            }
          }
        };
      default:
        throw new Error(`Unsupported binary operator: ${expr.operator}`);
    }
  }

  private parseUnaryExpression(expr: UnaryExpression): TypedValue {
    const operand = this.parseExpr(expr.operand);

    switch (expr.operator) {
      case '!':
        this.ensureBooleanType(operand, 'Operand of ! must be boolean');
        return {
          type: 'boolean',
          value: {
            opcode: 'operator_not',
            fields: {},
            inputs: {
              OPERAND: operand.value
            }
          }
        };
      case '-':
        return {
          type: 'any',
          value: {
            opcode: 'operator_subtract',
            fields: {},
            inputs: {
              NUM1: {
                opcode: 'text',
                fields: { TEXT: '0' },
                inputs: {}
              },
              NUM2: operand.value
            }
          }
        };
      default:
        throw new Error(`Unsupported unary operator: ${expr.operator}`);
    }
  }

  private parseCallExpressionAsReporter(expr: CallExpression): TypedValue {
    if (expr.then) {
      throw new Error(
        'Call expressions with then blocks cannot be used as reporters'
      );
    }

    if (expr.callee.type === 'MemberExpression') {
      const memberExpr = expr.callee as MemberExpression;
      return this.parseNamespaceCallAsReporter(memberExpr, expr.arguments);
    } else if (expr.callee.type === 'Identifier') {
      const funcName = (expr.callee as IdentifierExpression).name;
      return this.parseFunctionCallAsReporter(funcName, expr.arguments);
    } else {
      throw new Error('Unsupported call expression callee type');
    }
  }

  private parseMemberExpression(expr: MemberExpression): TypedValue {
    if (expr.object.type !== 'Identifier') {
      throw new Error('Only simple member expressions are supported');
    }

    const objectName = (expr.object as IdentifierExpression).name;

    if (expr.computed) {
      // Handle computed access like test[1]
      const index = this.parseExpr(expr.property);
      return this.parseComputedAccess(objectName, index);
    } else {
      // Handle dot notation like test.method
      const propertyName = (expr.property as IdentifierExpression).name;

      // Check if this is a variable method call
      const varType = this.globalScope.typeof(objectName);
      if (varType) {
        const methodResult = this.globalScope.exprMethod(
          objectName,
          propertyName,
          []
        );
        if (methodResult) {
          return methodResult;
        }
      }

      throw new Error(
        `Member ${propertyName} not found on ${objectName} or member functions cannot be used as values`
      );
    }
  }

  private parseNamespaceCallAsReporter(
    memberExpr: MemberExpression,
    args: Expression[]
  ): TypedValue {
    if (
      memberExpr.object.type !== 'Identifier' ||
      memberExpr.property.type !== 'Identifier'
    ) {
      throw new Error('Namespace calls must use simple identifiers');
    }

    const namespaceName = (memberExpr.object as IdentifierExpression).name;
    const functionName = (memberExpr.property as IdentifierExpression).name;

    const namespace = this.namespaces.get(namespaceName);
    if (!namespace) {
      throw new Error(`Namespace ${namespaceName} not found`);
    }

    const entry = namespace.get(functionName);
    if (!entry) {
      throw new Error(
        `Function ${functionName} not found in namespace ${namespaceName}`
      );
    }

    if (entry.type !== 'reporter' && entry.type !== 'boolean') {
      throw new Error(
        `${namespaceName}.${functionName} cannot be used as a reporter (type: ${entry.type})`
      );
    }

    const parsedArgs = args.map(arg => this.parseExpr(arg));
    const inputs: { [key: string]: Reporter } = {};

    // Match arguments with entry arguments
    if (parsedArgs.length !== entry.args.length) {
      throw new Error(
        `${namespaceName}.${functionName} expects ${entry.args.length} arguments, got ${parsedArgs.length}`
      );
    }

    for (let i = 0; i < entry.args.length; i++) {
      const argDef = entry.args[i];
      const argValue = parsedArgs[i];

      if (argDef.type === 'boolean' && argValue.type !== 'boolean') {
        throw new Error(`Argument ${argDef.name} must be boolean`);
      }

      inputs[argDef.name] = argValue.value;
    }

    return {
      type: entry.type === 'boolean' ? 'boolean' : 'any',
      value: {
        opcode: entry.opcode,
        fields: {},
        inputs
      }
    };
  }

  private parseFunctionCallAsReporter(
    funcName: string,
    args: Expression[]
  ): TypedValue {
    const func = this.funcs.find(f => {
      if (f.decl.name.type === 'Identifier') {
        return f.decl.name.name === funcName;
      }
      return false;
    });

    if (!func) {
      throw new Error(`Function ${funcName} not found`);
    }

    if (func.decl.returnType === 'void') {
      throw new Error(
        `Function ${funcName} returns void and cannot be used as a reporter`
      );
    }

    const parsedArgs = args.map(arg => this.parseExpr(arg));

    // Check argument count
    if (parsedArgs.length !== func.decl.parameters.length) {
      throw new Error(
        `Function ${funcName} expects ${func.decl.parameters.length} arguments, got ${parsedArgs.length}`
      );
    }

    // Check argument types
    for (let i = 0; i < func.decl.parameters.length; i++) {
      const paramType =
        func.decl.parameters[i].type === 'bool' ? 'boolean' : 'any';
      if (paramType === 'boolean' && parsedArgs[i].type !== 'boolean') {
        throw new Error(
          `Parameter ${func.decl.parameters[i].name} must be boolean`
        );
      }
    }

    const inputs: { [key: string]: Reporter } = {};
    for (let i = 0; i < parsedArgs.length; i++) {
      const paramName = func.decl.parameters[i].name;
      inputs[paramName] = parsedArgs[i].value;
    }

    return {
      type: func.decl.returnType === 'bool' ? 'boolean' : 'any',
      value: {
        opcode: 'procedures_call',
        fields: {
          PROCCODE: func.proccode
        },
        inputs
      }
    };
  }

  // Type checking utility
  private ensureBooleanType(value: TypedValue, message: string): void {
    if (value.type !== 'boolean') {
      throw new Error(message);
    }
  }

  // Type compatibility checking
  private isTypeCompatible(
    from: 'any' | 'boolean',
    to: 'any' | 'boolean'
  ): boolean {
    // boolean can be converted to any, but not vice versa
    return from === to || (from === 'boolean' && to === 'any');
  }

  // Handle computed access (test[index])
  private parseComputedAccess(
    objectName: string,
    index: TypedValue
  ): TypedValue {
    const varType = this.globalScope.typeof(objectName);
    if (!varType) {
      throw new Error(`Variable ${objectName} not found`);
    }

    if (varType === 'list') {
      // For lists, use list.at(index)
      const methodResult = this.globalScope.exprMethod(objectName, 'at', [
        index.value
      ]);
      if (methodResult) {
        return methodResult;
      }
      throw new Error(`Method 'at' not found on list ${objectName}`);
    } else if (varType === 'scalar' || varType === 'arg_any') {
      // For any/scalar, use string.at(index)
      const methodResult = this.globalScope.exprMethod(objectName, 'at', [
        index.value
      ]);
      if (methodResult) {
        return methodResult;
      }
      throw new Error(`Method 'at' not found on variable ${objectName}`);
    } else {
      throw new Error(`Cannot use computed access on ${varType}`);
    }
  }

  // Parse Program (top-level)
  private parseProgram(program: Program): Script[] {
    const scripts: Script[] = [];

    for (const stmt of program.body) {
      switch (stmt.type) {
        case 'ExpressionStatement':
          const exprStmt = stmt as ExpressionStatement;
          if (exprStmt.expression.type === 'CallExpression') {
            const callExpr = exprStmt.expression as CallExpression;
            if (callExpr.then) {
              // Check if this is a hat block call
              const hatScript = this.parseHatCall(callExpr);
              if (hatScript) {
                scripts.push(hatScript);
                continue;
              }
            }
          }
          // Fall through to handle as regular expression if not a hat call
          throw new Error('Top-level expressions must be hat block calls');

        case 'NamespaceDeclaration':
        case 'FunctionDeclaration':
        case 'VariableDeclaration':
          // Skip these as they are handled during initialization
          break;

        default:
          throw new Error(
            `Statement type ${stmt.type} is not allowed at top level`
          );
      }
    }

    return scripts;
  }

  // Parse Statement (in function/block context)
  private parseStatement(
    stmt: Statement,
    functionReturnType?: 'boolean' | 'any' | 'void'
  ): Block[] {
    switch (stmt.type) {
      case 'ExpressionStatement':
        return this.parseExpressionStatement(stmt as ExpressionStatement);
      case 'AssignmentStatement':
        return this.parseAssignmentStatement(stmt as AssignmentStatement);
      case 'IfStatement':
        return this.parseIfStatement(stmt as IfStatement);
      case 'WhileStatement':
        return this.parseWhileStatement(stmt as WhileStatement);
      case 'UntilStatement':
        return this.parseUntilStatement(stmt as UntilStatement);
      case 'LoopStatement':
        return this.parseLoopStatement(stmt as LoopStatement);
      case 'ReturnStatement':
        return this.parseReturnStatement(
          stmt as ReturnStatement,
          functionReturnType
        );
      case 'BlockStatement':
        return this.parseBlockStatement(stmt as BlockStatement);
      case 'NamespaceDeclaration':
        throw new Error(
          'Namespace declarations are not allowed in function bodies'
        );
      case 'VariableDeclaration':
        throw new Error(
          'Variable declarations are not allowed in function bodies (use assignment instead)'
        );
      case 'FunctionDeclaration':
        throw new Error(
          'Function declarations are not allowed in function bodies'
        );
      default:
        throw new Error(`Unsupported statement type: ${stmt.type}`);
    }
  }

  // Parse hat block calls
  private parseHatCall(callExpr: CallExpression): Script | null {
    if (!callExpr.then) return null;

    if (callExpr.callee.type === 'MemberExpression') {
      const memberExpr = callExpr.callee as MemberExpression;
      if (
        memberExpr.object.type === 'Identifier' &&
        memberExpr.property.type === 'Identifier'
      ) {
        const namespaceName = (memberExpr.object as IdentifierExpression).name;
        const functionName = (memberExpr.property as IdentifierExpression).name;

        const namespace = this.namespaces.get(namespaceName);
        if (!namespace) return null;

        const entry = namespace.get(functionName);
        if (!entry || entry.type !== 'hat') return null;

        const parsedArgs = callExpr.arguments.map(arg => this.parseExpr(arg));
        const inputs: { [key: string]: Reporter } = {};

        // Match arguments with entry arguments
        if (parsedArgs.length !== entry.args.length) {
          throw new Error(
            `${namespaceName}.${functionName} expects ${entry.args.length} arguments, got ${parsedArgs.length}`
          );
        }

        for (let i = 0; i < entry.args.length; i++) {
          const argDef = entry.args[i];
          const argValue = parsedArgs[i];

          if (argDef.type === 'boolean' && argValue.type !== 'boolean') {
            throw new Error(`Argument ${argDef.name} must be boolean`);
          }

          inputs[argDef.name] = argValue.value;
        }

        const hat = {
          opcode: entry.opcode,
          fields: {},
          inputs
        };

        const blocks = this.parseBlockStatement(callExpr.then);

        return { hat, blocks };
      }
    } else if (callExpr.callee.type === 'Identifier') {
      // Function call with no arguments but with then block, treat as hat if it's a hat
      const funcName = (callExpr.callee as IdentifierExpression).name;

      // Check if this is a namespace member without arguments (like event.greenflag)
      // This case should not happen as parser wraps it in CallExpression
      return null;
    }

    return null;
  }

  // Parse hat member calls (like event.greenflag {})
  // This method is no longer needed as parser wraps member expressions with then blocks into CallExpressions

  // Statement parsing methods
  private parseExpressionStatement(stmt: ExpressionStatement): Block[] {
    const expr = stmt.expression;

    if (expr.type === 'CallExpression') {
      const callExpr = expr as CallExpression;

      // Parse as command call
      if (callExpr.callee.type === 'MemberExpression') {
        const memberExpr = callExpr.callee as MemberExpression;
        if (
          memberExpr.object.type === 'Identifier' &&
          memberExpr.property.type === 'Identifier'
        ) {
          const objectName = (memberExpr.object as IdentifierExpression).name;
          const propertyName = (memberExpr.property as IdentifierExpression)
            .name;

          // First check if this is a namespace call
          const namespace = this.namespaces.get(objectName);
          if (namespace) {
            return this.parseNamespaceCallAsCommand(
              memberExpr,
              callExpr.arguments,
              callExpr.then
            );
          }

          // Then check if this is a method call on a variable
          const varType = this.globalScope.typeof(objectName);
          if (varType) {
            const args = callExpr.arguments.map(
              arg => this.parseExpr(arg).value
            );
            const methodResult = this.globalScope.stmtMethod(
              objectName,
              propertyName,
              args
            );
            if (methodResult) {
              return methodResult;
            }
            // If stmtMethod returns null, the method doesn't exist
            throw new Error(
              `Method ${propertyName} not found on ${objectName} or cannot be used as a statement`
            );
          }

          throw new Error(
            `Object ${objectName} not found (neither namespace nor variable)`
          );
        }
      } else if (callExpr.callee.type === 'Identifier') {
        const funcName = (callExpr.callee as IdentifierExpression).name;
        return this.parseFunctionCallAsCommand(funcName, callExpr.arguments);
      }
    }

    throw new Error(
      'Expression statements must be function calls or method calls'
    );
  }

  private parseAssignmentStatement(stmt: AssignmentStatement): Block[] {
    if (stmt.left.type === 'Identifier') {
      // Simple variable assignment
      const varName = (stmt.left as IdentifierExpression).name;
      const rightValue = this.parseExpr(stmt.right);

      switch (stmt.operator) {
        case '=':
          return this.globalScope.set(varName, rightValue.value);
        case '+=':
          return this.globalScope.add(varName, rightValue.value);
        case '-=':
          return this.globalScope.add(varName, {
            opcode: 'operator_subtract',
            fields: {},
            inputs: {
              NUM1: { opcode: 'text', fields: { TEXT: '0' }, inputs: {} },
              NUM2: rightValue.value
            }
          });
        default:
          throw new Error(`Unsupported assignment operator: ${stmt.operator}`);
      }
    } else if (stmt.left.type === 'MemberExpression') {
      // Computed member assignment like test[1] = 2
      const memberExpr = stmt.left as MemberExpression;

      if (!memberExpr.computed) {
        throw new Error('Dot notation assignments are not supported');
      }

      if (memberExpr.object.type !== 'Identifier') {
        throw new Error('Only simple computed assignments are supported');
      }

      const objectName = (memberExpr.object as IdentifierExpression).name;
      const varType = this.globalScope.typeof(objectName);

      if (varType !== 'list') {
        throw new Error('Computed assignment is only supported for lists');
      }

      if (stmt.operator !== '=') {
        throw new Error(
          'Only simple assignment (=) is supported for computed access'
        );
      }

      const index = this.parseExpr(memberExpr.property);
      const value = this.parseExpr(stmt.right);

      // Use list.replace(index, value)
      const methodResult = this.globalScope.stmtMethod(objectName, 'replace', [
        index.value,
        value.value
      ]);
      if (methodResult) {
        return methodResult;
      }
      throw new Error(`Method 'replace' not found on list ${objectName}`);
    } else {
      throw new Error('Unsupported assignment target');
    }
  }

  private parseIfStatement(stmt: IfStatement): Block[] {
    const condition = this.parseExpr(stmt.condition);
    this.ensureBooleanType(condition, 'If condition must be boolean');

    const thenBlocks = this.parseBlockStatement(stmt.then);
    const elseBlocks = stmt.else ? this.parseBlockStatement(stmt.else) : [];

    return [
      {
        opcode: elseBlocks.length > 0 ? 'control_if_else' : 'control_if',
        fields: {},
        inputs: {
          CONDITION: condition.value,
          SUBSTACK: thenBlocks,
          ...(elseBlocks.length > 0 && { SUBSTACK2: elseBlocks })
        }
      }
    ];
  }

  private parseWhileStatement(stmt: WhileStatement): Block[] {
    const condition = this.parseExpr(stmt.condition);
    this.ensureBooleanType(condition, 'While condition must be boolean');

    const bodyBlocks = this.parseBlockStatement(stmt.body);

    return [
      {
        opcode: 'control_while',
        fields: {},
        inputs: {
          CONDITION: condition.value,
          SUBSTACK: bodyBlocks
        }
      }
    ];
  }

  private parseUntilStatement(stmt: UntilStatement): Block[] {
    const condition = this.parseExpr(stmt.condition);
    this.ensureBooleanType(condition, 'Until condition must be boolean');

    const bodyBlocks = this.parseBlockStatement(stmt.body);

    return [
      {
        opcode: 'control_repeat_until',
        fields: {},
        inputs: {
          CONDITION: condition.value,
          SUBSTACK: bodyBlocks
        }
      }
    ];
  }

  private parseLoopStatement(stmt: LoopStatement): Block[] {
    const bodyBlocks = this.parseBlockStatement(stmt.body);

    return [
      {
        opcode: 'control_forever',
        fields: {},
        inputs: {
          SUBSTACK: bodyBlocks
        }
      }
    ];
  }

  private parseReturnStatement(
    stmt: ReturnStatement,
    functionReturnType?: 'boolean' | 'void' | 'any'
  ): Block[] {
    if (stmt.value) {
      if (functionReturnType === 'void' || !functionReturnType) {
        throw new Error(
          'Cannot return a value from a void function or a non-function context'
        );
      }
      const returnValue = this.parseExpr(stmt.value);
      return [
        {
          opcode: 'procedures_return',
          fields: {},
          inputs: {
            VALUE: returnValue.value
          }
        }
      ];
    } else {
      return [
        {
          opcode: 'control_stop',
          fields: {
            STOP_OPTION: 'this script'
          },
          inputs: {},
          mutation: {}
        }
      ];
    }
  }

  private parseBlockStatement(stmt: BlockStatement): Block[] {
    const blocks: Block[] = [];
    for (const statement of stmt.body) {
      blocks.push(...this.parseStatement(statement));
    }
    return blocks;
  }

  // Namespace and function call helpers
  private parseNamespaceCallAsCommand(
    memberExpr: MemberExpression,
    args: Expression[],
    thenBlock?: BlockStatement
  ): Block[] {
    if (
      memberExpr.object.type !== 'Identifier' ||
      memberExpr.property.type !== 'Identifier'
    ) {
      throw new Error('Namespace calls must use simple identifiers');
    }

    const namespaceName = (memberExpr.object as IdentifierExpression).name;
    const functionName = (memberExpr.property as IdentifierExpression).name;

    const namespace = this.namespaces.get(namespaceName);
    if (!namespace) {
      throw new Error(`Namespace ${namespaceName} not found`);
    }

    const entry = namespace.get(functionName);
    if (!entry) {
      throw new Error(
        `Function ${functionName} not found in namespace ${namespaceName}`
      );
    }

    if (entry.type !== 'command' && entry.type !== 'conditional') {
      throw new Error(
        `${namespaceName}.${functionName} cannot be used as a command (type: ${entry.type})`
      );
    }

    const parsedArgs = args.map(arg => this.parseExpr(arg));
    const inputs: { [key: string]: Reporter | Block[] } = {};

    // Find substack argument if any
    let substackArgDef = entry.args.find(arg => arg.type === 'substack');

    // Match arguments with entry arguments (non-substack arguments)
    const nonSubstackArgs = entry.args.filter(arg => arg.type !== 'substack');
    if (parsedArgs.length !== nonSubstackArgs.length) {
      throw new Error(
        `${namespaceName}.${functionName} expects ${nonSubstackArgs.length} non-substack arguments, got ${parsedArgs.length}`
      );
    }

    for (let i = 0; i < nonSubstackArgs.length; i++) {
      const argDef = nonSubstackArgs[i];
      const argValue = parsedArgs[i];

      if (argDef.type === 'boolean' && argValue.type !== 'boolean') {
        throw new Error(`Argument ${argDef.name} must be boolean`);
      }

      inputs[argDef.name] = argValue.value;
    }

    // Handle substack
    if (substackArgDef) {
      if (thenBlock) {
        inputs[substackArgDef.name] = this.parseBlockStatement(thenBlock);
      } else {
        inputs[substackArgDef.name] = [];
      }
    } else if (thenBlock) {
      throw new Error(
        `${namespaceName}.${functionName} does not accept a then block`
      );
    }

    return [
      {
        opcode: entry.opcode,
        fields: {},
        inputs
      }
    ];
  }

  private parseFunctionCallAsCommand(
    funcName: string,
    args: Expression[]
  ): Block[] {
    const func = this.funcs.find(f => {
      if (f.decl.name.type === 'Identifier') {
        return f.decl.name.name === funcName;
      }
      return false;
    });

    if (!func) {
      throw new Error(`Function ${funcName} not found`);
    }

    if (func.decl.returnType !== 'void') {
      throw new Error(
        `Function ${funcName} returns a value and cannot be used as a command`
      );
    }

    const parsedArgs = args.map(arg => this.parseExpr(arg));

    // Check argument count
    if (parsedArgs.length !== func.decl.parameters.length) {
      throw new Error(
        `Function ${funcName} expects ${func.decl.parameters.length} arguments, got ${parsedArgs.length}`
      );
    }

    // Check argument types
    for (let i = 0; i < func.decl.parameters.length; i++) {
      const paramType =
        func.decl.parameters[i].type === 'bool' ? 'boolean' : 'any';
      if (paramType === 'boolean' && parsedArgs[i].type !== 'boolean') {
        throw new Error(
          `Parameter ${func.decl.parameters[i].name} must be boolean`
        );
      }
    }

    const inputs: { [key: string]: Reporter } = {};
    for (let i = 0; i < parsedArgs.length; i++) {
      const paramName = func.decl.parameters[i].name;
      inputs[paramName] = parsedArgs[i].value;
    }

    return [
      {
        opcode: 'procedures_call',
        fields: {},
        inputs,
        mutation: {
          proccode: func.proccode,
          warp: func.decl.once ? 'true' : 'false',
          argumentids: func.decl.parameters.map(p => p.name).join(' ')
        }
      }
    ];
  }
}
