// 基础积木接口
export interface BlockBase {
  opcode: string
  inputs: { [key: string]: BooleanInput | AnyInput | SubstackInput }
  fields: { [key: string]: string }
  mutation?: BlockMutation
}

export interface BaseInput {
  type: 'bool' | 'any' | 'substack'
}
export interface BooleanInput extends BaseInput {
  type: 'bool'
  value: Reporter
}
export interface AnyInput extends BaseInput {
  type: 'any'
  value: string | Reporter
}
export interface SubstackInput extends BaseInput {
  type: 'substack'
  value: Block[]
}

// 积木 mutation
export type BlockMutation = Record<string, string | BlockMutation[]>

// 命令积木（包括控制流积木）
export interface Block extends BlockBase {}

// 报告积木（返回值的积木）
export interface Reporter extends BlockBase {}

// 脚本（以帽子积木开始的积木序列）
export interface Script {
  hat?: HatBlock
  blocks: Block[]
}

// 帽子积木
export interface HatBlock extends BlockBase {}

// 变量定义
export interface Variable {
  name: string
  exportName: string | null
  type: 'scalar' | 'list'
  isGlobal: boolean
}
