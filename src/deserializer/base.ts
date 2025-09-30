// 反序列化后的脚本结构
export interface DeserializedScript {
  scripts: Script[];
  variables: Variable[];
}

// 基础积木接口
export interface BlockBase {
  opcode: string;
  inputs: { [key: string]: string | Reporter | Block[] };
  fields: { [key: string]: string };
  mutation?: BlockMutation;
}

// 积木 mutation
export type BlockMutation = Record<string, string>;

// 命令积木（包括控制流积木）
export interface Block extends BlockBase {}

// 报告积木（返回值的积木）
export interface Reporter extends BlockBase {}

// 脚本（以帽子积木开始的积木序列）
export interface Script {
  hat?: HatBlock;
  blocks: Block[];
}

// 帽子积木
export interface HatBlock extends BlockBase {}

// 变量定义
export interface Variable {
  name: string;
  type: 'scalar' | 'list';
  value?: string | number | boolean | (string | number | boolean)[];
  isGlobal: boolean;
}
