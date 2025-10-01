import {
  ScratchFunction,
  Script,
  Block,
  Reporter,
  CompiledFunction,
  BooleanInput,
  AnyInput,
  SubstackInput
} from '../compiler'
import { Sb3Workspace, Sb3Block, Sb3Input, Sb3Field } from './base'
import uid from './uid'

/**
 * 序列化上下文，用于跟踪已序列化的积木和避免重复
 */
interface SerializationContext {
  workspace: Sb3Workspace
  reporterCache: Map<string, string> // Reporter 内容哈希 -> 积木 ID
}

/**
 * 创建序列化上下文
 */
function createContext(): SerializationContext {
  return {
    workspace: {},
    reporterCache: new Map()
  }
}

/**
 * 将 Reporter 转换为 Sb3Input
 */
function serializeReporter(
  reporter: Reporter,
  context: SerializationContext,
  parentId?: string
): [string, Sb3Block] {
  // const hash = getReporterHash(reporter)

  // // 检查是否已经序列化过相同的 Reporter
  // const existingId = context.reporterCache.get(hash)
  // if (existingId && context.workspace[existingId]) {
  //   return [existingId, context.workspace[existingId]]
  // }

  const id = uid()
  const sb3Block: Sb3Block = {
    opcode: reporter.opcode,
    next: null,
    parent: parentId || null,
    inputs: {},
    fields: {},
    shadow: false,
    topLevel: false
  }

  // 处理 inputs
  for (const [key, value] of Object.entries(reporter.inputs)) {
    sb3Block.inputs[key] = serializeInput(value, context, id)
  }

  // 处理 fields
  for (const [key, value] of Object.entries(reporter.fields)) {
    sb3Block.fields[key] = [value, null] as Sb3Field
  }

  // 处理 mutation
  if (reporter.mutation) {
    sb3Block.mutation = reporter.mutation
  }

  context.workspace[id] = sb3Block

  return [id, sb3Block]
}

/**
 * 将输入值转换为 Sb3Input
 */
function serializeInput(
  input: BooleanInput | AnyInput | SubstackInput,
  context: SerializationContext,
  parentId?: string
): Sb3Input {
  if (input.type === 'any') {
    if (typeof input.value === 'string') {
      // 字符串字面量
      return [1, [10, input.value]]
    } else {
      // Reporter 积木
      const [reporterId] = serializeReporter(input.value, context, parentId)
      return [3, reporterId, [10, '']]
    }
  } else if (input.type === 'bool') {
    // input.value 是一个 Reporter
    const [reporterId] = serializeReporter(input.value, context, parentId)
    return [2, reporterId]
  } else if (input.type === 'substack') {
    if (input.value.length === 0) {
      throw new Error('Cannot serialize empty substack')
    }
    // 是一个 substack
    const substackIds: string[] = []
    for (const block of input.value) {
      const id = uid()
      const sb3Block = serializeBlock(block, context, id)
      context.workspace[id] = sb3Block
      substackIds.push(id)
    }

    // 设置 substack 内积木的链接关系
    for (let i = 0; i < substackIds.length; i++) {
      const currentId = substackIds[i]
      const nextId = i < substackIds.length - 1 ? substackIds[i + 1] : null
      const parentId = i > 0 ? substackIds[i - 1] : null

      context.workspace[currentId].next = nextId
      context.workspace[currentId].parent = parentId
    }

    return [2, substackIds[0]]
  }

  throw new Error(`Unsupported input type: ${JSON.stringify(input)}`)
}

/**
 * 将单个 Block 转换为 Sb3Block
 */
function serializeBlock(
  block: Block,
  context: SerializationContext,
  blockId?: string
): Sb3Block {
  const sb3Block: Sb3Block = {
    opcode: block.opcode,
    next: null,
    parent: null,
    inputs: {},
    fields: {},
    shadow: false,
    topLevel: false
  }

  // 处理 inputs
  for (const [key, value] of Object.entries(block.inputs)) {
    sb3Block.inputs[key] = serializeInput(value, context, blockId)
  }

  // 处理 fields
  for (const [key, value] of Object.entries(block.fields)) {
    sb3Block.fields[key] = [value, ''] as Sb3Field
  }

  // 处理 mutation
  if (block.mutation) {
    sb3Block.mutation = block.mutation
  }

  return sb3Block
}

export function serializeBlocks(blocks: Block[]): Sb3Workspace {
  if (blocks.length === 0) {
    return {}
  }

  const context = createContext()
  const blockIds: string[] = []

  // 序列化所有积木
  for (const block of blocks) {
    const id = uid()
    const sb3Block = serializeBlock(block, context, id)
    context.workspace[id] = sb3Block
    blockIds.push(id)
  }

  // 设置积木链接关系
  for (let i = 0; i < blockIds.length; i++) {
    const currentId = blockIds[i]
    const nextId = i < blockIds.length - 1 ? blockIds[i + 1] : null
    const parentId = i > 0 ? blockIds[i - 1] : null

    context.workspace[currentId].next = nextId
    context.workspace[currentId].parent = parentId
  }

  return context.workspace
}

export function serializeScript(script: Script): Sb3Workspace {
  const context = createContext()
  let topLevelId: string | null = null

  // 处理帽子积木
  if (script.hat) {
    const hatId = uid()
    const hatBlock: Sb3Block = {
      opcode: script.hat.opcode,
      next: null,
      parent: null,
      inputs: {},
      fields: {},
      shadow: false,
      topLevel: true,
      x: 0,
      y: 0
    }

    // 处理帽子积木的 inputs 和 fields
    for (const [key, value] of Object.entries(script.hat.inputs)) {
      hatBlock.inputs[key] = serializeInput(value, context, hatId)
    }

    for (const [key, value] of Object.entries(script.hat.fields)) {
      hatBlock.fields[key] = [value, ''] as Sb3Field
    }

    if (script.hat.mutation) {
      hatBlock.mutation = script.hat.mutation
    }

    context.workspace[hatId] = hatBlock
    topLevelId = hatId
  }

  // 处理积木序列
  if (script.blocks.length > 0) {
    const blockIds: string[] = []

    // 序列化所有积木
    for (const block of script.blocks) {
      const id = uid()
      const sb3Block = serializeBlock(block, context, id)
      context.workspace[id] = sb3Block
      blockIds.push(id)
    }

    // 设置积木链接关系
    for (let i = 0; i < blockIds.length; i++) {
      const currentId = blockIds[i]
      const nextId = i < blockIds.length - 1 ? blockIds[i + 1] : null
      const parentId = i > 0 ? blockIds[i - 1] : null

      context.workspace[currentId].next = nextId
      context.workspace[currentId].parent = parentId
    }

    const firstBlockId = blockIds[0]

    if (topLevelId) {
      // 连接帽子积木和第一个普通积木
      context.workspace[topLevelId].next = firstBlockId
      context.workspace[firstBlockId].parent = topLevelId
    } else {
      // 没有帽子积木，第一个积木就是顶层积木
      context.workspace[firstBlockId].topLevel = true
      context.workspace[firstBlockId].x = 0
      context.workspace[firstBlockId].y = 0
    }
  } else if (topLevelId) {
    // 只有帽子积木，没有其他积木
    context.workspace[topLevelId].topLevel = true
  }

  return context.workspace
}

export function serializeFunction(func: CompiledFunction): Sb3Workspace {
  const context = createContext()

  // 创建函数定义积木
  const definitionId = uid()
  const prototypeId = uid()

  // 生成参数 ID
  // const argumentIds = func.decl.parameters.map(() => uid())
  const argumentNames = func.decl.parameters.map(p => p.name.name)

  const argumentReporterIds = Object.fromEntries(
    func.decl.parameters.map(p => {
      return [p.name.name, uid()]
    })
  )
  // 函数原型积木
  const prototypeBlock: Sb3Block = {
    opcode: 'procedures_prototype',
    next: null,
    parent: definitionId,
    inputs: {},
    fields: {},
    shadow: true,
    topLevel: false,
    mutation: {
      tagName: 'mutation',
      children: [],
      proccode: func.proccode,
      argumentids: JSON.stringify(argumentNames), // dummy since we don't know the IDs when calling function
      argumentnames: JSON.stringify(argumentNames),
      argumentdefaults: JSON.stringify(
        func.decl.parameters.map(p => (p.type.name === 'any' ? '' : 'false'))
      ),
      warp: func.decl.once ? 'true' : 'false'
    }
  }

  // 函数定义积木
  const definitionBlock: Sb3Block = {
    opcode: 'procedures_definition',
    next: null,
    parent: null,
    inputs: {
      custom_block: [1, prototypeId]
    },
    fields: {},
    shadow: false,
    topLevel: true,
    x: 0,
    y: 0
  }

  context.workspace[definitionId] = definitionBlock
  context.workspace[prototypeId] = prototypeBlock

  for (const [name, id] of Object.entries(argumentReporterIds)) {
    // create argument_reporter_boolean for boolean arguments, argument_reporter_string_number for others
    const param = func.decl.parameters.find(p => p.name.name === name)
    if (!param) continue

    const reporterBlock: Sb3Block = {
      opcode:
        param.type.name === 'bool'
          ? 'argument_reporter_boolean'
          : 'argument_reporter_string_number',
      next: null,
      parent: prototypeId,
      inputs: {},
      fields: {
        VALUE: [name, null]
      },
      shadow: true,
      topLevel: false
    }
    context.workspace[id] = reporterBlock

    // 将参数积木连接到函数原型积木的 inputs
    if (param.type.name === 'bool') {
      prototypeBlock.inputs[name] = [2, id]
    } else {
      prototypeBlock.inputs[name] = [1, id]
    }
  }

  // 如果函数有实现，序列化函数体
  if (func.impl && func.impl.length > 0) {
    const implIds: string[] = []

    // 序列化函数体积木
    for (const block of func.impl) {
      const id = uid()
      const sb3Block = serializeBlock(block, context, id)
      context.workspace[id] = sb3Block
      implIds.push(id)
    }

    // 设置函数体积木的链接关系
    for (let i = 0; i < implIds.length; i++) {
      const currentId = implIds[i]
      const nextId = i < implIds.length - 1 ? implIds[i + 1] : null
      const parentId = i > 0 ? implIds[i - 1] : null

      context.workspace[currentId].next = nextId
      context.workspace[currentId].parent = parentId
    }

    // 将函数定义积木连接到第一个实现积木
    if (implIds.length > 0) {
      const firstImplId = implIds[0]
      context.workspace[definitionId].next = firstImplId
      context.workspace[firstImplId].parent = definitionId
    }
  }

  return context.workspace
}

/**
 * 合并多个 Sb3Workspace
 */
export function mergeWorkspaces(...workspaces: Sb3Workspace[]): Sb3Workspace {
  return Object.assign({}, ...workspaces)
}

/**
 * 验证序列化后的 workspace 的完整性
 */
export function validateWorkspace(workspace: Sb3Workspace) {
  for (const [id, block] of Object.entries(workspace)) {
    // 检查 next 和 parent 引用的有效性
    if (block.next && !workspace[block.next]) {
      throw new Error(
        `Block ${id} references non-existent next block: ${block.next}`
      )
    }

    if (block.parent && !workspace[block.parent]) {
      throw new Error(
        `Block ${id} references non-existent parent block: ${block.parent}`
      )
    }

    // 检查 inputs 中引用的积木 ID
    for (const [inputKey, input] of Object.entries(block.inputs)) {
      if (
        Array.isArray(input) &&
        input.length >= 2 &&
        typeof input[1] === 'string'
      ) {
        const referencedId = input[1]
        if (!workspace[referencedId]) {
          throw new Error(
            `Block ${id} input ${inputKey} references non-existent block: ${referencedId}`
          )
        }
      }
    }
  }
}
