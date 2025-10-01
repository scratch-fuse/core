export interface Sb3Block {
  opcode: string
  next: string | null // ID
  parent: string | null // ID
  inputs: { [key: string]: Sb3Input }
  fields: { [key: string]: Sb3Field }
  shadow: boolean
  topLevel: boolean
  x?: number
  y?: number
  mutation?: Sb3Mutation
}

export interface Sb3Mutation {
  [key: string]: string | Sb3Mutation[]
}

export type Sb3ShadowInput =
  | [4, string] // [type (shadow number), shadow value]
  | [10, string] // [type (shadow string), shadow value]
export type Sb3Input =
  | [3, string, Sb3ShadowInput] // [type (with reporter), block ID, shadow input]
  | [1, Sb3ShadowInput] // [type (literal), shadow input]
  | [2, string] // [type (boolean reporter without shadow, or substack), block ID]
  | [1, string] // [type (any reporter without shadow), block ID]
export type Sb3Field = [string, string | null] // [value, id]

export type Sb3Workspace = Record<string, Sb3Block>

export interface Sb3Target {
  isStage: boolean
  name: string
  variables: Record<
    string,
    [string, string | number | boolean | (string | number | boolean)[]]
  > // id -> [name, value]
  lists: Record<string, [string, (string | number | boolean)[]]> // id -> [name, value]
  broadcasts: Record<string, string> // id -> name
  blocks: Sb3Workspace
  comments: Record<string, unknown>
  currentCostume: number
  costumes: Array<{
    assetId: string
    name: string
    md5ext: string
    dataFormat: string
    rotationCenterX: number
    rotationCenterY: number
  }>
  sounds: Array<{
    assetId: string
    name: string
    md5ext: string
    dataFormat: string
    format: string
    rate: number
    sampleCount: number
  }>
  volume: number
  layerOrder: number
  tempo: number
  videoTransparency: number
  videoState: 'on' | 'off' | 'on-flipped'
  textToSpeechLanguage: string | null
}
