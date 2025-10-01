#!/usr/bin/env node

// Usage: node bin/script-compiler.js <input-file> <output-file>
// Example: node bin/script-compiler.js examples/example.fuse examples/example.json
// Converts a .fuse script file to a project.json file containing the serialized Scratch blocks

const fs = require('fs')
const path = require('path')

// Import compiled modules
const {
  Lexer,
  Parser,
  uid,
  Compiler,
  getProgramInfo,
  Scope,
  serializeScript,
  serializeFunction,
  mergeWorkspaces,
  Builtins,
  mergeNamespace
} = require('../dist/index.cjs')

/**
 * Create a default Scratch 3.0 project structure
 */
function createDefaultProject() {
  return {
    targets: [
      {
        isStage: true,
        name: 'Stage',
        variables: {},
        lists: {},
        broadcasts: {},
        blocks: {},
        comments: {},
        currentCostume: 0,
        costumes: [
          {
            name: 'Backdrop1',
            dataFormat: 'svg',
            assetId: 'cd21514d0531fdffb22204e0ec5ed84a',
            md5ext: 'cd21514d0531fdffb22204e0ec5ed84a.svg',
            rotationCenterX: 240,
            rotationCenterY: 180
          }
        ],
        sounds: [],
        volume: 100,
        layerOrder: 0,
        tempo: 60,
        videoTransparency: 50,
        videoState: 'on',
        textToSpeechLanguage: null
      },
      {
        isStage: false,
        name: 'Sprite1',
        variables: {},
        lists: {},
        broadcasts: {},
        blocks: {},
        comments: {},
        currentCostume: 0,
        costumes: [
          {
            name: 'Costume1',
            bitmapResolution: 1,
            dataFormat: 'svg',
            assetId: '927d672925e7b99f7813735c484c6922',
            md5ext: '927d672925e7b99f7813735c484c6922.svg',
            rotationCenterX: 30.74937882782359,
            rotationCenterY: 58.864768144346826
          }
        ],
        sounds: [],
        volume: 100,
        layerOrder: 1,
        visible: true,
        x: 0,
        y: 0,
        size: 100,
        direction: 90,
        draggable: false,
        rotationStyle: 'all around'
      }
    ],
    monitors: [],
    extensions: [],
    meta: {
      semver: '3.0.0',
      vm: '0.2.0',
      agent: '',
      platform: { name: 'TurboWarp', url: 'https://turbowarp.org/' }
    }
  }
}

/**
 *
 * @param {Map<string, Namespace>} base
 * @param {Map<string, Namespace>} upper
 */
function mergeNamespaces(base, upper) {
  const merged = new Map(base)
  for (const [name, namespace] of upper.entries()) {
    if (merged.has(name)) {
      const existing = merged.get(name)
      merged.set(name, mergeNamespace(existing, namespace))
    } else {
      merged.set(name, namespace)
    }
  }
  return merged
}

/**
 * Compile .fuse file to Scratch 3.0 project
 */
function compileScript(inputPath, outputPath) {
  try {
    console.log(`Compiling ${inputPath} to ${outputPath}...`)

    // Step 1: Read and lex the .fuse file
    const sourceCode = fs.readFileSync(inputPath, 'utf-8')
    const lexer = new Lexer(sourceCode)
    const tokens = lexer.all()

    // Step 2: Parse tokens to generate AST
    const parser = new Parser(tokens)
    const program = parser.parse()

    // Step 3: Get program info (variables with default values, namespaces)
    const programInfo = getProgramInfo(program)

    // Create default project structure
    const project = createDefaultProject()
    const stage = project.targets[0]
    const sprite = project.targets[1]

    // Step 4: Extract and setup variables
    for (const [, [variable, defaultValue]] of programInfo.variables) {
      const scratchId = uid()

      if (variable.isGlobal) {
        // Global variables go to Stage
        if (variable.type === 'list') {
          stage.lists[scratchId] = [
            variable.exportName ?? variable.name,
            Array.isArray(defaultValue) ? defaultValue : []
          ]
        } else {
          stage.variables[scratchId] = [
            variable.exportName ?? variable.name,
            typeof defaultValue === 'string' ||
            typeof defaultValue === 'number' ||
            typeof defaultValue === 'boolean'
              ? defaultValue
              : 0
          ]
        }
      } else {
        // Private variables go to first Sprite
        if (variable.type === 'list') {
          sprite.lists[scratchId] = [
            variable.exportName ?? variable.name,
            Array.isArray(defaultValue) ? defaultValue : []
          ]
        } else {
          sprite.variables[scratchId] = [
            variable.exportName ?? variable.name,
            typeof defaultValue === 'string' ||
            typeof defaultValue === 'number' ||
            typeof defaultValue === 'boolean'
              ? defaultValue
              : 0
          ]
        }
      }
    }

    // Step 5: Create scope instance (remove default value information)
    const variablesMap = new Map()
    for (const [varName, [variable]] of programInfo.variables) {
      variablesMap.set(varName, variable)
    }
    const globalScope = new Scope(variablesMap)

    // Step 6: Get functions
    const functions = Compiler.getFunctions(globalScope, program)

    // Step 7: Create compiler instance
    const compiler = new Compiler(
      globalScope,
      functions,
      mergeNamespaces(Builtins.Sb3Namespaces, programInfo.namespaces)
    )

    // Step 8: Compile all functions to IR scripts
    const functionWorkspaces = []
    for (const [name, func] of functions) {
      const compiledFunction = compiler.parse(func)
      const functionWorkspace = serializeFunction(compiledFunction)
      functionWorkspaces.push(functionWorkspace)
    }

    // Step 9: Compile script blocks to IR scripts
    const scripts = compiler.parse(program)
    const scriptWorkspaces = []
    for (const script of scripts) {
      const scriptWorkspace = serializeScript(script)
      scriptWorkspaces.push(scriptWorkspace)
    }

    // Step 10: Merge all workspaces and assign to first sprite
    const allWorkspaces = [...functionWorkspaces, ...scriptWorkspaces]
    let mergedWorkspace = {}

    if (allWorkspaces.length > 0) {
      mergedWorkspace = mergeWorkspaces(...allWorkspaces)
    }

    // Assign merged workspace to first sprite
    sprite.blocks = mergedWorkspace

    // Step 11: Write project.json to output file
    fs.writeFileSync(outputPath, JSON.stringify(project, null, 2))
    console.log(`Successfully compiled to ${outputPath}`)
  } catch (error) {
    console.error(`Compilation failed: ${error.message}`)
    if (error.stack) {
      console.error(error.stack)
    }
    process.exit(1)
  }
}

/**
 * Main entry point
 */
function main() {
  const args = process.argv.slice(2)

  if (args.length !== 2) {
    console.error(
      'Usage: node bin/script-compiler.js <input-file> <output-file>'
    )
    console.error(
      'Example: node bin/script-compiler.js examples/example.fuse examples/example.json'
    )
    process.exit(1)
  }

  const [inputPath, outputPath] = args

  // Validate input file exists
  if (!fs.existsSync(inputPath)) {
    console.error(`Error: Input file '${inputPath}' does not exist`)
    process.exit(1)
  }

  // Validate input file has .fuse extension
  if (!inputPath.endsWith('.fuse')) {
    console.error(`Error: Input file must have .fuse extension`)
    process.exit(1)
  }

  // Create output directory if it doesn't exist
  const outputDir = path.dirname(outputPath)
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true })
  }

  // Compile the script
  compileScript(inputPath, outputPath)
}

// Run if this is the main module
if (require.main === module) {
  main()
}
