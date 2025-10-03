// TODO

const path = require('path')
const fs = require('fs')
const json5 = require('json5')
const jsonschema = require('jsonschema')

if (process.argv.length < 3) {
  console.error('Usage: node script-compiler.js <input-file> <output-file>')
  process.exit(1)
}

const inputPath = process.argv[2]
const outputPath = process.argv[3]

if (path.extname(inputPath) !== '.json5') {
  console.error('Input file must have .json5 extension')
  process.exit(1)
}

if (path.extname(outputPath) !== '.sb3') {
  console.error('Output file must have .sb3 extension')
  process.exit(1)
}

const inputDir = path.dirname(inputPath)
const inputJson = json5.parse(fs.readFileSync(inputPath, 'utf-8'))
const schema = {
  id: '/Sb3Project',
  type: 'object',
  properties: {
    types: {
      type: 'array',
      items: { type: 'string' }
    },
    stage: {
      type: 'object',
      properties: {
        currentBackdrop: { type: 'number' },
        tempo: { type: 'number' },
        volume: { type: 'number' },
        backdrops: {
          type: 'array',
          items: {
            type: 'object',
            properties: {
              path: { type: 'string' },
              name: { type: 'string' },
              x: { type: 'number' },
              y: { type: 'number' }
            }
          }
        },
        sounds: {
          type: 'array',
          items: {
            type: 'object',
            properties: {
              path: { type: 'string' },
              name: { type: 'string' }
            }
          }
        },
        entry: { type: 'string' }
      }
    },
    targets: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          name: { type: 'string' },
          currentCostume: { type: 'number' },
          rotationStyle: { type: 'string' },
          layerOrder: { type: 'number' },
          visible: { type: 'boolean' },
          x: { type: 'number' },
          y: { type: 'number' },
          size: { type: 'number' },
          direction: { type: 'number' },
          draggable: { type: 'boolean' },
          tempo: { type: 'number' },
          volume: { type: 'number' },
          costumes: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                path: { type: 'string' },
                name: { type: 'string' },
                x: { type: 'number' },
                y: { type: 'number' }
              }
            }
          },
          sounds: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                path: { type: 'string' },
                name: { type: 'string' }
              }
            }
          },
          entry: { type: 'string' }
        }
      }
    }
  }
}
const validator = new jsonschema.Validator()

const validationResult = validator.validate(inputJson, schema)

if (!validationResult.valid) {
  console.error('Input JSON does not conform to schema:')
  validationResult.errors.forEach(error => {
    console.error(`- ${error.stack}`)
  })
  process.exit(1)
}
